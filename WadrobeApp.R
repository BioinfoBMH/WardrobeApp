# app.R  --- Wardrobe Inventory Shiny App
# Run with: shiny::runApp()

library(shiny)
library(DBI)
library(RSQLite)
library(dplyr)
library(DT)
library(lubridate)
library(stringr)
library(tools)

# -------------------------
# CONFIG
# -------------------------
db_path <- "wardrobe.sqlite"

# make sure www/photos exists for images
if (!dir.exists("www")) dir.create("www")
if (!dir.exists("www/photos")) dir.create("www/photos")

# -------------------------
# DB HELPERS
# -------------------------
init_db <- function() {
  conn <- dbConnect(SQLite(), db_path)
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS wardrobe (
      ID           INTEGER PRIMARY KEY AUTOINCREMENT,
      ItemKey      TEXT UNIQUE,
      Person       TEXT,
      Location     TEXT,
      Category     TEXT,
      Type         TEXT,
      PhotoPath    TEXT,
      Season       TEXT,
      Color        TEXT,
      Size         TEXT,
      Brand        TEXT,
      Condition    TEXT,
      Quantity     INTEGER,
      PurchaseDate TEXT,
      Price        REAL,
      LastWorn     TEXT,
      Notes        TEXT
    )
  ")
  dbDisconnect(conn)
}

load_wardrobe <- function() {
  conn <- dbConnect(SQLite(), db_path)
  df <- dbReadTable(conn, "wardrobe")
  dbDisconnect(conn)
  df
}

save_item <- function(record, id = NULL) {
  conn <- dbConnect(SQLite(), db_path)
  
  if (is.null(id)) {
    # INSERT
    dbExecute(conn,
              "INSERT OR REPLACE INTO wardrobe
       (ItemKey, Person, Location, Category, Type, PhotoPath, Season, Color, Size,
        Brand, Condition, Quantity, PurchaseDate, Price, LastWorn, Notes)
       VALUES (:ItemKey, :Person, :Location, :Category, :Type, :PhotoPath, :Season,
               :Color, :Size, :Brand, :Condition, :Quantity, :PurchaseDate,
               :Price, :LastWorn, :Notes)",
              params = record
    )
  } else {
    # UPDATE
    record$ID <- id
    dbExecute(conn,
              "UPDATE wardrobe SET
          ItemKey      = :ItemKey,
          Person       = :Person,
          Location     = :Location,
          Category     = :Category,
          Type         = :Type,
          PhotoPath    = :PhotoPath,
          Season       = :Season,
          Color        = :Color,
          Size         = :Size,
          Brand        = :Brand,
          Condition    = :Condition,
          Quantity     = :Quantity,
          PurchaseDate = :PurchaseDate,
          Price        = :Price,
          LastWorn     = :LastWorn,
          Notes        = :Notes
       WHERE ID = :ID",
              params = record
    )
  }
  
  dbDisconnect(conn)
}

delete_item <- function(id) {
  conn <- dbConnect(SQLite(), db_path)
  dbExecute(conn, "DELETE FROM wardrobe WHERE ID = ?", params = list(id))
  dbDisconnect(conn)
}

# initialize DB once
init_db()

# -------------------------
# UI
# -------------------------
ui <- fluidPage(
  tags$head(
    tags$title("BioinfoBMH Wardrobe Tracker"),
    tags$style(HTML("
      body {
        background-color: #f5fafc;
        font-family: 'Segoe UI', sans-serif;
      }
      h2, h3, label {
        color: #003845;
      }
      .bmh-panel {
        background: #ffffff;
        border-radius: 12px;
        padding: 16px 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.06);
        margin-bottom: 16px;
      }
      .btn-primary {
        background-color: #006d77;
        border-color: #006d77;
      }
      .btn-primary:hover {
        background-color: #005662;
        border-color: #005662;
      }
      .btn-danger {
        background-color: #d62828;
        border-color: #d62828;
      }
      .dataTables_wrapper .paginate_button.current,
      .dataTables_wrapper .paginate_button.current:hover {
        background: #006d77 !important;
        border-color: #006d77 !important;
        color: #ffffff !important;
      }
      table.dataTable thead {
        background-color: #00a5cf;
        color: #ffffff;
      }
      .thumb-img {
        max-width: 60px;
        max-height: 60px;
        border-radius: 4px;
        cursor: pointer;
      }
    ")),
    # JS: when thumbnail clicked, send path back to Shiny
    tags$script(HTML("
      $(document).on('click', 'img.thumb-img', function() {
        var path = $(this).data('path');
        Shiny.setInputValue('thumb_clicked', path, {priority: 'event'});
      });
    "))
  ),
  
  titlePanel("Wardrobe Inventory – BioinfoBMH Style"),
  
  fluidRow(
    column(
      width = 4,
      div(
        class = "bmh-panel",
        h3("Item Details"),
        textInput("item_key", "Item Key (e.g., TSE0001)", ""),
        textInput("person", "Person / Owner", ""),
        textInput("location", "Location (Closet / Room)", ""),
        textInput("category", "Category (e.g., Coat, Dress, Shoes)", ""),
        textInput("type", "Type (e.g., Trench, Hoodie)", ""),
        textInput("color", "Color", ""),
        textInput("size", "Size", ""),
        textInput("brand", "Brand", ""),
        textInput("condition", "Condition", "New / Gently Used / Worn"),
        numericInput("quantity", "Quantity", value = 1, min = 1, step = 1),
        dateInput("purchase_date", "Purchase Date", value = NA),
        numericInput("price", "Price", value = NA, min = 0, step = 1),
        dateInput("last_worn", "Last Worn Date", value = NA),
        textAreaInput("notes", "Notes", rows = 3),
        
        fileInput("photo_file", "Upload Photo", accept = c("image/png", "image/jpeg")),
        helpText("Thumbnails will show in the table. Click a thumbnail to enlarge."),
        
        actionButton("save_item", "Add / Update Item", class = "btn-primary"),
        actionButton("reset_form", "Clear Form"),
        br(), br(),
        actionButton("delete_item", "Delete Selected Item", class = "btn-danger")
      )
    ),
    
    column(
      width = 8,
      div(
        class = "bmh-panel",
        h3("Filters & Search"),
        fluidRow(
          column(4, selectInput("filter_person", "Person", choices = c("All"), selected = "All")),
          column(4, selectInput("filter_category", "Category", choices = c("All"), selected = "All")),
          column(4, selectInput("filter_season", "Season", choices = c("All"), selected = "All"))
        ),
        fluidRow(
          column(4, textInput("filter_text", "Text Search (brand, notes, etc.)", "")),
          column(4, numericInput("filter_last_worn", "Last worn > X months (declutter)", value = 0, min = 0, step = 1)),
          column(4, checkboxInput("show_only_declutter", "Show ONLY items older than X months", FALSE))
        )
      ),
      
      div(
        class = "bmh-panel",
        h3("Wardrobe Table"),
        DTOutput("wardrobe_table")
      )
    )
  )
)

# -------------------------
# SERVER
# -------------------------
server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 10 * 1024 ^ 2)  # allow larger photos
  
  # reactive values
  rv <- reactiveValues(
    data = load_wardrobe(),
    selected_id = NULL
  )
  
  # ---- helper: refresh filters ----
  refresh_filters <- function(df) {
    updateSelectInput(session, "filter_person",
                      choices = c("All", sort(unique(df$Person))),
                      selected = input$filter_person %||% "All")
    updateSelectInput(session, "filter_category",
                      choices = c("All", sort(unique(df$Category))),
                      selected = input$filter_category %||% "All")
    updateSelectInput(session, "filter_season",
                      choices = c("All", sort(unique(df$Season))),
                      selected = input$filter_season %||% "All")
  }
  
  observe({
    refresh_filters(rv$data)
  })
  
  # ---- filtered data ----
  filtered_data <- reactive({
    df <- rv$data
    
    if (nrow(df) == 0) return(df)
    
    # filters
    if (!is.null(input$filter_person) && input$filter_person != "All") {
      df <- df %>% filter(Person == input$filter_person)
    }
    if (!is.null(input$filter_category) && input$filter_category != "All") {
      df <- df %>% filter(Category == input$filter_category)
    }
    if (!is.null(input$filter_season) && input$filter_season != "All") {
      df <- df %>% filter(Season == input$filter_season)
    }
    
    # free text search
    if (!is.null(input$filter_text) && input$filter_text != "") {
      txt <- tolower(input$filter_text)
      df <- df %>%
        filter(
          str_detect(tolower(Brand %||% ""), txt) |
            str_detect(tolower(Notes %||% ""), txt) |
            str_detect(tolower(Type %||% ""), txt) |
            str_detect(tolower(Category %||% ""), txt)
        )
    }
    
    # declutter filter: last worn > X months
    months_val <- input$filter_last_worn %||% 0
    if (!is.null(months_val) && months_val > 0) {
      cutoff_date <- Sys.Date() %m-% months(months_val)
      df$LastWornDate <- as.Date(df$LastWorn)
      old_items <- !is.na(df$LastWornDate) & df$LastWornDate <= cutoff_date
      
      if (isTRUE(input$show_only_declutter)) {
        df <- df[old_items, , drop = FALSE]
      } else {
        # just tag them (we'll color in the table)
        df$NeedsDeclutter <- old_items
      }
    } else {
      df$NeedsDeclutter <- FALSE
    }
    
    df
  })
  
  # ---- render table ----
  output$wardrobe_table <- renderDT({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No items yet. Add something on the left!")))
    }
    
    # create thumbnail HTML
    df$Photo <- ifelse(
      is.na(df$PhotoPath) | df$PhotoPath == "",
      "",
      sprintf('<img src="%s" class="thumb-img" data-path="%s">', df$PhotoPath, df$PhotoPath)
    )
    
    # order columns: ID hidden, then visible fields with Photo between Type & Season
    df_display <- df %>%
      select(
        ID,
        ItemKey, Person, Location, Category, Type, Photo, Season, Color,
        Size, Brand, Condition, Quantity, PurchaseDate, Price, LastWorn, Notes, PhotoPath
      )
    
    datatable(
      df_display,
      escape = FALSE,
      selection = "single",
      rownames = FALSE,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        columnDefs = list(
          list(visible = FALSE, targets = c(0, ncol(df_display) - 1)), # hide ID and PhotoPath
          list(className = 'dt-center', targets = "_all")
        )
      )
    )
  })
  
  # ---- handle row selection: populate form ----
  observeEvent(input$wardrobe_table_rows_selected, {
    idx <- input$wardrobe_table_rows_selected
    df <- filtered_data()
    if (!length(idx) || nrow(df) == 0) return()
    
    row <- df[idx, ]
    
    rv$selected_id <- row$ID
    
    updateTextInput(session, "item_key", value = row$ItemKey %||% "")
    updateTextInput(session, "person", value = row$Person %||% "")
    updateTextInput(session, "location", value = row$Location %||% "")
    updateTextInput(session, "category", value = row$Category %||% "")
    updateTextInput(session, "type", value = row$Type %||% "")
    updateTextInput(session, "color", value = row$Color %||% "")
    updateTextInput(session, "size", value = row$Size %||% "")
    updateTextInput(session, "brand", value = row$Brand %||% "")
    updateTextInput(session, "condition", value = row$Condition %||% "")
    updateNumericInput(session, "quantity", value = row$Quantity %||% 1)
    updateDateInput(session, "purchase_date",
                    value = if (!is.na(row$PurchaseDate) && row$PurchaseDate != "") as.Date(row$PurchaseDate) else NA)
    updateNumericInput(session, "price", value = row$Price %||% NA)
    updateDateInput(session, "last_worn",
                    value = if (!is.na(row$LastWorn) && row$LastWorn != "") as.Date(row$LastWorn) else NA)
    updateTextAreaInput(session, "notes", value = row$Notes %||% "")
  })
  
  # ---- clear form ----
  observeEvent(input$reset_form, {
    rv$selected_id <- NULL
    updateTextInput(session, "item_key", value = "")
    updateTextInput(session, "person", value = "")
    updateTextInput(session, "location", value = "")
    updateTextInput(session, "category", value = "")
    updateTextInput(session, "type", value = "")
    updateTextInput(session, "color", value = "")
    updateTextInput(session, "size", value = "")
    updateTextInput(session, "brand", value = "")
    updateTextInput(session, "condition", value = "New / Gently Used / Worn")
    updateNumericInput(session, "quantity", value = 1)
    updateDateInput(session, "purchase_date", value = NA)
    updateNumericInput(session, "price", value = NA)
    updateDateInput(session, "last_worn", value = NA)
    updateTextAreaInput(session, "notes", value = "")
  })
  
  # ---- add / update item ----
  observeEvent(input$save_item, {
    req(input$item_key)
    
    # handle photo upload
    photo_path <- NULL
    if (!is.null(input$photo_file)) {
      ext <- file_ext(input$photo_file$name)
      new_name <- paste0("photo_", format(Sys.time(), "%Y%m%d%H%M%S"), ".", ext)
      dest <- file.path("www/photos", new_name)
      file.copy(input$photo_file$datapath, dest, overwrite = TRUE)
      photo_path <- file.path("photos", new_name)  # relative for Shiny
    } else if (!is.null(rv$selected_id)) {
      # keep existing photopath if editing and no new upload
      existing <- rv$data %>% filter(ID == rv$selected_id)
      if (nrow(existing) == 1) {
        photo_path <- existing$PhotoPath
      }
    }
    
    rec <- list(
      ItemKey      = input$item_key,
      Person       = input$person,
      Location     = input$location,
      Category     = input$category,
      Type         = input$type,
      PhotoPath    = photo_path,
      Season       = "",  # you can swap to a selectInput later if you want
      Color        = input$color,
      Size         = input$size,
      Brand        = input$brand,
      Condition    = input$condition,
      Quantity     = ifelse(is.na(input$quantity), 1L, as.integer(input$quantity)),
      PurchaseDate = if (!is.na(input$purchase_date)) as.character(input$purchase_date) else "",
      Price        = ifelse(is.na(input$price), NA, input$price),
      LastWorn     = if (!is.na(input$last_worn)) as.character(input$last_worn) else "",
      Notes        = input$notes
    )
    
    save_item(rec, id = rv$selected_id)
    
    rv$data <- load_wardrobe()
    refresh_filters(rv$data)
    
    showNotification("Item saved.", type = "message")
  })
  
  # ---- delete item ----
  observeEvent(input$delete_item, {
    req(rv$selected_id)
    delete_item(rv$selected_id)
    rv$data <- load_wardrobe()
    rv$selected_id <- NULL
    refresh_filters(rv$data)
    showNotification("Item deleted.", type = "warning")
  })
  
  # ---- big photo preview when thumbnail clicked ----
  observeEvent(input$thumb_clicked, {
    req(input$thumb_clicked)
    showModal(
      modalDialog(
        title = "Photo Preview",
        size = "l",
        easyClose = TRUE,
        footer = NULL,
        tags$img(src = input$thumb_clicked, style = "width: 100%; border-radius: 12px;")
      )
    )
  })
}

shinyApp(ui, server)
