# 👗 Wardrobe Inventory Shiny App  
*A BioinfoBMH-inspired by busy families*

This Shiny application is a modern, photo-enabled wardrobe tracking system built with my **BioinfoBMH color theme**. It helps you organize clothing across people, categories, seasons, and storage locations — with built-in photos, filters, decluttering tools, and automatic database storage. As a scientist who loves fashion, this was a fun activity to build. This wardrobe app is for my fashion lovers as well as for people who have a lot of clothing to track. This is also a perfect way to keep up to date with clothing for your family so that you can focus only on purchasing needed items.

---

## 🌟 Features

### 🧥 Full Wardrobe Management  
Track and edit details for every clothing item:

- Item Key (unique ID)
- Person / Owner
- Location (Closet, Room, Storage)
- Category (Coat, Dress, Shoes, etc.)
- Type (Jacket, Hoodie, Sweater, etc.)
- Color, Size, Brand
- Condition
- Quantity
- Purchase Date
- Price
- Last Worn Date
- Notes

---

### 🖼️ Photo Upload + Thumbnail Preview  
- Upload photos directly from your device  
- Images are stored in `www/photos/`  
- Thumbnails appear **between Type and Season** in the table  
- **Click any thumbnail → opens a large modal preview**  
- Selecting a table row also shows a large preview section

---

### 🔍 Intelligent Search + Filters  
Filter items by:

- Person  
- Category  
- Season  
- Free-text search (brand, notes, color, type)  

---

### 🧹 Decluttering Mode  
Use the built-in decluttering tool:

- Enter “Last worn > X months”  
- Optionally show **only** items older than X months  
- Helps identify clothes to donate or recycle

---

### 🗃️ SQLite Database (No Excel Required)
The app automatically:

- Creates `wardrobe.sqlite` on first run  
- Creates folders:  
