 ### Savannah Thesis Topographic Survey Plots ###

# Load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggforce)
library(patchwork)
library(gtable)




####STANDARD PLOTS####

#set working directory #
setwd("C:/Users/savan/OneDrive/Documents/Code/GitHub/DepositionSFB/data/Topo Surveys")

# Set output folder
output_dir <- "transect_plots"
dir.create(output_dir, showWarnings = FALSE)

# Define the plot function
plot_transects <- function(file, location_name) {
  df <- read_csv(file)
  
  # Loop through transects
  transects <- unique(df$Transect_ID)
  for (transect in transects) {
    df_transect <- df %>% filter(Transect_ID == transect)
    
    # Find the "pin" point, case-insensitive match in Point_ID
    pin_point <- df_transect %>% filter(str_detect(tolower(Point_ID), "pin"))
    has_pin <- nrow(pin_point) > 0
    
    # Plot (no dots)
    y_min <- min(df_transect$Elevation_m, na.rm = TRUE)
    y_max <- max(df_transect$Elevation_m, na.rm = TRUE)
    
    p <- ggplot(df_transect, aes(x = Distance_AlongTransect_m, y = Elevation_m)) +
      geom_line(color = "blue", linewidth = 1) +
      geom_point(color = "black", size = 1.5) +  
      geom_point(data = pin_point, color = "red", size = 1.5) +
      {if (has_pin) geom_text(data = pin_point, aes(label = "Pin"), vjust = -1, color = "red", size = 4)} +
      scale_y_continuous(limits = c(y_min - 0.1, y_max + 0.2)) +
      labs(
        title = paste(location_name, "- Transect", transect),
        x = "Distance along transect (m)",
        y = "Elevation (m NAVD88)"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5))
    
    
    # Save the plot
    ggsave(
      filename = file.path(output_dir, paste0(location_name, "_", transect, ".png")),
      plot = p, width = 12, height = 5,  # ⬅️ wider plot
      bg = "white"
    )
    
    message("✔ Saved: ", location_name, " - Transect ", transect)
  }
}

# Define file paths
file_paths <- list(
  "SonomaBaylands" = "SonomaBaylands_06032025_LinRef.csv",
  "GiantMarsh" = "GiantMarsh_06022025_LinRef.csv",
  "GallinasCreek" = "GallinasCreek_06172025_LinRef.csv",
  "CorteMadera" = "CorteMadera_06162025_LinRef.csv"
)

# Run it
for (loc in names(file_paths)) {
  plot_transects(file_paths[[loc]], loc)
}




#### FACETED PLOTS #### 
# for all transects at a location, faceted together
plot_faceted_location <- function(file, location_name) {
  df <- read_csv(file)
  
  y_min <- min(df$Elevation_m, na.rm = TRUE)
  y_max <- max(df$Elevation_m, na.rm = TRUE)
  
# Find the "pin" point, case-insensitive match in Point_ID
pin_point <- df %>% filter(str_detect(tolower(Point_ID), "pin"))
has_pin <- nrow(pin_point) > 0
    
  p <- ggplot(df, aes(x = Distance_AlongTransect_m, y = Elevation_m)) +
    geom_line(color = "blue", linewidth = 1) +
    geom_point(color = "black", size = 1.5) +
    geom_point(data = pin_point, color = "red", size = 1.5) +
    {if (has_pin) geom_text(data = pin_point, aes(label = "Pin"), vjust = -1, color = "red", size = 4)} +
    facet_wrap(~ Transect_ID, scales = "free_x") +
    scale_y_continuous(limits = c(y_min - 0.1, y_max + 0.2)) +
    labs(
      title = paste(location_name, "- All Transects"),
      x = "Distance along transect (m)",
      y = "Elevation (m NAVD88)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5),
      strip.text = element_text(face = "bold")
    )
  
  ggsave(
    filename = file.path(output_dir, paste0(location_name, "_ALL_TRANSECTS.png")),
    plot = p, width = 14, height = 6,
    bg = "white"
  )
  
  message("✔ Saved faceted plot for: ", location_name)
}


run_faceted_plots <- TRUE  # Set to FALSE if you want to skip them

if (run_faceted_plots) {
  for (loc in names(file_paths)) {
    plot_faceted_location(file_paths[[loc]], loc)
  }
}


#### OVERLAID PLOTS ####

plot_overlay_location <- function(file, location_name) {
  df <- read_csv(file)
  
  y_min <- min(df$Elevation_m, na.rm = TRUE)
  y_max <- max(df$Elevation_m, na.rm = TRUE)
  
  # Find the "pin" point, case-insensitive match in Point_ID
  pin_point <- df %>% filter(str_detect(tolower(Point_ID), "pin"))
  has_pin <- nrow(pin_point) > 0
  
  p <- ggplot(df, aes(x = Distance_AlongTransect_m, y = Elevation_m, color = Transect_ID)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.5) +
    
    # Add PIN point with black outline and correct fill
    if (has_pin) {
      geom_point(
        data = pin_point,
        aes(x = Distance_AlongTransect_m, y = Elevation_m, fill = Transect_ID, shape = "PIN"),
        color = "black",
        size = 1.5,
        stroke = 0.8,
        inherit.aes = FALSE
      )
    } else {
      NULL
    } +
    
    # Match fill to transect color
    scale_fill_manual(
      name = "Transect",
      values = scale_color_hue()$palette(length(unique(df$Transect_ID)))
    ) +
    
    # Add PIN shape to legend
    scale_shape_manual(name = NULL, values = c("PIN" = 21)) +
    
    # Combine legends
    guides(
      color = guide_legend(order = 1, override.aes = list(shape = 16)),
      shape = guide_legend(order = 2, override.aes = list(fill = "white", color = "black", shape = 21, size = 3, stroke = 0.8)),
      fill = "none"  # suppress duplicate fill legend
    ) +
    
    scale_y_continuous(limits = c(y_min - 0.1, y_max + 0.2)) +
    labs(
      title = paste(location_name, "- All Transects (Overlayed)"),
      x = "Distance along transect (m)",
      y = "Elevation (m NAVD88)",
      color = "Transect"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.title = element_text(face = "bold"),
      legend.position = "right"
    )
  
  # Save the plot using your existing output_dir
  ggsave(
    filename = file.path(output_dir, paste0(location_name, "_OVERLAY_TRANSECTS.png")),
    plot = p, width = 12, height = 6,
    bg = "white"
  )
  
  message("✔ Saved overlay plot for: ", location_name)
}



run_overlay_plots <- TRUE  # set to FALSE to skip

if (run_overlay_plots) {
  for (loc in names(file_paths)) {
    plot_overlay_location(file_paths[[loc]], loc)
  }
}




####50 M PLOTS####
# Set output folder
output_dir <- "transect_plots_50"
dir.create(output_dir, showWarnings = FALSE)

# Define the plot function
plot_transects <- function(file, location_name) {
  df <- read_csv(file) %>% filter(Distance_AlongTransect_m <= 50)
  
  # Loop through transects
  transects <- unique(df$Transect_ID)
  for (transect in transects) {
    df_transect <- df %>% filter(Transect_ID == transect)
 
    # Find the "pin" point, case-insensitive match in Point_ID
    pin_point <- df_transect %>% filter(str_detect(tolower(Point_ID), "pin"))
    has_pin <- nrow(pin_point) > 0
    
    # Plot (with dots)
    y_min <- min(df_transect$Elevation_m, na.rm = TRUE)
    y_max <- max(df_transect$Elevation_m, na.rm = TRUE)
    
    p <- ggplot(df_transect, aes(x = Distance_AlongTransect_m, y = Elevation_m)) +
      geom_line(color = "blue", linewidth = 1) +
      geom_point(color = "black", size = 1.5) +
      geom_point(data = pin_point, color = "red", size = 1.5) +
      {if (has_pin) geom_text(data = pin_point, aes(label = "Pin"), vjust = -1, color = "red", size = 4)} +
      scale_y_continuous(limits = c(y_min - 0.1, y_max + 0.2)) +
      labs(
        title = paste(location_name, "- Transect", transect),
        x = "Distance along transect (m)",
        y = "Elevation (m NAVD88)"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggsave(
      filename = file.path(output_dir, paste0(location_name, "_", transect, ".png")),
      plot = p, width = 12, height = 5,
      bg = "white"
    )
    
    message("✔ Saved: ", location_name, " - Transect ", transect)
  }
}

# Define file paths
file_paths <- list(
  "SonomaBaylands" = "SonomaBaylands_06032025_LinRef.csv",
  "GiantMarsh" = "GiantMarsh_06022025_LinRef.csv",
  "GallinasCreek" = "GallinasCreek_06172025_LinRef.csv",
  "CorteMadera" = "CorteMadera_06162025_LinRef.csv"
)

# Run it
for (loc in names(file_paths)) {
  plot_transects(file_paths[[loc]], loc)
}

# Faceted plot for all transects at a location
plot_faceted_location <- function(file, location_name) {
  df <- read_csv(file) %>% filter(Distance_AlongTransect_m <= 50)
  
  y_min <- min(df$Elevation_m, na.rm = TRUE)
  y_max <- max(df$Elevation_m, na.rm = TRUE)
  
  # Find the "pin" point, case-insensitive match in Point_ID
  pin_point <- df %>% filter(str_detect(tolower(Point_ID), "pin"))
  has_pin <- nrow(pin_point) > 0
  
  p <- ggplot(df, aes(x = Distance_AlongTransect_m, y = Elevation_m)) +
    geom_line(color = "blue", linewidth = 1) +
    geom_point(color = "black", size = 1.5) +
    geom_point(data = pin_point, color = "red", size = 1.5) +
    {if (has_pin) geom_text(data = pin_point, aes(label = "Pin"), vjust = -1, color = "red", size = 4)} +
    facet_wrap(~ Transect_ID, scales = "free_x") +
    scale_y_continuous(limits = c(y_min - 0.1, y_max + 0.2)) +
    labs(
      title = paste(location_name, "- All Transects"),
      x = "Distance along transect (m)",
      y = "Elevation (m NAVD88)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5),
      strip.text = element_text(face = "bold")
    )
  
  ggsave(
    filename = file.path(output_dir, paste0(location_name, "_ALL_TRANSECTS.png")),
    plot = p, width = 14, height = 6,
    bg = "white"
  )
  
  message("✔ Saved faceted plot for: ", location_name)
}

# ---- Faceted plots ----
run_faceted_plots <- TRUE

if (run_faceted_plots) {
  for (loc in names(file_paths)) {
    plot_faceted_location(file_paths[[loc]], loc)
  }
}

####OVERLAID PLOTS####

plot_overlay_location <- function(file, location_name) {
  df <- read_csv(file) %>% filter(Distance_AlongTransect_m <= 50)
  
  y_min <- min(df$Elevation_m, na.rm = TRUE)
  y_max <- max(df$Elevation_m, na.rm = TRUE)

  # Find the "pin" point, case-insensitive match in Point_ID
  pin_point <- df %>% filter(str_detect(tolower(Point_ID), "pin"))
  has_pin <- nrow(pin_point) > 0
  
  p <- ggplot(df, aes(x = Distance_AlongTransect_m, y = Elevation_m, color = Transect_ID)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.5) +
    
    # Add PIN point with black outline and correct fill
    if (has_pin) {
      geom_point(
        data = pin_point,
        aes(x = Distance_AlongTransect_m, y = Elevation_m, fill = Transect_ID, shape = "PIN"),
        color = "black",
        size = 1.5,
        stroke = 0.8,
        inherit.aes = FALSE
      )
    } else {
      NULL
    } +
    
    # Match fill to transect color
    scale_fill_manual(
      name = "Transect",
      values = scale_color_hue()$palette(length(unique(df$Transect_ID)))
    ) +
    
    # Add PIN shape to legend
    scale_shape_manual(name = NULL, values = c("PIN" = 21)) +
    
    # Combine legends
    guides(
      color = guide_legend(order = 1, override.aes = list(shape = 16)),
      shape = guide_legend(order = 2, override.aes = list(fill = "white", color = "black", shape = 21, size = 3, stroke = 0.8)),
      fill = "none"  # suppress duplicate fill legend
    ) +
    
    scale_y_continuous(limits = c(y_min - 0.1, y_max + 0.2)) +
    labs(
      title = paste(location_name, "- All Transects (Overlayed)"),
      x = "Distance along transect (m)",
      y = "Elevation (m NAVD88)",
      color = "Transect"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.title = element_text(face = "bold"),
      legend.position = "right"
    )

  ggsave(
    filename = file.path(output_dir, paste0(location_name, "_OVERLAY_TRANSECTS.png")),
    plot = p, width = 14, height = 6,
    bg = "white"
  )
  
  message("✔ Saved overlay plot for: ", location_name)
}  

# ---- Overlay plots ----
run_overlay_plots <- TRUE

if (run_overlay_plots) {
  for (loc in names(file_paths)) {
    plot_overlay_location(file_paths[[loc]], loc)
  }
}




####FIRST 5 METERS####

# Set output directory
output_dir <- "transect_plots_50m_split_labeledpin"
dir.create(output_dir, showWarnings = FALSE)

# Define the plotting function
plot_transects <- function(file, location_name) {
  df <- read_csv(file) %>%
    filter(Distance_AlongTransect_m <= 50)
  
  transects <- unique(df$Transect_ID)
  
  for (transect in transects) {
    df_transect <- df %>% filter(Transect_ID == transect)
 
    # Find the "pin" point, case-insensitive match in Point_ID
    pin_point <- df_transect %>% filter(str_detect(tolower(Point_ID), "pin"))
    has_pin <- nrow(pin_point) > 0
    
    if (nrow(df_transect) == 0) {
      message("Skipping transect ", transect, ": no data")
      next
    }
    
    y_min <- min(df_transect$Elevation_m, na.rm = TRUE)
    y_max <- max(df_transect$Elevation_m, na.rm = TRUE)
    
    # Zoomed-in panel: 0–5 m
    p_zoom <- ggplot(df_transect, aes(x = Distance_AlongTransect_m, y = Elevation_m)) +
      geom_line(color = "blue", linewidth = 1) +
      geom_point(color = "black", size = 1.5) +
      # Inside your p_full or p_zoom plot
      geom_point(data = pin_point, color = "red", size = 1.5) +
      {if (has_pin) geom_text(data = pin_point, aes(label = "Pin"), vjust = -1, color = "red", size = 4)} +
      scale_x_continuous(breaks = seq(0, 5, 1), limits = c(0, 5)) +
      scale_y_continuous(limits = c(y_min - 0.1, y_max + 0.2)) +
      labs(
        title = NULL,
        x = "0–5 meters",
        y = "Elevation (m NAVD88)"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_blank())
    
    # Full transect panel: 0–50 m
    p_full <- ggplot(df_transect, aes(x = Distance_AlongTransect_m, y = Elevation_m)) +
      geom_line(color = "blue", linewidth = 1) +
      geom_point(color = "black", size = 1.5) +
      # Inside your p_full or p_zoom plot
      geom_point(data = pin_point, color = "red", size = 1.5)+
      {if (has_pin) geom_text(data = pin_point, aes(label = "Pin"), vjust = -1, color = "red", size = 4)}+
      scale_x_continuous(breaks = seq(0, 50, 5), limits = c(0, 50)) +
      scale_y_continuous(limits = c(y_min - 0.1, y_max + 0.2)) +
      labs(
        title = paste(location_name, "- Transect", transect),
        x = "0–50 meters",
        y = NULL
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Combine plots side by side
    p_combined <- p_zoom + p_full + plot_layout(widths = c(1.5, 2))
    
    # Save to file
    ggsave(
      filename = file.path(output_dir, paste0(location_name, "_", transect, "_split_zoom.png")),
      plot = p_combined,
      width = 14, height = 5,
      bg = "white"
    )
    
    message("✔ Saved: ", location_name, " - Transect ", transect)
  }
}

# Define your file paths
file_paths <- list(
  "SonomaBaylands" = "SonomaBaylands_06032025_LinRef.csv",
  "GiantMarsh" = "GiantMarsh_06022025_LinRef.csv",
  "GallinasCreek" = "GallinasCreek_06172025_LinRef.csv",
  "CorteMadera" = "CorteMadera_06162025_LinRef.csv"
)

# Run the function on all locations
for (loc in names(file_paths)) {
  plot_transects(file_paths[[loc]], loc)
}



#### 5 METERS AROUND PIN ####

# Set output directory
output_dir <- "transect_plots_50m_split_5mAROUNDpin"
dir.create(output_dir, showWarnings = FALSE)

# Define the plotting function
plot_transects <- function(file, location_name) {
  df <- read_csv(file) %>%
    filter(Distance_AlongTransect_m <= 50)
  
  transects <- unique(df$Transect_ID)
  
  for (transect in transects) {
    df_transect <- df %>% filter(Transect_ID == transect)
    
    # Find the "pin" point, case-insensitive match in Point_ID
    pin_point <- df_transect %>% filter(str_detect(tolower(Point_ID), "pin"))
    has_pin <- nrow(pin_point) > 0
    pin_x <- if (has_pin) pin_point$Distance_AlongTransect_m[1] else 5
    zoom_min <- max(0, pin_x - 5)
    zoom_max <- min(50, pin_x + 5)
    
    if (nrow(df_transect) == 0) {
      message("Skipping transect ", transect, ": no data")
      next
    }
    
    y_min <- min(df_transect$Elevation_m, na.rm = TRUE)
    y_max <- max(df_transect$Elevation_m, na.rm = TRUE)
    
    # Zoomed-in panel: 0–5 m
    p_zoom <- ggplot(df_transect, aes(x = Distance_AlongTransect_m, y = Elevation_m)) +
      geom_line(color = "blue", linewidth = 1) +
      geom_point(color = "black", size = 1.5) +
      # Inside your p_full or p_zoom plot
      geom_point(data = pin_point, color = "red", size = 1.5) +
      {if (has_pin) geom_text(data = pin_point, aes(label = "Pin"), vjust = -1, color = "red", size = 4)} +
      scale_x_continuous(
        limits = c(zoom_min, zoom_max),
        breaks = scales::pretty_breaks(n = 5)  # automatically picks ~5 nice ticks
      )+
      scale_y_continuous(limits = c(y_min - 0.1, y_max + 0.2)) +
      labs(
        title = NULL,
        x = "Zooomed In Around Pin",
        y = "Elevation (m NAVD88)"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_blank())
    
    # Full transect panel: 0–50 m
    p_full <- ggplot(df_transect, aes(x = Distance_AlongTransect_m, y = Elevation_m)) +
      geom_line(color = "blue", linewidth = 1) +
      geom_point(color = "black", size = 1.5) +
      # Inside your p_full or p_zoom plot
      geom_point(data = pin_point, color = "red", size = 1.5)+
      {if (has_pin) geom_text(data = pin_point, aes(label = "Pin"), vjust = -1, color = "red", size = 4)}+
      scale_x_continuous(breaks = seq(0, 50, 5), limits = c(0, 50)) +
      scale_y_continuous(limits = c(y_min - 0.1, y_max + 0.2)) +
      labs(
        title = paste(location_name, "- Transect", transect),
        x = "0–50 meters",
        y = NULL
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Combine plots side by side
    p_combined <- p_zoom + p_full + plot_layout(widths = c(1.5, 2))
    
    # Save to file
    ggsave(
      filename = file.path(output_dir, paste0(location_name, "_", transect, "_split_zoom_AROUNDpin.png")),
      plot = p_combined,
      width = 14, height = 5,
      bg = "white"
    )
    
    message("✔ Saved: ", location_name, " - Transect ", transect)
  }
}

# Define your file paths
file_paths <- list(
  "SonomaBaylands" = "SonomaBaylands_06032025_LinRef.csv",
  "GiantMarsh" = "GiantMarsh_06022025_LinRef.csv",
  "GallinasCreek" = "GallinasCreek_06172025_LinRef.csv",
  "CorteMadera" = "CorteMadera_06162025_LinRef.csv"
)

# Run the function on all locations
for (loc in names(file_paths)) {
  plot_transects(file_paths[[loc]], loc)
}
