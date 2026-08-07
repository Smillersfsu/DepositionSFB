#############################################################
# make_sed_plots()

# Builds every plot from the original "Distributions" chunk
# (master_analysis_cleanup.Rmd, ~line 271) as a named list, so
# the same function can be reused for master_thesis, 
# plot_data_noG, and plot_data_noG2 without triplicating code.

#List_name    <- original object name(s)
#basic        <- sed_basic / sed_basic_noG / sed_basic_noG2
#edge         <- sed_edge / sed_edge_noG / sed_edge_noG2
#dist_edge    <- sed_dist_edge / sed_dist_edge_noG / sed_dist_edge_noG2
#time         <- sed_time / sed_time_noG / sed_time_noG2
#edge_area    <- sed_edge_area / sed_edge_area_noG / sed_edge_area_noG2
#site_time    <- sed_site_time / sed_site_time_noG / sed_site_time_noG2
#site_area    <- sed_site_area / sed_site_area_noG / sed_site_area_noG2
#phase_site   <- sed_phase_site / sed_phase_site_noG / sed_phase_site_noG2
#phase_site_log <- sed_phase_site_log / sed_phase_site_log_noG / sed_phase_site_log_noG2
#phase_edge   <- sed_phase_edge / sed_phase_edge_noG / sed_phase_edge_noG2
#phase_area   <- sed_phase_area / sed_phase_area_noG / sed_phase_area_noG2

# NOTE: the "sed_z" plot (Flux vs z*) was commented out in the
# original with "do this later once you've figured out z*" --
# not included here since it was never actually built. Add it
# once z* is ready to go, following the same pattern below.

#############################################################
make_sed_plots <- function(data, title_suffix = "") {

make_box_plot <- function(mapping, title, x_lab, facet = NULL, log_y = FALSE) {
  p <- ggplot(data, mapping) +
    geom_boxplot() +
    geom_jitter(width = 0.15, alpha = 0.5) +
    theme_minimal() +
    labs(title = paste0(title, title_suffix), x = x_lab, y = expression(Flux~(g/m^2/day)))
  if (log_y) p <- p + scale_y_log10()
  if (!is.null(facet)) p <- p + facet_wrap(facet)
  p
}

plots <- list(
  
  basic = ggplot(data, aes(x = Flux)) +
    geom_histogram(bins = 30) +
    theme_minimal() +
    labs(
      title = paste0("Flux Distribution", title_suffix),
      x = expression(Flux~(g/m^2/day)),
      y = "Count"
    ),
  
  edge = make_box_plot(aes(x = marsh_edge_type, y = Flux), "Deposition by Edge Type", "Marsh Edge Type"),
  
  dist_edge = make_box_plot(aes(x = Dist, y = Flux, color = marsh_edge_type), "Deposition by Distance from Marsh Edge", "Distance from the Marsh Edge (m)"),
  
  time = make_box_plot(aes(x = Phen_Period, y = Flux), "Deposition by Phenological Time", "Phenological Period"),
  
  edge_area = make_box_plot(aes(x = Marsh_Area, y = Flux, color = marsh_edge_type), "Deposition by Marsh Area", "Marsh Area"),
  
  site_time = make_box_plot(aes(x = Phen_Period, y = Flux, color = Site), "Deposition by Phenological Time by Site", "Phenological Period"),
  
  site_area = make_box_plot(aes(x = Marsh_Area, y = Flux, color = Site), "Deposition by Marsh Area by Site", "Marsh Area"),
  
  phase_site = make_box_plot(aes(x = Study.Phase, y = Flux, color = Site), "Deposition by Study Phase by Site", "Study Phase"),
  
  phase_site_log = make_box_plot(aes(x = Study.Phase, y = Flux, color = Site), "Deposition by Study Phase by Site (log)", "Study Phase", log_y = TRUE),
  
  phase_edge = make_box_plot(aes(x = Study.Phase, y = Flux, color = marsh_edge_type), "Deposition by Study Phase by Marsh Edge Type", "Study Phase"),
  
  phase_area = make_box_plot(aes(x = Marsh_Area, y = Flux, color = Study.Phase), "Deposition by Study Phase Across Marsh Area", "Marsh Area", facet = ~Site)
  )

if ("z_star" %in% names(data)) {
  plots$elev_site <- make_box_plot(
    aes(x = z_star, y = Flux, color = Site),
    "Deposition by Site Across Relative Elevations (z*)",
    "z*"
  )
}

plots
}


save_sed_plots <- function(plot_list, folder, prefix) {
  if (!dir.exists(folder)) dir.create(folder, recursive = TRUE) #use this next time if you want to invent a folder of your plots next time
  
  for (name in names(plot_list)) {
    print(plot_list[[name]])
    ggsave(
      filename = file.path(folder, paste0(prefix, "_", name, ".png")),
      plot = plot_list[[name]],
      width = 8,
      height = 5,
      dpi = 300,
      bg = "white"
    )
  }
}

images_folder <- "C:\\Users\\savan\\OneDrive - San Francisco State University\\ProjectData\\GitHub\\DepositionSFB\\images"


# Save Full Dataset Plots 

plots_full <- make_sed_plots(master_thesis, title_suffix = " (Full Dataset)")


save_sed_plots(plots_full, file.path(images_folder, "all_sites_raw_plot"), "sed_full")


#############################################################
# BUILD ALL THREE SETS
#############################################################

all_plots  <- make_sed_plots(master_thesis)
noG_plots  <- make_sed_plots(plot_data_noG,  title_suffix = " (No Giant Marsh)")
noG2_plots <- make_sed_plots(plot_data_noG2, title_suffix = " (No Giant Marsh, Tran 2)")

save_sed_plots(all_plots,  file.path(images_folder, "all_sites"),         "sed")
save_sed_plots(noG_plots,  file.path(images_folder, "no_giant_marsh"),    "sed_noG")
save_sed_plots(noG2_plots, file.path(images_folder, "no_giant_marsh_t2"), "sed_noG2")

# View any individual plot the same way you would have before:
all_plots$basic
noG_plots$phase_site_log
noG2_plots$dist_edge


#############################################################
# FLUX vs z* BY SITE, WITH LINEAR TREND + R^2 ANNOTATION
#
# Replaces the four separate ~85-line chunks (San Pablo, Corte
# Madera, Giant Marsh, Buck's Landing) with one function.
#
# This also fixes, by construction, the four copy-paste bugs
# from the original chunks (points 4 in the review):
#   - San Pablo chunk printed "CorteMadera" instead of "SanPablo"
#   - Giant Marsh chunk used nrow(s_df) and range(s_df$x)
#     instead of its own g_df
#   - Buck's Landing chunk used "CorteMadera" and s_df in the
#     same way
# Since every site now runs through the same function body
# using its own df, there's no longer a second copy of the
# logic that could drift out of sync with the first.
#############################################################

plot_flux_vs_zstar <- function(site_name, data = elev_z_flux) {
  
  if (!site_name %in% data[["Site"]]) {
    stop(paste0(
      "No rows found for site '", site_name, "'. ",
      "Available sites: ", paste(unique(data[["Site"]]), collapse = ", ")
    ))
  }
  
  df <- data %>%
    filter(.data[["Site"]] == site_name) %>%
    select(x = all_of("z_star"), y = all_of("Flux")) %>%
    na.omit()
  
  # --- Fit linear model and get R^2 ---
  fit       <- lm(y ~ x, data = df)
  r_squared <- summary(fit)$r.squared
  slope     <- coef(fit)[2]
  intercept <- coef(fit)[1]
  p_value   <- summary(fit)$coefficients[2, 4]
  
  cat("Site:", site_name, "\n")
  cat("n =", nrow(df), "\n")
  cat(sprintf("slope = %.4f, intercept = %.4f\n", slope, intercept))
  cat(sprintf("R^2 = %.4f, p = %.4g\n", r_squared, p_value))
  
  # --- Build plot ---
  r2_label <- paste0("R^2 == ", round(r_squared, 2))
  
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.5) +
    geom_point(size = 3, color = "#3b6fa0", alpha = 0.9) +
    geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1) +
    annotate(
      "text",
      x = min(df$x) + 0.05 * diff(range(df$x)),
      y = max(df$y) * 0.9,
      label = r2_label,
      parse = TRUE,
      hjust = 0,
      size = 5,
      color = "dimgray"
    ) +
    labs(
      title = site_name,
      x = "z*",
      y = "Flux"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.background = element_rect(fill = "#e9e9e9", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "white"),
      panel.grid.minor = element_line(color = "white"),
      plot.title = element_text(hjust = 0.5)
    )
  
  print(p)
  p
}

#############################################################
# RUN FOR ALL FOUR SITES
#
# Kept the original explanatory header as context for why this
# code exists (per your call to preserve it), just trimmed down
# to the actual working line -- no more copy-pasted CONFIG block.
#
# ------------------------------------------------------------
# Plot flux (elevation/accretion surplus, mm/yr) vs z* for ONE site,
# with a linear trend line and R^2 annotation -- styled after Thorne et al.
#
# Expected input: a CSV with at least these columns:
#   site   : site/category name, e.g. "SFB Elevation", "SC Accretion"
#   z_star : position within tidal frame (x-axis)
#   flux   : elevation/accretion surplus rate, mm/yr (y-axis)
# ------------------------------------------------------------
#############################################################

p_san_pablo  <- plot_flux_vs_zstar("SanPablo")
p_corte_mad  <- plot_flux_vs_zstar("CorteMadera")
p_giant_mar  <- plot_flux_vs_zstar("GiantMarsh")
p_bucks_land <- plot_flux_vs_zstar("Buck'sLanding")

#############################################################
# SAVE ALL FOUR (explicit plot = ..., consistent path/filenames
# -- fixes the bare-relative-filename issue from point 5)
#############################################################

ggsave(file.path(images_folder, "flux_vs_zstar_sanpablo.png"),
       plot = p_san_pablo, width = 8, height = 6, dpi = 200, bg = "white")

ggsave(file.path(images_folder, "flux_vs_zstar_cortemadera.png"),
       plot = p_corte_mad, width = 8, height = 6, dpi = 200, bg = "white")

ggsave(file.path(images_folder, "flux_vs_zstar_giantmarsh.png"),
       plot = p_giant_mar, width = 8, height = 6, dpi = 200, bg = "white")

ggsave(file.path(images_folder, "flux_vs_zstar_buckslanding.png"),
       plot = p_bucks_land, width = 8, height = 6, dpi = 200, bg = "white")
