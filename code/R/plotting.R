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
make_sed_plots <- function(data, title_suffix = "", 
                           phase_var = "Date.removed.from.field", phase_label = NULL) {

  # phase_var: column used for the "phase_*" plots below (phase_site,
  # phase_site_log, phase_edge, phase_area). Defaults to
  # "Date.removed.from.field" for all datasets (full/noG/noG2/tiers alike)
  # -- Study.Phase is no longer used anywhere in this function. Override
  # phase_var if you ever want to group these plots by something else.
  # phase_label: axis/legend label text; auto-derived from phase_var if
  # not supplied (dots -> spaces).
  if (is.null(phase_label)) {
    phase_label <- gsub("\\.", " ", phase_var)
  }
  
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
  
  phase_site = make_box_plot(aes(x = .data[[phase_var]], y = Flux, color = Site), paste0("Deposition by ", phase_label, " by Site"), phase_label),
  
  phase_site_log = make_box_plot(aes(x = .data[[phase_var]], y = Flux, color = Site), paste0("Deposition by ", phase_label, " by Site (log)"), phase_label, log_y = TRUE),
  
  phase_edge = make_box_plot(aes(x = .data[[phase_var]], y = Flux, color = marsh_edge_type), paste0("Deposition by ", phase_label, " by Marsh Edge Type"), phase_label),
  
  phase_area = make_box_plot(aes(x = Marsh_Area, y = Flux, color = .data[[phase_var]]), paste0("Deposition by ", phase_label, " Across Marsh Area"), "Marsh Area", facet = ~Site)
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

# NOTE: this file is a pure function library now -- no top-level code
# that runs on source(). The calls that used to live here (building
# plots_full / all_plots / noG_plots / noG2_plots from master_thesis /
# plot_data_noG / plot_data_noG2, plus their save_sed_plots() calls)
# moved to master_analysis_cleanup.Rmd, since that's where those
# objects actually get built -- sourcing this file used to error
# immediately in any document (like flagtier.qmd) that doesn't also
# have master_thesis/plot_data_noG/plot_data_noG2 defined.


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
# NOTE: the per-site calls that used to live here
# (p_san_pablo <- plot_flux_vs_zstar("SanPablo"), etc., plus their
# four ggsave() calls) were duplicate top-level code -- the real,
# working versions already live in elevation_and_z_star.qmd (~line
# 264), which is where elev_z_flux actually gets built. Removed here
# rather than kept as dead weight. If you want those four plots saved
# to disk, add ggsave() calls in elevation_and_z_star.qmd itself,
# following the same file.path(images_folder, ...) pattern used
# elsewhere in this file.
#############################################################


#############################################################
# make_rq_plots()
#
# Moved here from master_analysis_flagtier.qmd (was already
# consolidated there, replacing 4 near-identical chunks -- one per
# tier). Living in plotting.R since it's a plot-producing function
# like make_sed_plots(), not a stats-table function like
# make_flux_table() in flux_summary.R.
#
# Returns a named list of 6 ggplots plus one summary dataframe
# (slopes_by_transect, kept for reference/export, not a plot):
#   interval - collection interval length over time, by transect
#   edge     - edge type comparison: flux by marsh edge type (violin + jitter by site) -- feeds RQ1
#   dist     - inundation gradient: flux vs distance along transect, faceted by edge type -- feeds RQ1
#   time     - trend check: flux over time by transect (thesis-only,
#              faceted by edge type -- NOT the cross-dataset RQ2 test)
#   slope    - trend check: per-transect flux-vs-time slope summary
#              (same thesis-only scope as `time`)
#   season   - seasonality check, flux by month pooled across years
#   site     - site comparison: flux by site, sorted by median -- feeds RQ3
#
# None of these six plots are placeholders or auto-populate later --
# every one runs fully now on whatever tier dataframe you pass in,
# using only thesis-side columns (marsh_edge_type, Dist, Tran, Site,
# Date.removed.from.field). The "feeds RQ1/RQ3" notes above just
# describe which eventual write-up section each plot supports.
#
# The actual RQ2 (GLMM / Shapiro-Wilk comparison between USGS and
# thesis datasets) happens later, in Research question 2.qmd, once
# usgs_thesis_merge.qmd has produced the combined dataframe with the
# "Study Dataset" column. This function only ever sees one tier's
# thesis data at a time, so it can't and doesn't answer that question.
#############################################################

make_rq_plots <- function(df, tier_label) {
  
  # ---------------------------------------------------------------
  # Collection Interval Consistency
  # ---------------------------------------------------------------
  data_intervals <- df %>%
    mutate(interval_days = as.numeric(difftime(Date.removed.from.field, Date.placed.in.field, units = "days"))) %>%
    distinct(Tran, Date.removed.from.field, interval_days)
  
  p_interval <- ggplot(data_intervals, aes(x = Date.removed.from.field, y = interval_days, color = Tran)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_hline(yintercept = 30, linetype = "dashed", color = "gray40") +
    labs(
      title = paste("Collection Interval Length Over Time —", tier_label),
      subtitle = "Dashed line = nominal 30-day target",
      x = "Time", y = "Interval Length (days)", color = "Transect"
    ) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
    theme_minimal() +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(summary(data_intervals$interval_days))
  
  # ---------------------------------------------------------------
  # Edge Type Comparison: Flux by Marsh Edge Type
  # (fully computed now, thesis-only columns -- feeds into RQ1, but
  # does not depend on the USGS merge)
  # ---------------------------------------------------------------
  p_edge <- ggplot(df, aes(x = marsh_edge_type, y = Flux, fill = marsh_edge_type)) +
    geom_violin(alpha = 0.4, trim = FALSE) +
    geom_jitter(aes(color = Site), width = 0.15, size = 2, alpha = 0.7) +
    labs(
      title = paste("Sediment Flux by Marsh Edge Type —", tier_label),
      subtitle = "Points colored by Site — check if the two sites within each type agree",
      x = NULL, y = "Flux (g/m2/day)"
    ) +
    theme_minimal() +
    theme(legend.position = "right")
  
  # ---------------------------------------------------------------
  # Inundation Gradient: Flux vs. Distance Along Transect
  # (fully computed now, thesis-only columns -- feeds into RQ1, but
  # does not depend on the USGS merge)
  # ---------------------------------------------------------------
  p_dist <- ggplot(df, aes(x = Dist, y = Flux, color = Tran, group = Tran)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_line(alpha = 0.5) +
    geom_smooth(mapping = aes(group = marsh_edge_type),
                method = "loess", color = "black",
                linewidth = 1, se = TRUE, inherit.aes = FALSE) +
    facet_wrap(~ marsh_edge_type) +
    labs(
      title = paste("Sediment Flux vs Distance Along Transect —", tier_label),
      subtitle = "Black smooth = overall trend within edge type; colored lines = individual transects",
      x = "Distance Along Transect (m)", y = "Flux (g/m2/day)", color = "Transect"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  # ---------------------------------------------------------------
  # Trend: Flux Over Time, by Edge Type
  # (thesis-only check that flux stays constant over time within this
  # tier -- NOT the cross-dataset RQ2 GLMM comparison, which happens
  # later in Research question 2.qmd once USGS + thesis are merged)
  # ---------------------------------------------------------------
  p_time <- ggplot(df, aes(x = Date.removed.from.field, y = Flux, color = Tran, group = Tran)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_line(alpha = 0.4) +
    geom_smooth(mapping = aes(group = marsh_edge_type),
                method = "lm", se = TRUE, color = "black",
                linewidth = 1, inherit.aes = FALSE) +
    facet_wrap(~ marsh_edge_type) +
    labs(
      title = paste("Sediment Flux Over Time by Transect —", tier_label),
      subtitle = "A flat black trend line within a panel supports a constant-flux hypothesis",
      x = "Time (Year-Month)", y = "Flux (g/m2/day)", color = "Transect"
    ) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
    theme_minimal() +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
  
  # ---------------------------------------------------------------
  # Trend: Per-Transect Slope Summary
  # (same thesis-only scope as p_time above)
  # ---------------------------------------------------------------
  slopes_by_transect <- df %>%
    group_by(Tran, marsh_edge_type) %>%
    summarize(
      slope = coef(lm(Flux ~ as.numeric(Date.removed.from.field)))[2],
      .groups = "drop"
    )
  
  p_slope <- ggplot(slopes_by_transect, aes(x = marsh_edge_type, y = slope, color = marsh_edge_type)) +
    geom_jitter(width = 0.1, size = 3, alpha = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
      title = paste("Flux Trend Over Time, by Transect —", tier_label),
      subtitle = "Values near zero support the constant-flux hypothesis",
      x = NULL, y = "Slope of Flux vs Time (per day)"
    ) +
    theme_minimal()
  
  # ---------------------------------------------------------------
  # Seasonality Check (Exploratory)
  # ---------------------------------------------------------------
  p_season <- ggplot(df, aes(x = factor(month(Date.removed.from.field)), y = Flux, fill = marsh_edge_type)) +
    geom_boxplot(alpha = 0.7) +
    labs(
      title = paste("Sediment Flux by Month (Pooled Across Years) —", tier_label),
      subtitle = "Checks whether flux varies seasonally, independent of longer-term trend",
      x = "Month", y = "Flux (g/m2/day)", fill = "Edge Type"
    ) +
    theme_minimal()
  
  # ---------------------------------------------------------------
  # Site Comparison: Flux by Site
  # (fully computed now, thesis-only columns -- feeds into RQ3, but
  # does not depend on the USGS merge)
  # ---------------------------------------------------------------
  p_site <- ggplot(df, aes(x = reorder(Site, Flux, median), y = Flux, fill = marsh_edge_type)) +
    geom_boxplot(alpha = 0.7) +
    labs(
      title = paste("Sediment Flux by Site —", tier_label),
      subtitle = "Sorted by median — flags whether MorphType groups sites as expected",
      x = "Site", y = "Flux (g/m2/day)", fill = "Edge Type"
    ) +
    theme_minimal() +
    coord_flip()
  
  # Return everything as a named list so you can access/print/save individually,
  # e.g. rq_plots$tier4$edge, or walk(rq_plots$tier4, print)
  list(
    interval = p_interval,
    edge     = p_edge,
    dist     = p_dist,
    time     = p_time,
    slope    = p_slope,
    season   = p_season,
    site     = p_site,
    slopes_by_transect = slopes_by_transect  # data, not a plot, kept for reference/export
  )
}
