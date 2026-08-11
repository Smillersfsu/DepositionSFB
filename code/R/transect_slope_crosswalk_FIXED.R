### Transect Slope/Curvature Covariate — Crosswalk + Extraction Pipeline
### RECONSTRUCTED, not recovered — the original transect_slope_crosswalk_FIXED.R
### wasn't in the GitHub repo (only referenced by other files, never
### committed itself -- likely built in an earlier chat session and
### never saved out). Rebuilt from two solid sources instead of guessing:
###   1. transect_slope_crosswalk.R (the OLDER scaffold version, still
###      in the repo) -- local_slope_and_elevation_at() is unchanged
###      from it below, since thesis_slope.qmd's own successful run
###      this session (finding the real pin-registration bug, writing
###      local_slope_thesis.csv) confirms that function works as-is.
###   2. thesis_slope.qmd's OWN code -- it shows exactly how
###      transect_crosswalk gets used (Site, Tran, Topo_Transect_ID;
###      Topo_Transect_ID looked up directly against
###      topo_data_by_transect, which thesis_slope.qmd itself builds
###      as a list keyed ONLY by "xsa"/"xsb"). That tells us precisely
###      what shape this table needs, rather than guessing at it.
###
### CONFIDENCE NOTE: the crosswalk table below (all 4 sites, T1->xsa,
### T2->xsb) is a reconstruction consistent with everything
### thesis_slope.qmd assumes and everything confirmed working this
### session -- but it's still a rebuild, not the original file byte
### for byte. Worth a quick sanity check before trusting it blindly:
### run the anti_join() check at the bottom once, and confirm
### thesis_slope.qmd's own consistency-check chunk (comparing
### fitted_elevation_m against All_Sites_Calculated) still looks the
### way it did last session.

library(tidyverse)

# ---------------------------------------------------------------------------
# STEP 1: Crosswalk -- topo survey Transect_ID <-> Flux data Site/Tran
# ---------------------------------------------------------------------------
# Simpler than the old scaffold's per-site-file mapping attempt: every
# site's topo file uses "xsa"/"xsb" as its Transect_ID values (confirmed
# in thesis_slope.qmd -- GallinasCreek's uppercase "XSA"/"XSB" gets
# lowercased on load, same as every other site), so the mapping is
# uniform across all 4 sites: T1 -> xsa, T2 -> xsb. No per-site file
# path needed here -- thesis_slope.qmd loads and filters the 4 topo
# CSVs itself (giant_marsh_topo, corte_madera_topo, san_pablo_topo,
# bucks_landing_topo) and builds topo_data_by_transect from them
# directly; this crosswalk only needs to say which Tran maps to which
# Topo_Transect_ID key in that list.

transect_crosswalk <- tribble(
  ~Site,             ~Tran,  ~Topo_Transect_ID,
  "GiantMarsh",       "T1",   "xsa",
  "GiantMarsh",       "T2",   "xsb",
  "CorteMadera",      "T1",   "xsa",
  "CorteMadera",      "T2",   "xsb",
  "SanPablo",         "T1",   "xsa",
  "SanPablo",         "T2",   "xsb",
  "Buck'sLanding",    "T1",   "xsa",
  "Buck'sLanding",    "T2",   "xsb",
)

# Sanity check before trusting this reconstruction -- run once real
# data is loaded (thesis_slope.qmd loads sed_thesis/master_thesis
# upstream of this):
#
# anti_join(sed_thesis %>% distinct(Site, Tran), transect_crosswalk, by = c("Site","Tran"))
# anti_join(transect_crosswalk, sed_thesis %>% distinct(Site, Tran), by = c("Site","Tran"))
# Both should return 0 rows -- if not, this reconstruction has a real gap.

# ---------------------------------------------------------------------------
# STEP 2: Local slope extraction -- smoothing spline, evaluated at each pad's Dist
# ---------------------------------------------------------------------------
# UNCHANGED from the scaffold version -- this is the function
# thesis_slope.qmd actually calls (local_slope_and_elevation_at), and
# its results this session (pin-registration bug found via direct
# comparison against pin markers, extrapolation correctly flagged for
# GiantMarsh T2 / Buck'sLanding T1 / Buck'sLanding T2) confirm it's
# working correctly. Not modified in this reconstruction.

#' Fit a smoothing spline to one transect's elevation profile and return the
#' LOCAL slope (first derivative) at each requested distance.
#'
#' @param topo_df Data frame for ONE transect with Distance_AlongTransect_m
#'   and Elevation_m columns (the RTK survey points).
#' @param query_dists Numeric vector of distances to evaluate slope at --
#'   pass the sediment pad Dist values here, NOT a regular grid.
#' @return Numeric vector of local slopes, same length as query_dists.
local_slope_at <- function(topo_df, query_dists) {
  topo_df <- topo_df %>% arrange(Distance_AlongTransect_m)

  if (nrow(topo_df) < 4) {
    warning("Fewer than 4 survey points -- smoothing spline may be unreliable or fail.")
  }

  fit <- smooth.spline(topo_df$Distance_AlongTransect_m, topo_df$Elevation_m)

  # deriv = 1 gives the first derivative (slope) directly, not a finite-
  # difference approximation.
  predict(fit, query_dists, deriv = 1)$y
}

#' Same idea, but also returns the FITTED ELEVATION at each query distance,
#' so you can sanity-check the spline against the raw survey points before
#' trusting the derivative. This is the function thesis_slope.qmd actually
#' calls.
local_slope_and_elevation_at <- function(topo_df, query_dists) {
  topo_df <- topo_df %>% arrange(Distance_AlongTransect_m)
  fit <- smooth.spline(topo_df$Distance_AlongTransect_m, topo_df$Elevation_m)

  tibble(
    Dist = query_dists,
    fitted_elevation_m = predict(fit, query_dists, deriv = 0)$y,
    local_slope = predict(fit, query_dists, deriv = 1)$y
  )
}

# ---------------------------------------------------------------------------
# STEP 3: Join local slope onto the Flux/sediment-pad data (kept for
# reference/reuse -- thesis_slope.qmd itself does its own equivalent
# join inline via pmap_dfr rather than calling this function directly,
# but add_local_slope() is kept here in case any other file wants a
# single-call version of the same logic.)
# ---------------------------------------------------------------------------

add_local_slope <- function(sed_df, topo_data_by_transect, crosswalk) {
  # topo_data_by_transect: named list, one element per Topo_Transect_ID,
  # each a data frame with Distance_AlongTransect_m + Elevation_m,
  # ALREADY FILTERED TO ONE SITE before being passed in -- crosswalk
  # values ("xsa"/"xsb") are shared across all 4 sites, so the caller
  # is responsible for site-filtering topo_data_by_transect first (see
  # thesis_slope.qmd's own pmap_dfr, which does exactly this: filters
  # topo_df to the current Site before calling the per-distance function).

  sed_df %>%
    left_join(crosswalk, by = c("Site", "Tran")) %>%
    group_by(Site, Tran, Topo_Transect_ID) %>%
    group_modify(~ {
      topo_id <- unique(.y$Topo_Transect_ID)
      if (is.na(topo_id) || is.null(topo_data_by_transect[[topo_id]])) {
        # No topo match (crosswalk gap) -- leave NA rather than
        # guessing, so missing coverage is visible rather than silently
        # dropped or wrongly imputed.
        .x %>% mutate(local_slope = NA_real_)
      } else {
        topo_df <- topo_data_by_transect[[topo_id]]
        .x %>% mutate(local_slope = local_slope_at(topo_df, Dist))
      }
    }) %>%
    ungroup()
}

# ---------------------------------------------------------------------------
# STEP 4: Diagnostics to run BEFORE trusting local_slope in a model
# ---------------------------------------------------------------------------
# 1. Visual check -- does the spline follow the actual survey points
#    reasonably, or is it over/under-smoothed? Plot per transect:
#
# plot_spline_check <- function(topo_df, transect_label) {
#   fit <- smooth.spline(topo_df$Distance_AlongTransect_m, topo_df$Elevation_m)
#   pred_grid <- seq(min(topo_df$Distance_AlongTransect_m),
#                     max(topo_df$Distance_AlongTransect_m), length.out = 200)
#   pred_df <- tibble(x = pred_grid, y = predict(fit, pred_grid)$y)
#
#   ggplot() +
#     geom_point(data = topo_df, aes(Distance_AlongTransect_m, Elevation_m)) +
#     geom_line(data = pred_df, aes(x, y), color = "blue") +
#     labs(title = transect_label, x = "Distance (m)", y = "Elevation (m)") +
#     theme_minimal()
# }
#
# 2. Collinearity check -- local_slope vs. z_star, and local_slope vs. Dist
#    itself (slope varies systematically with position, so some correlation
#    with Dist/Marsh_Area is expected -- the question is whether it's adding
#    NEW information or just re-encoding position).
#
# cor(model_data$local_slope, model_data$z_star, use = "complete.obs")
# cor(model_data$local_slope, as.numeric(as.character(model_data$Dist)), use = "complete.obs")
#
# 3. Coverage check -- how many rows end up with NA local_slope because of
#    an incomplete crosswalk or out-of-range Dist? Decide whether that's
#    small enough to drop, or big enough to mean the crosswalk needs more
#    work before this covariate is usable.
#
# sum(is.na(model_data$local_slope)) / nrow(model_data)
