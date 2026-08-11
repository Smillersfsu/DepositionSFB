### Transect Slope/Curvature Covariate — Crosswalk + Extraction Pipeline
### Status: SCAFFOLD — the crosswalk table below has placeholder rows.
### Fill in the actual Transect_ID <-> Site/Tran mapping before running.
###
### Goal: instead of one slope value per transect (averages away curvature),
### compute LOCAL slope at each sediment pad's actual Dist, using a smoothing
### spline fit to the RTK topo survey and evaluating its first derivative.
### This lets Edge pads and Upland pads on the same transect get different
### slope values, reflecting real curvature (e.g. steep drop near a scarp
### that flattens out on the platform) instead of one averaged number.

library(tidyverse)

# ---------------------------------------------------------------------------
# STEP 1: Crosswalk -- topo survey Transect_ID <-> Flux data Site/Tran
# ---------------------------------------------------------------------------
# The RTK topo files (LinearReferencing.R / Notebook_TransectsPlots.Rmd) use
# Transect_ID values like "A", "B", "C", "D" per site. The Flux/sediment-pad
# data uses Site + Tran ("T1"/"T2"). There's no existing link between them --
# this table IS that link. Same pattern as R/marsh_area_crosswalk.R, just for
# transect identity instead of marsh zone.
#
# TODO: fill in the real Transect_ID for each Site x Tran combination.
# Placeholder values below are guesses based on file naming seen this
# session (SonomaBaylands / GiantMarsh / GallinasCreek / CorteMadera topo
# files) -- CONFIRM every row before trusting anything downstream.

transect_crosswalk <- tribble(
  ~Site,             ~Tran,  ~Topo_Transect_ID,  ~Topo_Source_File,
  "GiantMarsh",       "T1",   "A",                "GiantMarsh_06022025_LinRef.csv",
  "GiantMarsh",       "T2",   "B",                "GiantMarsh_06022025_LinRef.csv",
  "CorteMadera",      "T1",   "A",                "CorteMadera_06162025_LinRef.csv",
  "CorteMadera",      "T2",   "B",                "CorteMadera_06162025_LinRef.csv",
  "SanPablo",         "T1",   NA_character_,      NA_character_,  # TODO: no SanPablo topo file identified yet -- confirm source
  "SanPablo",         "T2",   NA_character_,      NA_character_,  # TODO
  "Buck'sLanding",    "T1",   NA_character_,      NA_character_,  # TODO
  "Buck'sLanding",    "T2",   NA_character_,      NA_character_,  # TODO
)

# Sanity check before using: are there Site x Tran combos in the Flux data
# with no topo match, or topo transects never referenced? Run once both
# real datasets are loaded:
#
# anti_join(sed_thesis %>% distinct(Site, Tran), transect_crosswalk, by = c("Site","Tran"))
# anti_join(transect_crosswalk, sed_thesis %>% distinct(Site, Tran), by = c("Site","Tran"))

# ---------------------------------------------------------------------------
# STEP 2: Local slope extraction -- smoothing spline, evaluated at each pad's Dist
# ---------------------------------------------------------------------------

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
#' trusting the derivative.
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
# STEP 3: Join local slope onto the Flux/sediment-pad data
# ---------------------------------------------------------------------------
# ASSUMPTION TO CONFIRM: sediment pad Dist values (0.5, 2, 6, 12, 18, 24, 48
# for thesis; different set for USGS, see Research_question_2.qmd's notes)
# fall WITHIN the surveyed range of the matching topo transect. If a pad's
# Dist is beyond where the RTK survey actually measured, smooth.spline will
# extrapolate -- which can produce misleading slope values. Worth checking
# range(topo_df$Distance_AlongTransect_m) against the pad Dist values before
# trusting the output for any given transect.

add_local_slope <- function(sed_df, topo_data_by_transect, crosswalk) {
  # topo_data_by_transect: named list, one element per Topo_Transect_ID,
  # each a data frame with Distance_AlongTransect_m + Elevation_m.

  sed_df %>%
    left_join(crosswalk, by = c("Site", "Tran")) %>%
    group_by(Site, Tran, Topo_Transect_ID) %>%
    group_modify(~ {
      topo_id <- unique(.y$Topo_Transect_ID)
      if (is.na(topo_id) || is.null(topo_data_by_transect[[topo_id]])) {
        # No topo match yet (crosswalk TODOs above) -- leave NA rather than
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

# ---------------------------------------------------------------------------
# NEXT STEPS (for tomorrow)
# ---------------------------------------------------------------------------
# 1. Fill in the real Transect_ID values in transect_crosswalk (Site x Tran
#    for all 4 sites, thesis AND usgs if their transect naming differs).
# 2. Load the actual topo survey CSVs and build topo_data_by_transect as a
#    named list keyed by Topo_Transect_ID.
# 3. Run the anti_join() sanity checks above to confirm crosswalk coverage.
# 4. Run add_local_slope() on sed_thesis and sed_usgs separately (they may
#    need separate crosswalks if topo coverage differs between them).
# 5. Run the Step 4 diagnostics before adding local_slope to any GLMM.
# 6. Check whether a single global smoothing parameter is appropriate for
#    every transect, or whether some transects need spar= tuned individually
#    (visible from the Step 4 visual check).
