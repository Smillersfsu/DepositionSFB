#############################################################
# marsh_area_crosswalk()
#
# Reads Site_S_USGS_Thesis_matchguide.xlsx and reshapes it into a tidy
# reference table showing, for each Marsh_Area, what raw Dist value
# each dataset actually used to sample that position -- despite the
# file's name, it covers BOTH Corte Madera (Site C) and San Pablo
# (Site S), which sit side by side in the same sheet (cols 1-3 = Site
# C, cols 4-6 = Site S).
#
# This does NOT change either dataset's Dist column -- both raw
# distance scales are kept as their own columns here. The point is to
# make visible that they are NOT directly comparable: e.g. at San
# Pablo, USGS's Dist = 24 and Thesis's Dist = 0.5 are both "Edge" --
# the same physical marsh position -- even though the numbers look
# nothing alike. Corte Madera's two scales are almost a 1:1 match;
# San Pablo's are not. Comparing Flux by raw Dist instead of by
# Marsh_Area would silently mix up marsh positions at San Pablo.
#
# Both existing Marsh_Area derivations already agree with this guide
# (thesis's uniform Edge/Platform/Upland thresholds in
# master_analysis_cleanup.Rmd, and USGS's per-site thresholds in
# master_analysis_usgs.Rmd) -- this crosswalk doesn't replace either,
# it documents the mapping explicitly and lets you double-check both
# stay in sync with the guide going forward.
#############################################################

build_marsh_area_crosswalk <- function(
    path = "C:\\Users\\savan\\OneDrive - San Francisco State University\\ProjectData\\GitHub\\DepositionSFB\\data\\Site_S_USGS_Thesis_matchguide.xlsx"
) {

  # Read by position, not by header name -- the sheet's header row has
  # a duplicated label ("Thesis Site S Locations") reused for both the
  # Site C and Site S tables, so headers alone can't disambiguate them.
  raw <- readxl::read_excel(path, sheet = 1, col_names = FALSE, skip = 1) %>%
    dplyr::select(1:6)

  names(raw) <- c("USGS_Dist_C", "Thesis_Dist_C", "Marsh_Area_C",
                   "USGS_Dist_S", "Thesis_Dist_S", "Marsh_Area_S")

  # Wide, side-by-side view -- one row per Marsh_Area per site, showing
  # both datasets' Dist value for that position. This is the direct
  # "these marsh areas are different" table.
  wide_c <- raw %>%
    dplyr::transmute(Site = "CorteMadera",
                      USGS_Dist = USGS_Dist_C,
                      Thesis_Dist = Thesis_Dist_C,
                      Marsh_Area = Marsh_Area_C) %>%
    dplyr::filter(!is.na(Marsh_Area))

  wide_s <- raw %>%
    dplyr::transmute(Site = "SanPablo",
                      USGS_Dist = USGS_Dist_S,
                      Thesis_Dist = Thesis_Dist_S,
                      Marsh_Area = Marsh_Area_S) %>%
    dplyr::filter(!is.na(Marsh_Area))

  crosswalk_wide <- dplyr::bind_rows(wide_c, wide_s) %>%
    dplyr::mutate(Marsh_Area = factor(Marsh_Area,
                    levels = c("Intertidal", "Edge", "Platform", "Upland")))

  # Long version -- one row per (Site, Study.Dataset, Dist), ready to
  # join onto usgs_thesis (or sed_thesis/sed_usgs individually) by
  # Site + Study.Dataset + Dist, e.g. to validate the Marsh_Area each
  # file already assigned matches this guide.
  crosswalk_long <- dplyr::bind_rows(
    crosswalk_wide %>% dplyr::transmute(Site, Study.Dataset = "USGS",
                                          Dist = USGS_Dist, Marsh_Area),
    crosswalk_wide %>% dplyr::transmute(Site, Study.Dataset = "Thesis",
                                          Dist = Thesis_Dist, Marsh_Area)
  ) %>%
    dplyr::filter(!is.na(Dist)) %>%
    dplyr::distinct()

  list(wide = crosswalk_wide, long = crosswalk_long)
}

