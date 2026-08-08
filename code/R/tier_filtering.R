#############################################################
# filter_flux()
#
# Reconstructed from the tier-building chunks in
# master_analysis_flagtier.qmd (~lines 96-148), which built
# master_tier1 / tier1.5 / tier2 / tier3 / tier4 by hand-copying
# the same two filter steps five times. This collapses that into
# one function parameterized by site_scope and flag_criterion, as
# planned in Session_Status.
#
# site_scope:
#   "all"  - no site filtering (base elev_z_flux)
#   "noG"  - drop Site == "GiantMarsh" entirely
#   "noG2" - drop only GiantMarsh Transect 2 (keeps GiantMarsh Tran 1)
#
# flag_criterion:
#   "review_only" - drop only rows flagged Master_Flag == "Review"
#   "full"        - drop rows flagged for big biological disturbance,
#                   negative values, or flood observation
#                   (BigBio_Obsv, NegValue_Num, Flood_Obsv == "1")
#
# Original tier <-> (site_scope, flag_criterion) mapping (whole-number naming):
#   tier1 <- filter_flux(elev_z_flux, "noG",  "review_only")
#   tier2 <- filter_flux(elev_z_flux, "noG2", "review_only")
#   tier3 <- filter_flux(elev_z_flux, "all",  "review_only")
#   tier4 <- filter_flux(elev_z_flux, "noG2", "full")
#   tier5 <- filter_flux(elev_z_flux, "all",  "full")
#   tier6 <- filter_flux(elev_z_flux, "noG",  "full")
#############################################################

filter_flux <- function(data = elev_z_flux,
                         site_scope = c("all", "noG", "noG2"),
                         flag_criterion = c("review_only", "full")) {

  site_scope     <- match.arg(site_scope)
  flag_criterion <- match.arg(flag_criterion)

  out <- data

  out <- switch(site_scope,
    all  = out,
    noG  = dplyr::filter(out, !(Site == "GiantMarsh")),
    noG2 = dplyr::filter(out, !(Site == "GiantMarsh" & Tran == "2"))
  )

  out <- switch(flag_criterion,
    review_only = dplyr::filter(out, !(Master_Flag == "Review")),
    full        = dplyr::filter(out,
                    !(BigBio_Obsv == "1"),
                    !(NegValue_Num == "1"),
                    !(Flood_Obsv == "1"))
  )

  out
}

#############################################################
# save_tier()
#
# Small wrapper around readr::write_csv so the tier-building chunk
# in the .qmd doesn't have to repeat the full Windows OneDrive path
# five times. Swap in file.path(data_folder, ...) if/when the
# hardcoded path gets centralized (see Loose Ends in Session_Status).
#############################################################

save_tier <- function(data, filename,
                       folder = "C:\\Users\\savan\\OneDrive - San Francisco State University\\ProjectData\\GitHub\\DepositionSFB\\data") {
  readr::write_csv(data, file.path(folder, filename))
}
