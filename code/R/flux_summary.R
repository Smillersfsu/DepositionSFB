## Statistical Tables

make_flux_table <- function(data, group_vars = NULL, title, subtitle = NULL,
                            group_labels = NULL, include_ci = FALSE) {
  if (!is.null(group_vars)) {
    data <- dplyr::group_by(data, dplyr::across(dplyr::all_of(group_vars)))
  }
  
  result <- data %>%
    dplyr::summarise(
      n = dplyr::n(),
      Mean = mean(Flux, na.rm = TRUE),
      SD = sd(Flux, na.rm = TRUE),
      SE = SD / sqrt(n),
      Median = median(Flux, na.rm = TRUE),
      Min = min(Flux, na.rm = TRUE),
      Max = max(Flux, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (include_ci) {
    result <- dplyr::mutate(result,
                            CI95_Lower = Mean - qt(0.975, n - 1) * SE,
                            CI95_Upper = Mean + qt(0.975, n - 1) * SE
    )
  }
  
  gt_table <- result %>%
    gt::gt() %>%
    gt::tab_header(title = title, subtitle = subtitle) %>%
    gt::fmt_number(
      columns = dplyr::any_of(c("Mean", "SD", "SE", "Median", "Min", "Max", "CI95_Lower", "CI95_Upper")),
      decimals = 3
    ) %>%
    gt::tab_source_note(source_note = "Flux units: g/m\u00b2")
  
  if (!is.null(group_labels)) {
    gt_table <- gt_table %>% gt::cols_label(!!!group_labels)
  }
  
  list(data = result, table = gt_table)
}

general_result <- make_flux_table(
  master_thesis,
  title = "Summary Statistics for Sediment Flux",
  subtitle = "Overall flux values across all sites and study phases",
  include_ci = TRUE
)
general_summary <- general_result$data
flux_summary_table <- general_result$table

phen_result <- make_flux_table(
  master_thesis, group_vars = "Phen_Period",
  title = "Sediment Flux by Phenological Period",
  group_labels = list(Phen_Period = "Phenological Period")
)
phen_summary <- phen_result$data
flux_period_table <- phen_result$table

edge_result <- make_flux_table(
  master_thesis, group_vars = "marsh_edge_type",
  title = "Sediment Flux by Marsh Edge Type",
  group_labels = list(marsh_edge_type = "Marsh Edge Type")
)
flux_edge_table <- edge_result$table
# no plain "edge_summary" existed before - add one here if you need it for plotting later:
# edge_summary <- edge_result$data

site_result <- make_flux_table(
  master_thesis, group_vars = "Site",
  title = "Sediment Flux by Site",
  group_labels = list(Site = "Site")
)
site_summary <- site_result$data
flux_site_table <- site_result$table

site_phen_result <- make_flux_table(
  master_thesis, group_vars = c("Site", "Phen_Period"),
  title = "Sediment Flux by Site and Phenological Period",
  group_labels = list(Site = "Site", Phen_Period = "Phenological Period")
)
flux_site_phen <- site_phen_result$table
# flux_site_phen_summary <- site_phen_result$data  # if you need this one for plotting too

phase_result <- make_flux_table(
  master_thesis, group_vars = "Study.Phase",
  title = "Sediment Flux by Study Phase",
  group_labels = list(Study.Phase = "Study Phase")
)
phase_summary <- phase_result$data
flux_phase_table <- phase_result$table  # only if you want a gt version too

skewness(master_thesis$Flux, na.rm = TRUE)

# Visual check
hist(master_thesis$Flux)
shapiro.test(master_thesis$Flux)   # p < 0.05 suggests non-normal


