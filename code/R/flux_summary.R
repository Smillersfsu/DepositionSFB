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

# NOTE: this file is a pure function library now -- no top-level code
# that runs on source(). The calls that used to live here (general_result
# / phen_result / edge_result / site_result / site_phen_result /
# phase_result, all built from master_thesis, plus the skewness/hist/
# shapiro.test() diagnostic checks) moved to master_analysis_cleanup.Rmd,
# since that's where master_thesis actually gets built -- sourcing this
# file used to error immediately in any document (like flagtier.qmd)
# that doesn't also have master_thesis defined.


