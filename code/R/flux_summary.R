## Statistical Tables

# Adjusted Fisher-Pearson standardized skewness coefficient (same formula
# used by default in most stats software, e.g. SPSS/Minitab "skewness").
# Written out manually rather than pulling in e1071/moments, so this file
# doesn't pick up a new package dependency just for one metric.
# Named skewness_adj() (not skewness()) so it can't get silently shadowed
# by -- or silently shadow -- moments::skewness(), which uses the plain
# (biased) sample skewness formula instead. Returns NA if n < 3, since
# skewness is undefined below that.

skewness_adj <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  
  m <- mean(x)
  s <- sd(x)
  g1 <- sum((x - m)^3) / n / s^3               # sample skewness (biased)
  (sqrt(n * (n - 1)) / (n - 2)) * g1            # bias-adjusted (Fisher-Pearson)
}

# Shapiro-Wilk normality test, returning just the p-value for easy use in
# a summarise() pipeline. shapiro.test() requires 3 <= n <= 5000; returns
# NA outside that range rather than erroring, so this can run safely
# inside grouped summaries where group sizes vary (e.g. small/large Site
# groups in the same table).
shapiro_p <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3 || n > 5000) return(NA_real_)
  
  shapiro.test(x)$p.value
}

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
      Skewness = skewness_adj(Flux, na.rm = TRUE),
      Shapiro_p = shapiro_p(Flux, na.rm = TRUE),
      .groups = "drop"
    )
  
  # NEW: readable p-value for reporting (avoids showing "0.000")
  result <- result %>%
    dplyr::mutate(
      Shapiro_p_display = ifelse(
        is.na(Shapiro_p), NA_character_,
        ifelse(Shapiro_p < 0.001, "< .001", sprintf("%.3f", Shapiro_p))
      )
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
      columns = dplyr::any_of(c("Mean", "SD", "SE", "Median", "Min", "Max", "Skewness", "CI95_Lower", "CI95_Upper")),
      decimals = 3
    ) %>%
    gt::fmt_number(
      columns = dplyr::any_of("Shapiro_p"),
      decimals = 4
    ) %>%
    gt::tab_source_note(source_note = "Flux units: g/m\u00b2") %>%
    gt::tab_source_note(source_note = "Shapiro_p < .05 indicates a significant departure from normality; NA where n < 3 or n > 5000.")
  
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


