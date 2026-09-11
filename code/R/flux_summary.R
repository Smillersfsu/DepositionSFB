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

# ADDED: raw (non-excess) sample kurtosis -- normal distribution ~= 3,
# NOT 0. Deliberately calls moments::kurtosis() rather than writing
# another hand-rolled formula (the way skewness_adj() above does) --
# moments is already a real dependency of this project (loaded in
# setup.R), and using it here specifically matches the convention
# already established and explained for
# distribution_comparison (Claude).qmd's shape_stats table: both use
# moments::kurtosis(), both read ~3 as "normal," so a Kurtosis number
# from this table and one from that document are directly comparable.
#
# NOTE ON AN EXISTING INCONSISTENCY, not something this change fixes:
# Skewness (skewness_adj(), above) and Kurtosis (kurtosis_raw(), here)
# in this same table are computed by two DIFFERENT methods --
# skewness_adj() is a hand-rolled, BIAS-ADJUSTED (Fisher-Pearson)
# skewness that does not use the moments package at all, while
# kurtosis_raw() calls moments::kurtosis() directly (unadjusted).
# They're internally consistent (every table built with this file uses
# the same two formulas), but worth knowing if either number is ever
# checked against a source that computes both with one single package.
kurtosis_raw <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  n <- length(x)
  if (n < 4) return(NA_real_)   # kurtosis needs more points than skewness to be meaningful
  
  moments::kurtosis(x)
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

# ADDED: group-comparison test (Mann-Whitney or Kruskal-Wallis), for the
# "does Flux actually differ across these groups" question -- a
# different question from Shapiro_p above, which only checks per-group
# normality and says nothing about whether groups differ from each
# other. Chooses the test automatically based on how many groups
# group_vars actually defines in the data:
#   - exactly 2 groups  -> Mann-Whitney U (wilcox.test)
#   - 3+ groups         -> Kruskal-Wallis (kruskal.test), the same idea
#     generalized past two groups -- same relationship Kruskal-Wallis
#     has to Mann-Whitney that Fligner-Killeen has to a two-group
#     variance test in distribution_comparison (Claude).qmd.
#   - 0 or 1 groups (no group_vars, or only one level present after
#     filtering) -> NULL, nothing to compare.
# exact = FALSE on the Mann-Whitney call avoids the "cannot compute
# exact p-value with ties" warning that's likely with a large
# replicate-level dataset (repeated/rounded Flux values are common
# enough at that grain to produce ties).
group_comparison_test <- function(data, group_vars) {
  if (is.null(group_vars)) return(NULL)
  
  data_complete <- data %>% dplyr::filter(!is.na(Flux))
  group_factor <- interaction(
    dplyr::select(data_complete, dplyr::all_of(group_vars)),
    drop = TRUE
  )
  n_groups <- nlevels(group_factor)
  
  if (n_groups == 2) {
    res <- tryCatch(
      wilcox.test(data_complete$Flux ~ group_factor, exact = FALSE),
      error = function(e) NULL
    )
    if (is.null(res)) return(NULL)
    list(test = "Mann-Whitney U", statistic = unname(res$statistic), p_value = res$p.value)
  } else if (n_groups > 2) {
    res <- tryCatch(
      kruskal.test(data_complete$Flux ~ group_factor),
      error = function(e) NULL
    )
    if (is.null(res)) return(NULL)
    list(test = "Kruskal-Wallis", statistic = unname(res$statistic), p_value = res$p.value)
  } else {
    NULL
  }
}

make_flux_table <- function(data, group_vars = NULL, title, subtitle = NULL,
                            group_labels = NULL, include_ci = FALSE) {
  # ADDED: compute the group-comparison test on the RAW row-level data,
  # before group_by()/summarise() collapses it to one row per group --
  # wilcox.test()/kruskal.test() need the original Flux values split by
  # group, not the per-group summary statistics.
  comparison_test <- group_comparison_test(data, group_vars)
  
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
      Kurtosis = kurtosis_raw(Flux, na.rm = TRUE),
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
      columns = dplyr::any_of(c("Mean", "SD", "SE", "Median", "Min", "Max", "Skewness", "Kurtosis", "CI95_Lower", "CI95_Upper")),
      decimals = 3
    ) %>%
    gt::fmt_number(
      columns = dplyr::any_of("Shapiro_p"),
      decimals = 4
    ) %>%
    gt::tab_source_note(source_note = "Flux units: g/m\u00b2") %>%
    gt::tab_source_note(source_note = "Kurtosis is RAW (not excess) -- a normal distribution reads ~3, not 0.") %>%
    gt::tab_source_note(source_note = "Shapiro_p < .05 indicates a significant departure from normality; NA where n < 3 or n > 5000.")
  
  # ADDED: group-comparison source note, only when one was computable
  # (2+ groups present). States which test ran and why, so a reader
  # doesn't have to guess whether Mann-Whitney or Kruskal-Wallis was
  # used for a given table.
  if (!is.null(comparison_test)) {
    comparison_p_display <- if (comparison_test$p_value < 0.001) {
      "< .001"
    } else {
      sprintf("%.3f", comparison_test$p_value)
    }
    gt_table <- gt_table %>%
      gt::tab_source_note(source_note = paste0(
        comparison_test$test, " across ", paste(group_vars, collapse = " x "),
        ": statistic = ", sprintf("%.2f", comparison_test$statistic),
        ", p = ", comparison_p_display,
        " -- tests whether Flux differs across these groups (distinct from Shapiro_p, which only checks per-group normality)."
      ))
  }
  
  if (!is.null(group_labels)) {
    gt_table <- gt_table %>% gt::cols_label(!!!group_labels)
  }
  
  list(data = result, table = gt_table, comparison_test = comparison_test)
}

# NOTE: this file is a pure function library now -- no top-level code
# that runs on source(). The calls that used to live here (general_result
# / phen_result / edge_result / site_result / site_phen_result /
# phase_result, all built from master_thesis, plus the skewness/hist/
# shapiro.test() diagnostic checks) moved to master_analysis_cleanup.Rmd,
# since that's where master_thesis actually gets built -- sourcing this
# file used to error immediately in any document (like flagtier.qmd)
# that doesn't also have master_thesis defined.
#
# group_comparison_test() (and the source note it adds to every
# make_flux_table() call with group_vars set) is new -- added to answer
# "does Flux actually differ across these groups," which Shapiro_p was
# never able to answer on its own.


