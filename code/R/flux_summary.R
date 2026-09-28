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
#
# ADDED (Claude): the "Image export" section below (save_glmm_table,
# save_glmm_tables, save_site_split_tables, and two p-value formatters)
# -- used by the output-table chunks at the end of RQ1-spatial and
# RQ1-temporal.


## Image export: GLMM output tables and per-site descriptive tables
#
# ADDED (Claude): these were briefly in a separate file, model_tables
# (Claude).R -- moved here so every table-building function lives in one
# place. Like everything above, these are functions only (no top-level
# code runs on source()), and every call is namespaced (dplyr::,
# flextable::, glmmTMB::) so sourcing this file doesn't attach packages.
#
#   save_glmm_table()        -- one image of one glmmTMB model's output
#   save_glmm_tables()       -- the same, looped over a list of models,
#                               for both the _byrep and _avg versions
#   save_site_split_tables() -- one make_flux_table() image per Site
#
# Images are written with flextable::save_as_image(), the same way the
# existing site_flux_stats tables are. File names always end in _byrep or
# _avg; the tier is passed in explicitly, never guessed.

# p-value as a table cell, same style as Shapiro_p_display: "< .001", ".023"
fmt_p_cell <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.001, "< .001", sub("^0", "", formatC(p, format = "f", digits = 3))))
}

# p-value inside a sentence: "p < .001", "p = .023"
fmt_p_sentence <- function(p) {
  ifelse(is.na(p), "p = NA",
         ifelse(p < 0.001, "p < .001", paste0("p = ", fmt_p_cell(p))))
}

# -----------------------------------------------------------------------------
# save_glmm_table()
#
# model   : a fitted glmmTMB object
# name    : file stem INCLUDING the tier suffix, e.g. "distmodel_glmm_byrep"
# title   : caption shown above the table
# out_dir : folder the PNG is written to
#
# Body   = fixed effects: estimate (link scale), SE, z, p, and -- for log
#          links -- exp(estimate) with a 95% Wald CI (the multiplicative
#          change in Flux per unit, or vs. the reference level).
# Footer = Type II Wald chi-square tests (car::Anova), random-effect
#          variances, dispersion-model terms (if any), N, AIC, marginal /
#          conditional R2 (performance::r2), formula, family.
# -----------------------------------------------------------------------------
save_glmm_table <- function(model, name, title, out_dir) {

  if (!inherits(model, "glmmTMB")) {
    warning(name, " is not a glmmTMB model -- skipped.")
    return(invisible(NULL))
  }

  sm       <- summary(model)
  fam      <- stats::family(model)
  log_link <- identical(fam$link, "log")

  # ---- fixed effects -------------------------------------------------------
  cf <- as.data.frame(sm$coefficients$cond)
  fixed <- data.frame(
    Term     = rownames(cf),
    Estimate = cf[, "Estimate"],
    SE       = cf[, "Std. Error"],
    z        = cf[, "z value"],
    p        = fmt_p_cell(cf[, "Pr(>|z|)"]),
    stringsAsFactors = FALSE
  )
  if (log_link) {
    fixed[["exp(\u03b2)"]]  <- exp(fixed$Estimate)
    fixed[["95% CI low"]]  <- exp(fixed$Estimate - 1.96 * fixed$SE)
    fixed[["95% CI high"]] <- exp(fixed$Estimate + 1.96 * fixed$SE)
  }

  # ---- dispersion-model terms (only when dispformula != ~1) ----------------
  disp_lines <- character(0)
  if (!is.null(sm$coefficients$disp) && nrow(sm$coefficients$disp) > 1) {
    d <- as.data.frame(sm$coefficients$disp)
    disp_lines <- paste0(
      "Dispersion model -- ", rownames(d), ": \u03b2 = ", sprintf("%.3f", d[, 1]),
      ", SE = ", sprintf("%.3f", d[, 2]), ", ", fmt_p_sentence(d[, 4])
    )
  }

  # ---- Type II Wald chi-square tests ---------------------------------------
  anova_lines <- tryCatch({
    a <- car::Anova(model, type = "II")
    paste0("Type II Wald \u03c7\u00b2 -- ", rownames(a), ": \u03c7\u00b2(", a$Df, ") = ",
           sprintf("%.2f", a$Chisq), ", ", fmt_p_sentence(a$`Pr(>Chisq)`))
  }, error = function(e) "Type II Wald \u03c7\u00b2: could not be computed for this model")

  # ---- random effects --------------------------------------------------------
  vc    <- glmmTMB::VarCorr(model)$cond
  ngrps <- sm$ngrps$cond
  re_lines <- vapply(names(vc), function(g) {
    sds <- attr(vc[[g]], "stddev")
    cor <- attr(vc[[g]], "correlation")
    base <- if (length(sds) == 1) {
      paste0("Random intercept (", g, "): variance = ", sprintf("%.3f", sds^2),
             ", SD = ", sprintf("%.3f", sds))
    } else {
      # e.g. ar1(Study.Phase + 0 | Loc_ID): one shared SD + lag-1 correlation
      rho <- if (!is.null(cor) && ncol(cor) > 1) sprintf("%.3f", cor[1, 2]) else "NA"
      paste0("Random effect (", g, "): SD = ", sprintf("%.3f", sds[1]),
             ", lag-1 correlation \u03c1 = ", rho)
    }
    paste0(base, " [", ngrps[[g]], " groups]")
  }, character(1))

  # ---- fit summary -------------------------------------------------------------
  r2_line <- tryCatch({
    r2 <- suppressWarnings(suppressMessages(performance::r2(model)))
    paste0("Marginal R\u00b2 = ", sprintf("%.3f", r2$R2_marginal),
           "; conditional R\u00b2 = ", sprintf("%.3f", r2$R2_conditional),
           " (NA = a random-effect variance was estimated at ~0)")
  }, error = function(e) "R\u00b2: could not be computed for this model")

  info_line <- paste0("N = ", stats::nobs(model), " observations; AIC = ",
                      sprintf("%.1f", stats::AIC(model)))
  form_line <- paste0("Formula: ",
                      paste(deparse(stats::formula(model), width.cutoff = 500), collapse = ""))
  fam_line  <- paste0("Family: ", fam$family, " (", fam$link, " link)",
                      if (log_link) " -- exp(\u03b2) = multiplicative change in Flux; 95% CI = Wald" else "")

  footer <- c(anova_lines, re_lines, disp_lines, info_line, r2_line, form_line, fam_line)

  # ---- build + save -------------------------------------------------------------
  ft <- flextable::flextable(fixed) %>%
    flextable::set_caption(paste0(title, "  [", name, "]")) %>%
    flextable::colformat_double(digits = 3) %>%
    flextable::add_footer_lines(footer) %>%
    flextable::fontsize(part = "footer", size = 8) %>%
    flextable::align(j = -1, align = "right", part = "header") %>%
    flextable::align(j = -1, align = "right", part = "body") %>%
    flextable::align(align = "left", part = "footer") %>%
    flextable::autofit()

  path <- file.path(out_dir, paste0(name, ".png"))
  flextable::save_as_image(ft, path = path)
  message("Saved: ", path)
  invisible(ft)
}

# -----------------------------------------------------------------------------
# save_glmm_tables()
#
# model_titles : named list -- names = model object names as they exist in
#                your session (the byrep version, no suffix), values =
#                captions. For each name, saves <name>_byrep.png from the
#                object as named and <name>_avg.png from <name>_avg.
# Any object that doesn't exist yet is reported as NOT FOUND, not fatal.
# -----------------------------------------------------------------------------
save_glmm_tables <- function(model_titles, out_dir, env = parent.frame()) {
  for (obj in names(model_titles)) {
    for (tier in c("byrep", "avg")) {
      obj_name <- if (tier == "byrep") obj else paste0(obj, "_avg")
      caption  <- paste0(model_titles[[obj]],
                         if (tier == "byrep") " (by replicate)" else " (averaged)")
      if (exists(obj_name, envir = env, inherits = TRUE)) {
        tryCatch(
          save_glmm_table(get(obj_name, envir = env, inherits = TRUE),
                          name = paste0(obj, "_", tier), title = caption,
                          out_dir = out_dir),
          error = function(e) message("FAILED ", obj_name, ": ", conditionMessage(e))
        )
      } else {
        message("NOT FOUND (run its chunk first): ", obj_name)
      }
    }
  }
}

# -----------------------------------------------------------------------------
# save_site_split_tables()
#
# Splits a Site x <group_var> make_flux_table() into one table per Site,
# e.g. tier5_dist_site_stats (Site x Dist) -> four Dist tables.
#
# data        : data frame with Site, Flux and the grouping column
# group_var   : column summarised within each site ("Dist", "Study.Phase")
# group_label : display name for that column ("Distance (m)", "Study Phase")
# file_stub   : e.g. "dist_stats" -> dist_stats_GiantMarsh_byrep.png
# tier        : "byrep" or "avg" (goes into the file name and caption)
# out_dir     : output folder
#
# Columns are make_flux_table()'s own (n, Mean, SD, SE, Median, Min, Max,
# Skewness, Kurtosis, Shapiro_p), with Shapiro_p shown in its "< .001"
# display form -- same layout as the combined table. make_flux_table()'s
# gt source notes don't carry over to flextable, so the same notes are
# re-added as footer lines, including the Kruskal-Wallis test of whether
# Flux differs across <group_var> within that site.
# -----------------------------------------------------------------------------
save_site_split_tables <- function(data, group_var, group_label, file_stub,
                                   tier = c("byrep", "avg"), out_dir) {
  tier       <- match.arg(tier)
  tier_label <- if (tier == "byrep") "By Replicate" else "Averaged"
  sites      <- if (is.factor(data$Site)) levels(droplevels(data$Site)) else sort(unique(data$Site))
  out <- list()

  for (s in sites) {
    site_data <- dplyr::filter(data, Site == s)

    tbl <- make_flux_table(
      site_data,
      group_vars = group_var,
      title      = paste0("Sediment Flux by ", group_label, " -- ", s, " (", tier_label, ")"),
      subtitle   = "Flux measured in g/m\u00b2/day"
    )

    # numeric Shapiro_p -> the "< .001" display version, under the same name
    tbl_data <- tbl$data %>%
      dplyr::select(-dplyr::any_of(c("Site", "Shapiro_p"))) %>%
      dplyr::rename(Shapiro_p = Shapiro_p_display)

    footer <- c(
      "Flux units: g/m\u00b2/day",
      "Kurtosis is RAW (not excess) -- a normal distribution reads ~3, not 0.",
      "Shapiro_p < .05 indicates a significant departure from normality; NA where n < 3 or n > 5000."
    )
    if (!is.null(tbl$comparison_test)) {
      ct <- tbl$comparison_test
      footer <- c(footer, paste0(
        ct$test, " across ", group_label, " within ", s,
        ": statistic = ", sprintf("%.2f", ct$statistic), ", ", fmt_p_sentence(ct$p_value)
      ))
    }

    ft <- flextable::flextable(tbl_data) %>%
      flextable::set_header_labels(values = stats::setNames(list(group_label), group_var)) %>%
      flextable::set_caption(paste0("Descriptive Statistics of Sediment Flux by ", group_label,
                                    " -- ", s, " (", tier_label, ")")) %>%
      flextable::colformat_double(digits = 3) %>%
      flextable::add_footer_lines(footer) %>%
      flextable::fontsize(part = "footer", size = 8) %>%
      flextable::align(align = "left", part = "footer") %>%
      flextable::autofit()

    site_stub <- gsub("[^A-Za-z0-9]", "", s)   # "Buck'sLanding" -> "BucksLanding"
    path <- file.path(out_dir, paste0(file_stub, "_", site_stub, "_", tier, ".png"))
    flextable::save_as_image(ft, path = path)
    message("Saved: ", path)
    out[[s]] <- ft
  }
  invisible(out)
}
