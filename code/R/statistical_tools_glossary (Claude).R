# ============================================================
# statistical_tools_glossary.R (Claude)
# ============================================================
# Definitions for every statistical tool/function used across
# RQ1-spatial.Rmd, RQ1-temporal.qmd, Research_question_2_FIXED.qmd, and
# RQ3-variables.qmd. Source this alongside modeling_conventions.R.
#
# Each RQ file also has its OWN short "Statistical Tools Used in This
# File" section near the top, listing only the subset relevant to that
# file, so you don't have to come back here while reading a chunk. This
# file is the fuller version, for when you want more than a sentence.

# ------------------------------------------------------------------
# MODEL FITTING
# ------------------------------------------------------------------
# lmer() [lme4]
#   Fits a linear mixed-effects model: a regression with both FIXED
#   effects (the predictors you want an estimate/p-value for) and
#   RANDOM effects (grouping variables you want to account for but
#   aren't estimating a specific effect for, e.g. (1|Site) = "let each
#   site have its own baseline Flux level"). Assumes Gaussian
#   (normally-distributed) residuals. Used here as the SENSITIVITY
#   check for every model where a Gamma GLMM is the primary (Rule 2 in
#   modeling_conventions.R), almost always on log(Flux) since raw Flux
#   isn't close to normal.
#
# glmer() [lme4]
#   Same idea as lmer(), but for a Generalized Linear Mixed Model --
#   i.e. you can specify a non-Gaussian family (Poisson, binomial, etc.)
#   and a link function. Used once in RQ1-spatial (distmodel) with the
#   default family; glmmTMB (below) is used everywhere else a
#   non-Gaussian family is actually needed, since it supports Gamma.
#
# glmmTMB() [glmmTMB]
#   The PRIMARY model-fitting function in this project (Rule 2). Like
#   glmer(), but built on Template Model Builder, which supports a
#   wider range of families (including Gamma, used throughout for Flux)
#   and more flexible variance/dispersion structures (e.g.
#   dispformula = ~ Site, or the ar1() correlation structure used in
#   RQ1-temporal for repeated monthly sampling).
#
# family = Gamma(link = "log")
#   Tells glmmTMB the response is modeled as Gamma-distributed (right-
#   skewed, strictly positive -- matches Flux's shape) with a log link,
#   meaning predictors act multiplicatively on Flux rather than
#   additively. This is why emmeans(..., type = "response") is needed
#   to get back-transformed, directly interpretable estimates.

# ------------------------------------------------------------------
# SIGNIFICANCE / MODEL COMPARISON
# ------------------------------------------------------------------
# Anova() [car, capital A]
#   Runs Type II (or Type III) significance tests on the FIXED effects
#   of an already-fitted model (lmer or glmmTMB object) -- answers "is
#   this predictor significant overall," accounting properly for other
#   terms in the model. This is the standard way to test fixed effects
#   in this project; used after every glmmTMB() primary model.
#
# anova() [base R / lme4, lowercase a]
#   Two different uses show up in these files:
#     1. anova(model) on a single lmer object -- an ANOVA-style table
#        for that model's fixed effects (Satterthwaite-approximated df
#        if lmerTest is loaded).
#     2. anova(model_a, model_b) on two NESTED models -- a likelihood
#        ratio test asking "does adding this term significantly improve
#        fit," e.g. deposition_model_base vs. deposition_model_slope in
#        RQ1-spatial (does adding local_slope help).
#   Don't confuse this with Anova() above -- different capitalization,
#   different job.
#
# AIC()
#   Akaike Information Criterion -- a single number balancing model fit
#   against model complexity; lower is better. Used to compare
#   non-nested models that anova()'s likelihood ratio test can't compare
#   directly (e.g. edge_time_model_glmm vs. its AR1 version in
#   RQ1-temporal, or rq3_model_glmm vs. rq3_model_glmm_edgeID in RQ3).
#   A useful secondary check, not a substitute for asking whether the
#   two models are actually answering the same question.

# ------------------------------------------------------------------
# POST-HOC / EFFECT SIZE
# ------------------------------------------------------------------
# emmeans() [emmeans]
#   "Estimated marginal means" -- takes a fitted model and computes the
#   model's predicted average outcome for each level (or combination of
#   levels) of a factor, holding other predictors at their mean/reference
#   value. Used throughout for pairwise comparisons (e.g.
#   emmeans(model, pairwise ~ Study.Dataset | Site)) and to build the
#   emmeans plots (points + CI ribbons by Month, Phen_Period, etc.).
#   type = "response" back-transforms Gamma/log-link estimates onto the
#   original Flux scale -- always use this for Gamma models, or you're
#   reading log-scale numbers as if they were Flux.
#
# emtrends() [emmeans]
#   Same family as emmeans(), but for the SLOPE of a continuous
#   predictor rather than a factor mean -- e.g. emtrends(model_full,
#   ~ marsh_edge_type, var = "z_star") asks "is the Flux~z_star slope
#   different between Ramped and Scarped," not just "is the mean
#   different."
#
# ggpredict() [ggeffects]
#   Generates model-predicted values across a range of a predictor
#   (e.g. z_star from min to max), for plotting a fitted line/ribbon
#   over the raw data. Conceptually similar to emmeans() but built for
#   continuous predictors and direct ggplot handoff.
#
# r.squaredGLMM() [MuMIn]
#   Computes marginal R² (variance explained by fixed effects only) and
#   conditional R² (fixed + random effects) for a mixed model -- gives a
#   sense of overall model fit, since a normal R² isn't defined the same
#   way once random effects are involved.

# ------------------------------------------------------------------
# DISTRIBUTION / RESIDUAL DIAGNOSTICS
# ------------------------------------------------------------------
# shapiro.test()
#   Tests whether a numeric vector is consistent with a normal
#   distribution (null hypothesis: normal). A significant result
#   (p < .05) means "reject normality." Used early in each file as a
#   first check on raw Flux -- consistently rejects normality, which is
#   part of the justification for Gamma over Gaussian (Rule 2).
#
# qqnorm() / qqline() [base R]
#   Visual companion to shapiro.test() -- plots your data's quantiles
#   against theoretical normal quantiles; points hugging the line =
#   normal-ish, curving away = skewed/heavy-tailed. Same limitation as
#   shapiro.test(): built around the normal distribution, so it's a
#   reasonable check on raw Flux but not the right diagnostic for a
#   fitted Gamma model's residuals -- that's what DHARMa is for, below.
#
# simulateResiduals() / plot() / testDispersion() /
# testTemporalAutocorrelation() / recalculateResiduals() [DHARMa]
#   DHARMa works by simulation rather than assuming residuals should
#   look normal, which is what makes it appropriate for a Gamma GLMM
#   where raw residuals never look Gaussian even in a well-fit model.
#   For each observation, it simulates many fake datasets from the
#   fitted model, builds an empirical distribution of "what the model
#   thinks this observation should look like," and asks where the real
#   value falls in that distribution (as a quantile, 0-1). If the model
#   is correctly specified, these quantile residuals come out uniformly
#   distributed regardless of the underlying family -- so "does this
#   Gamma model fit" becomes "are these numbers uniform," a
#   distribution-free question you can test/plot normally.
#     - plot(sim_res): Q-Q plot vs. uniform(0,1) (+ KS test) on the
#       left, residuals-vs-fitted with quantile regression lines on the
#       right. Flat lines / points on the diagonal = good fit.
#     - testDispersion(): checks whether simulated data's variance
#       matches the real data's variance -- flags Gamma over/under-
#       dispersion, which usually means the wrong family or a missing
#       predictor/random effect.
#     - testTemporalAutocorrelation(): checks whether residuals at
#       consecutive time points (Study.Phase here) are correlated --
#       relevant because this is repeated monthly sampling at the same
#       transect. A significant result is the actual justification for
#       trying the ar1() correlation structure, rather than assuming
#       autocorrelation is or isn't a problem.
#     - recalculateResiduals(): re-aggregates the simulated residuals
#       for a subset of rows (used per-Loc_ID in RQ1-temporal) so
#       testTemporalAutocorrelation() can be run separately for each
#       transect's own time series, not pooled across all of them.

# ------------------------------------------------------------------
# VARIABLE SELECTION
# ------------------------------------------------------------------
# cv.glmnet() / glmnet() [glmnet]
#   Fits a LASSO-penalized regression: like ordinary regression, but
#   adds a penalty that shrinks weak-predictor coefficients all the way
#   to exactly zero, effectively doing automatic variable selection.
#   cv.glmnet() uses cross-validation to pick the penalty strength
#   (lambda) that generalizes best rather than just fitting the
#   training data closest. Used in RQ3 to cross-check that the
#   predictors kept in rq3_model_glmm (z_star, local_slope,
#   marsh_edge_type) are actually the ones that survive automatic
#   selection, not just the ones chosen by hand.

# ------------------------------------------------------------------
# NOT YET IN THESE FILES, but worth knowing (raised in conversation):
# ------------------------------------------------------------------
# descdist() / fitdist() / gofstat() [fitdistrplus]
#   descdist() plots raw data on a skewness-kurtosis (Cullen and Frey)
#   graph to suggest plausible candidate distributions; fitdist() fits a
#   specific named distribution via maximum likelihood; gofstat()
#   compares several fitted candidates by AIC/BIC/K-S side by side. This
#   tests the distribution of the RAW response variable -- useful
#   justification for choosing Gamma in the first place, but it's a
#   different question from whether a fitted GLMM's residuals look
#   right once covariates are included (that's DHARMa's job, above).
