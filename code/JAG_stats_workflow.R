################################################################################
# Statistics Workflow
# Template R script for exploratory plots, preliminary tests, model fitting,
# diagnostics, model selection, and interpretation.
#
# Purpose:
#   This script is designed as an example package for ecology data with 
#   blocked experimental designs, treatments, covariates such as elevation 
#   or inundation, and common response types such as biomass, chlorophyll, 
#   density, counts, proportions, or repeated measurements.
#
# How to use:
#   1. Copy this file into the project folder.
#   2. Replace the example column names with actual column names.
#   3. Work through the script from top to bottom.
#   4. Keep notes in the commented sections on why each model was kept or rejected.
#
# General philosophy:
#   - Start with plots and diagrams before running models.
#   - Understand the design: treatments, blocks, years, plots, repeated measures.
#   - Use simple summaries and exploratory tests to learn the data structure.
#   - Choose a candidate model set based on the design and response distribution.
#   - Diagnose model fit before interpreting p-values or pairwise comparisons.
#   - Report the model that best matches both the ecology and diagnostics.
################################################################################


#### 0. PACKAGES ####

# Install missing packages if needed:
# install.packages(c(
#   "tidyverse", "lme4", "lmerTest", "car", "emmeans", "performance",
#   "DHARMa", "glmmTMB", "patchwork", "GGally", "broom.mixed"
# ))

library(tidyverse)
library(lme4)
library(lmerTest)
library(car)
library(emmeans)
library(performance)
library(DHARMa)
library(glmmTMB)
library(patchwork)
library(GGally)
library(broom.mixed)

# Optional: set a consistent plot theme
custom_theme <- theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank())
theme_set(custom_theme)


#### 1. LOAD AND PREPARE DATA ####

# Replace this file name with your actual data file.
# Keep raw data unchanged; create a cleaned object for analysis.
raw_dat <- read.csv("your_data_file.csv")

# Example expected columns. Rename these to match the actual data:
#   response_count        count response such as total_snails or total_amph
#   response_continuous   continuous response such as chl_a, biomass, height
#   treatment             experimental treatment, e.g., inundation treatment
#   grazer_treatment      second treatment, e.g., crab cage / no cage
#   year                  sampling or experiment year
#   block                 block ID
#   plot_id               plot ID or experimental unit
#   time_point            initial/final or repeated time step
#   water_level           measured covariate, e.g., inundation hours
#   elevation             measured covariate, e.g., RTK elevation
#   location              site or marsh

# Clean factor/covariate types. Edit this block for your dataset.
dat <- raw_dat %>%
  mutate(
    treatment = as.factor(treatment),
    grazer_treatment = as.factor(grazer_treatment),
    year = as.factor(year),
    block = as.factor(block),
    plot_id = as.factor(plot_id),
    time_point = as.factor(time_point),
    location = as.factor(location),
    water_level = as.numeric(water_level),
    elevation = as.numeric(elevation)
  )
 
# Optional: set biologically meaningful reference levels.
# dat$treatment <- factor(dat$treatment, levels = c("control", "partial", "full"))
# dat$grazer_treatment <- factor(dat$grazer_treatment, levels = c("no_treat", "cage", "no_cage", "egc"))

# Basic checks before plotting or modeling.
glimpse(dat)
summary(dat)
colSums(is.na(dat))

# Check replication. This is often where design problems first appear.
dat %>% count(location, year, block, treatment, grazer_treatment)
dat %>% count(year, block)
dat %>% count(plot_id, time_point)


#### 2. DEFINE THE DESIGN BEFORE MODELING ####

# Write the design in words before choosing a model.
# Example:
#   - Treatments are applied to plots.
#   - Blocks group plots within a year or site.
#   - If blocks changed between years, blocks are nested in year: (1 | year:block).
#   - If the whole experiment was repeated in multiple years, year can be treated
#     as a replicate of the experiment: (1 | year).
#   - If treatment effects may differ by year, include treatment-by-year structure.
#   - Repeated measures on the same plot need plot-level random effects, e.g.
#     (1 | plot_id), or possibly a time interaction.

# Mixed model for an RCBD repeated across years:
#   outcome ~ treatment + water_level + (1 | year) + (1 | year:block) +
#             (1 | treatment:year)
#
# Interpretation:
#   - water_level is an ANCOVA-style covariate measured on each plot.
#   - This assumes the water_level slope does not differ by treatment unless
#     treatment:water_level is added and supported.
#   - treatment:year as a random effect allows treatment effects to vary across
#     years. This is often ecologically plausible, but it changes how the main
#     treatment effect is tested and may limit post-hoc tests for that interaction.


#### 3. EXPLORATORY PLOTS AND DIAGRAMS ####

# 3A. Response distributions ---------------------------------------------------

# Continuous response
p_hist_cont <- ggplot(dat, aes(x = response_continuous)) +
  geom_histogram(bins = 20) +
  labs(title = "Distribution of continuous response", x = "Response", y = "Count")

# Count response
p_hist_count <- ggplot(dat, aes(x = response_count)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Distribution of count response", x = "Count response", y = "Frequency")

p_hist_cont
p_hist_count

# Things to look for:
#   - Continuous positive skew: consider log() or sqrt() transformation.
#   - Count data with many zeros: consider Poisson, negative binomial, or zero-inflated models.
#   - Strong outliers: check field notes before deleting anything.


# 3B. Treatment patterns --------------------------------------------------------

p_box_cont <- ggplot(dat, aes(x = treatment, y = response_continuous)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.6) +
  facet_wrap(~ year) +
  labs(title = "Continuous response by treatment and year")

p_box_count <- ggplot(dat, aes(x = treatment, y = response_count)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.6) +
  facet_wrap(~ year) +
  labs(title = "Count response by treatment and year")

p_box_cont
p_box_count

# If there are two crossed treatments:
ggplot(dat, aes(x = treatment, y = response_continuous, shape = grazer_treatment)) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.3), size = 2) +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width = 0.3), width = 0.15) +
  facet_wrap(~ year) +
  labs(title = "Mean response by crossed treatments", y = "Mean +/- SE")


# 3C. Covariate relationships --------------------------------------------------

# Check whether covariates differ by treatment. If they do, they may need to be
# included as covariates, but be careful not to control away part of the treatment.
ggplot(dat, aes(x = treatment, y = water_level)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.6) +
  facet_wrap(~ year) +
  labs(title = "Water level / inundation by treatment")

ggplot(dat, aes(x = water_level, y = response_continuous, shape = treatment)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ year) +
  labs(title = "Response vs water level by treatment")

# Important ANCOVA check:
# If treatment groups have different slopes against water_level, a simple
# outcome ~ treatment + water_level model may be too simple. Test or plot
# treatment:water_level before assuming one shared slope.


# 3D. Repeated-measures trajectories -------------------------------------------

# Use this only if the same plot was measured repeatedly.
ggplot(dat, aes(x = time_point, y = response_continuous, group = plot_id, shape = treatment)) +
  geom_line(alpha = 0.35) +
  geom_point(alpha = 0.75) +
  facet_wrap(~ year) +
  labs(title = "Plot-level trajectories over time")


# 3E. Simple diagram of design --------------------------------------------------

# A table can be more useful than a formal diagram for spotting imbalance.
design_table <- dat %>%
  count(year, block, treatment, grazer_treatment, name = "n_plots") %>%
  arrange(year, block, treatment, grazer_treatment)

design_table

# Optional visual design map if x/y coordinates exist:
# ggplot(dat, aes(x = x_coord, y = y_coord, shape = treatment)) +
#   geom_point(size = 3) +
#   facet_grid(year ~ block) +
#   coord_equal() +
#   labs(title = "Field design map")


#### 4. BASIC SUMMARIES AND PRELIMINARY TESTS ####

# Summary statistics by treatment/year.
dat %>%
  group_by(year, treatment, grazer_treatment) %>%
  summarize(
    n = n(),
    mean_cont = mean(response_continuous, na.rm = TRUE),
    sd_cont = sd(response_continuous, na.rm = TRUE),
    se_cont = sd_cont / sqrt(n),
    mean_count = mean(response_count, na.rm = TRUE),
    var_count = var(response_count, na.rm = TRUE),
    .groups = "drop"
  )

# Quick count-data check: variance much larger than mean suggests overdispersion.
dat %>%
  group_by(treatment) %>%
  summarize(
    mean_count = mean(response_count, na.rm = TRUE),
    var_count = var(response_count, na.rm = TRUE),
    var_to_mean = var_count / mean_count,
    .groups = "drop"
  )

# Preliminary tests are not the final analysis, but they help identify structure.
# Example: Do covariates differ across treatment groups?
# Use with caution because design balance and random effects matter.
summary(lm(water_level ~ treatment * year, data = dat))
car::Anova(lm(water_level ~ treatment * year, data = dat), type = 3)

# Example: Is there visual/statistical support for treatment-specific covariate slopes?
lm_slope_check <- lm(response_continuous ~ treatment * water_level, data = dat)
summary(lm_slope_check)
car::Anova(lm_slope_check, type = 3)


#### 5. CHOOSE A STARTING MODEL FAMILY ####

# Use the response variable and diagnostic plots to choose candidates.
#
# Continuous, roughly normal residuals:
#   lmer(response ~ predictors + random effects)
#
# Continuous, positive and right-skewed:
#   lmer(log(response) ~ predictors + random effects)
#   lmer(sqrt(response) ~ predictors + random effects)
#
# Counts with mean approximately equal to variance:
#   glmer(count ~ predictors + random effects, family = poisson)
#
# Counts with overdispersion:
#   glmer.nb(count ~ predictors + random effects)
#   or glmmTMB(count ~ predictors + random effects, family = nbinom2)
#
# Counts with many structural zeros:
#   glmmTMB(count ~ predictors + random effects,
#           ziformula = ~ 1,
#           family = nbinom2)
#
# Proportions / binary outcomes:
#   glmer(cbind(successes, failures) ~ predictors + random effects,
#         family = binomial)


#### 6. HELPER FUNCTIONS FOR MODEL DIAGNOSTICS ####

# This function creates a consistent diagnostic workflow for lmer, glmer, glmer.nb,
# and many glmmTMB models.
check_fit <- function(model, model_name = deparse(substitute(model))) {
  message("\n==============================")
  message("Diagnostics for: ", model_name)
  message("==============================")

  print(summary(model))

  message("\nModel performance checks:")
  print(performance::check_singularity(model))
  print(performance::check_collinearity(model))

  # check_model is especially useful for Gaussian models but can still help.
  print(performance::check_model(model))

  message("\nDHARMa simulated residuals:")
  sim_res <- DHARMa::simulateResiduals(model, plot = FALSE)
  plot(sim_res)
  print(DHARMa::testUniformity(sim_res))
  print(DHARMa::testDispersion(sim_res))
  print(DHARMa::testOutliers(sim_res))

  invisible(sim_res)
}

# Notes for interpreting diagnostics:
#   - Singular fit: random-effect structure may be too complex for the data.
#   - Non-normal residuals in Gaussian models: consider transformation or GLMM.
#   - Heteroskedasticity: consider transformation, different family, or variance model.
#   - Overdispersion in Poisson GLMM: consider negative binomial or observation-level
#     random effect.
#   - DHARMa patterns: do not interpret fixed effects until the residual pattern is acceptable.


#### 7. CANDIDATE MODELS: CONTINUOUS RESPONSE ####

# Example 1: simple randomized complete block design within one year/site.
m_cont_1 <- lmer(
  response_continuous ~ treatment + water_level + (1 | block),
  data = dat,
  REML = FALSE
)

# Example 2: crossed treatments.
m_cont_2 <- lmer(
  response_continuous ~ treatment * grazer_treatment + water_level + (1 | block),
  data = dat,
  REML = FALSE
)

# Example 3: transformed response for positive, skewed data such as chlorophyll.
# Add a small constant only if zeros are present and the choice is documented.
m_cont_log <- lmer(
  log(response_continuous) ~ treatment * grazer_treatment + water_level + (1 | block),
  data = dat,
  REML = FALSE
)

m_cont_sqrt <- lmer(
  sqrt(response_continuous) ~ treatment * grazer_treatment + water_level + (1 | block),
  data = dat,
  REML = FALSE
)

# Example 4: experiment repeated across years; blocks nested in year.
m_cont_year <- lmer(
  response_continuous ~ treatment + water_level +
    (1 | year) + (1 | year:block) + (1 | treatment:year),
  data = dat,
  REML = FALSE
)

# Example 5: check whether the covariate slope differs among treatments.
m_cont_slope <- lmer(
  response_continuous ~ treatment * water_level +
    (1 | year) + (1 | year:block),
  data = dat,
  REML = FALSE
)

# Run diagnostics before choosing.
check_fit(m_cont_1, "continuous: treatment + covariate + block")
check_fit(m_cont_2, "continuous: crossed treatments")
check_fit(m_cont_log, "continuous: log transformed")
check_fit(m_cont_sqrt, "continuous: sqrt transformed")
check_fit(m_cont_year, "continuous: year/block structure")
check_fit(m_cont_slope, "continuous: treatment-specific water_level slopes")


#### 8. CANDIDATE MODELS: COUNT RESPONSE ####

# Example 1: Poisson GLMM.
m_count_pois <- glmer(
  response_count ~ treatment * grazer_treatment + water_level + (1 | block),
  data = dat,
  family = poisson
)

# Example 2: negative binomial GLMM using lme4.
m_count_nb <- glmer.nb(
  response_count ~ treatment * grazer_treatment + water_level + (1 | block),
  data = dat
)

# Example 3: negative binomial using glmmTMB. This is often flexible and stable.
m_count_tmb_nb <- glmmTMB(
  response_count ~ treatment * grazer_treatment + water_level + (1 | block),
  data = dat,
  family = nbinom2
)

# Example 4: zero-inflated negative binomial if many zeros are plausible structural zeros.
m_count_tmb_zinb <- glmmTMB(
  response_count ~ treatment * grazer_treatment + water_level + (1 | block),
  ziformula = ~ 1,
  data = dat,
  family = nbinom2
)

# Example 5: repeated experiment across years.
m_count_year_nb <- glmmTMB(
  response_count ~ treatment + water_level +
    (1 | year) + (1 | year:block) + (1 | treatment:year),
  data = dat,
  family = nbinom2
)

# Diagnostics.
check_fit(m_count_pois, "count: Poisson")
check_fit(m_count_nb, "count: negative binomial lme4")
check_fit(m_count_tmb_nb, "count: negative binomial glmmTMB")
check_fit(m_count_tmb_zinb, "count: zero-inflated negative binomial")
check_fit(m_count_year_nb, "count: year/block negative binomial")


#### 9. MODEL SELECTION PROCESS ####

# Step 1: Define candidate models before looking at p-values.
# Candidate models should reflect real biological/statistical alternatives:
#   - Is the interaction biologically meaningful?
#   - Is year a fixed effect, random effect, or replicate of the experiment?
#   - Are blocks nested within year?
#   - Is the response Gaussian, transformed Gaussian, Poisson, or negative binomial?
#   - Is there overdispersion or zero inflation?

# Step 2: Compare only models fit to the same data and response scale.
# Do not compare AIC across models with different response transformations
# unless you understand the implications. Use diagnostics
# first, then compare AIC among models in the same family/scale.

# Example AIC comparison among count models fit to same response/data.
AIC(m_count_pois, m_count_nb, m_count_tmb_nb, m_count_tmb_zinb)

# Example nested likelihood-ratio tests.
# Use these for nested models fit with ML, not REML, and interpret cautiously.
anova(m_cont_1, m_cont_2, test = "Chisq")
anova(m_count_tmb_nb, m_count_tmb_zinb)

# Example: compare with/without an interaction.
m_count_no_interaction <- glmmTMB(
  response_count ~ treatment + grazer_treatment + water_level + (1 | block),
  data = dat,
  family = nbinom2
)
AIC(m_count_no_interaction, m_count_tmb_nb)
anova(m_count_no_interaction, m_count_tmb_nb)

# Step 3: Refit the selected Gaussian model with REML = TRUE for final estimates.
# This applies to lmer Gaussian models after fixed-effect selection.
# final_cont_model <- update(m_cont_2, REML = TRUE)

# Step 4: Record why models were rejected.
# Example model-selection notes:
#   - Poisson GLMM rejected because DHARMa showed overdispersion.
#   - Log-transformed LMM improved residual spread relative to untransformed model.
#   - Treatment:water_level interaction was not retained because slopes appeared
#     parallel and the interaction did not improve fit.
#   - Random treatment:year term was retained because the experiment was repeated
#     across years and treatment effects were expected to vary by year.


#### 10. FINAL MODEL INTERPRETATION ####

# Replace this with the selected model.
final_model <- m_count_tmb_nb

# Overall tests.
# car::Anova works for many model classes. For glmmTMB, also consider drop1().
car::Anova(final_model, type = 3)
drop1(final_model, test = "Chisq")

# Estimated marginal means. This is what I primarily use.
# For GLMMs, type = "response" back-transforms to the original response scale.
emm_treatment <- emmeans(final_model, ~ treatment, type = "response")
emm_treatment
pairs(emm_treatment)

# If there is an interaction, compare treatments within levels of the other factor.
emm_interaction <- emmeans(final_model, ~ treatment | grazer_treatment, type = "response")
emm_interaction
pairs(emm_interaction)

# If treatment-by-year was modeled as random, standard post-hoc p-values for that
# interaction may not be available or meaningful. In that case, use predictions
# and uncertainty intervals to describe how treatment effects vary by year.


#### 11. PREDICTIONS AND EFFECT PLOTS ####

# Build a prediction grid for visualizing model-estimated treatment means.
newdat <- dat %>%
  distinct(treatment, grazer_treatment) %>%
  mutate(
    water_level = mean(dat$water_level, na.rm = TRUE),
    block = dat$block[1]
  )

# emmeans is often easier and safer than manual predict() for marginal means.
emm_df <- as.data.frame(emmeans(final_model, ~ treatment | grazer_treatment, type = "response"))

# Check the column names because emmeans output differs among model families.
names(emm_df)

# For GLMMs, response-scale estimates are often in the column named response.
# For LMMs, they are often in emmean.
ggplot(emm_df, aes(x = treatment, y = response)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.15) +
  facet_wrap(~ grazer_treatment) +
  labs(
    title = "Model-estimated treatment effects",
    y = "Estimated response",
    x = "Treatment"
  )


#### 12. REPORTING TEMPLATE ####

# Use this as a writing template for methods/results.
#
# Methods example:
#   We analyzed [response] using a [linear mixed model / generalized linear mixed
#   model] with [treatment] as a fixed effect and [water_level/elevation] as a
#   covariate. Because the experiment used a randomized complete block design,
#   block was included as a random intercept. For analyses spanning multiple years,
#   year and blocks nested within year were included as random effects. Count data
#   were modeled with a [Poisson / negative binomial] error distribution. Model
#   assumptions were evaluated using residual diagnostics from performance and
#   simulation-based diagnostics from DHARMa. Estimated marginal means and pairwise
#   contrasts were calculated using emmeans.
#
# Results example:
#   The selected model was [model family] because [diagnostic reason]. There was
#   [evidence/no evidence] that [treatment] affected [response] after accounting
#   for [covariate/block/year]. Estimated marginal means indicated that [describe
#   direction and magnitude], with [confidence intervals / pairwise contrasts].


#### 13. COMMON TROUBLESHOOTING NOTES ####

# Problem: model is singular.
# Try:
#   - Check replication for each random effect.
#   - Remove unsupported random slopes or overly complex random terms.
#   - Decide whether year should be fixed instead of random if there are only two years.

# Problem: Poisson model is overdispersed.
# Try:
#   - Negative binomial model: glmer.nb() or glmmTMB(..., family = nbinom2).
#   - Observation-level random effect, if appropriate.
#   - Check for zero inflation.

# Problem: residual variance increases with fitted values.
# Try:
#   - log() or sqrt() transformation for positive continuous responses.
#   - Gamma GLMM for positive continuous data.
#   - Model variance structure using packages such as nlme or glmmTMB.

# Problem: interaction is hard to interpret.
# Try:
#   - Plot raw data by groups first.
#   - Use emmeans with pairwise comparisons within levels:
#       emmeans(model, pairwise ~ treatment | grazer_treatment)
#   - Report simple effects rather than only the global interaction p-value.

# Problem: covariate differs among treatments.
# Try:
#   - Plot covariate distributions by treatment.
#   - Decide whether the covariate is a confounder, mechanism, or treatment outcome.
#   - Test treatment:covariate slopes before assuming a single ANCOVA slope.


#### 14. CHECKLIST ####

# Before finalizing analysis, answer these questions:
#   [ ] What is the experimental unit?
#   [ ] What are the fixed treatment factors?
#   [ ] What are the blocking factors?
#   [ ] Are there repeated measures on the same plot?
#   [ ] Are blocks nested in year/site?
#   [ ] Is year a fixed effect, random effect, or replicate of the whole experiment?
#   [ ] What is the response distribution?
#   [ ] Are there many zeros?
#   [ ] Is there overdispersion?
#   [ ] Are covariate slopes similar across treatment groups?
#   [ ] Did diagnostics improve for the selected model?
#   [ ] Are final estimates presented on an interpretable scale?
#   [ ] Are model choices documented clearly enough to reproduce later?

################################################################################
# End of script
################################################################################
