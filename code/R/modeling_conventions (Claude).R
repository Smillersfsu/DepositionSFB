# ============================================================
# modeling_conventions.R (Claude)
# ============================================================
# Shared modeling rules for RQ1-spatial.Rmd, RQ1-temporal.qmd,
# Research_question_2_FIXED.qmd, and RQ3-variables.qmd.
#
# WHY THIS FILE EXISTS
# Across the RQ files, the same three questions kept getting re-decided
# independently in every chunk: (1) is Site fixed or random this time,
# (2) is Tran nested under Site or not, (3) which distributional family
# is "the" model. Re-deciding these per-chunk is how a genuine bug crept
# in -- (1|Tran) used alone in several places, which silently pools
# "Transect 1" across every site as if it were one physical transect,
# since Tran only ever takes the values "1"/"2" and resets at each site.
# Deciding these three things ONCE, here, in writing, means every RQ
# file can just follow the rule instead of re-litigating it, and any
# future deviation has to be an explicit, commented exception.
#
# Source this file (after setup.R) at the top of every RQ file.

# ------------------------------------------------------------------
# RULE 1 -- Fixed vs. random effects for Site / Tran / Dist
# ------------------------------------------------------------------
# a) Tran ("1"/"2") is NEVER used as a random effect on its own. It is
#    not a unique ID -- it resets at every site. Always nest it:
#       - (1|Site/Tran)  when Site is NOT already a fixed effect in the
#         same formula. Equivalent to (1|Site) + (1|Site:Tran).
#       - (1|Site:Tran)  when Site IS already a fixed effect in the
#         formula (Site can't be fixed and random at once, so this adds
#         a transect-within-site random intercept without a redundant
#         second Site term).
#    Where the research question specifically wants transect-level
#    replication that is NOT tied to Site identity, use marsh_edgeID
#    instead (built in step1_master_analysis_cleanup.Rmd) -- but see the
#    caveat logged there: marsh_edgeID is still one level per real
#    Site x Tran combination, so swapping it in for (1|Site/Tran) is a
#    relabeling, not additional independent replication. Report it as
#    an explicit model-comparison ("does the conclusion change if
#    transect isn't attributed to site"), not as a silently preferred
#    structure.
#
# b) Site as RANDOM: use when the model includes all 4 thesis sites (or
#    all sites available) and Site identity itself isn't the thing
#    being tested -- e.g. Flux ~ marsh_edge_type + (1|Site/Tran), where
#    the goal is to generalize the edge-type effect beyond these 4
#    specific sites.
#
# c) Site as FIXED: use when a model is restricted to 2 sites (e.g. the
#    USGS-vs-Thesis comparison, CorteMadera + SanPablo only, or a
#    Dist*Site interaction model) -- 2 levels is too few to estimate a
#    variance component reliably, and these comparisons usually want
#    the site-specific estimate reported explicitly anyway.
#
# Any model that departs from (b)/(c) needs a one-line comment saying
# why, right above the model call.

# ------------------------------------------------------------------
# RULE 2 -- Response distribution: one primary family per RQ
# ------------------------------------------------------------------
# Flux is strictly positive and right-skewed. The Gamma GLMM
# (family = Gamma(link = "log"), fit via glmmTMB) is the PRIMARY model
# for every RQ -- decided once, here, rather than re-argued per chunk.
#
# The Gaussian lmer() fit (usually on log(Flux)) is kept as a
# SENSITIVITY CHECK only: does the conclusion hold under a simpler,
# more familiar model family? It is not a co-equal reported result.
# Label every lmer() chunk that exists alongside a glmmTMB() version as
# "# SENSITIVITY" in a comment, and only report its summary()/anova()
# output if it disagrees with the primary model -- in which case that
# disagreement itself belongs in the write-up.
#
# Rationale (logged once, not re-derived per chunk): Shapiro-Wilk on
# raw Flux rejects normality everywhere it's been checked; the Gamma
# GLMM avoids the arbitrary log(0)/offset problem that log(Flux) has
# whenever a zero could occur, and keeps effect estimates on a directly
# interpretable multiplicative (percent-change) scale via
# emmeans(..., type = "response").
#
# Diagnostics on the PRIMARY model only (not required for every
# sensitivity refit): simulateResiduals() + plot() + testDispersion()
# from DHARMa, at minimum once per distinct fixed-effects structure.

# ------------------------------------------------------------------
# RULE 3 -- Model naming convention
# ------------------------------------------------------------------
#   <rq>_<construct>[_<temporal-variant>]_<family>
#     rq          = rq1 / rq2 / rq3
#     construct   = short description of what's being tested
#                   (edge, dist, marsharea, sitetran, orient, ...)
#     temporal-variant = month / phenperiod, omitted if not applicable
#     family      = glmm (primary) / lmer (sensitivity check)
#   e.g. rq1_edge_month_glmm, rq1_edge_month_lmer_sensitivity
#
# Existing object names in RQ1-spatial.Rmd/RQ1-temporal.qmd/
# Research_question_2_FIXED.qmd were NOT mass-renamed to this scheme --
# doing so risked silently breaking downstream emmeans()/ggplot() calls
# that reference the old names without a way to re-run and verify every
# chunk end to end. New models added going forward (including
# RQ3-variables.qmd, rebuilt fresh) follow this convention; a couple of
# accidental object-name COLLISIONS found in the existing files (a
# model silently overwritten by a same-named plot, or by a second model
# with the same name) were fixed in place with clearer names and a
# comment explaining what was lost before the fix.

# ------------------------------------------------------------------
# RULE 4 -- Zero/negative Flux handling
# ------------------------------------------------------------------
# Gamma GLMMs require Flux > 0. Filter immediately before any Gamma
# model, and report how many rows were dropped and from where (this is
# already good practice in usgs_thesis_merge.qmd and RQ2's bay_channel
# chunk -- keep doing it everywhere a new Gamma model is added).
