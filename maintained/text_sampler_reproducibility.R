# coppock_guess_ternovski_2016/maintained/text_sampler_reproducibility.R
# Output: output/text_sampler_reproducibility.csv
# Depends on: clean_lcv.R output, original/LCVsource.r, helpers.R
# Description: The computational reproducibility check for the two omnibus balance
#   p-values, which is a question about the deposit rather than about the rewrite.
#   Both are randomization inference p-values over 1,000 permuted assignments, and
#   R 3.6.0 replaced the sampler behind sample(), so an analysis from 2015 cannot be
#   re-run faithfully under the current default. RNGkind(sample.kind = "Rounding")
#   restores the sampler of the day. Study 2's permutations are drawn by the deposit's
#   own strata_ra(), sourced from LCVsource.r, so the only thing changing there is the
#   stream. Study 1's come from complete_ra(), which the deposit takes from randomizr
#   and which is therefore the installed version rather than the archive's, so the old
#   sampler is not on its own enough to recover the published value.
#
#   The rewrite itself keeps the modern sampler. It is the code we would write now,
#   the current sampler is the correct one, and where that moves a number the ground
#   truth records it as a property of the environment rather than a defect.
#
#   It fits 2,000 pairs of multinomial logits and takes about two minutes.
source(here::here("maintained", "helpers.R"))

# The deposit's own permutation function for Study 2, which is written out in
# LCVsource.r rather than taken from a package. Sourcing it is the point: a
# reimplementation would draw a different stream and could not answer the question.
source(here::here("original", "LCVsource.r"))

lcv_study_1 <- read_rds(here::here("maintained", "output", "lcv_study_1.rds"))
lcv_study_2 <- read_rds(here::here("maintained", "output", "lcv_study_2.rds"))

arm_sizes <- table(lcv_study_1$treat)

# Re-run under the sampler of 2015 ----
# on.exit is unreliable at the top level of a sourced file, and run_all.R sources
# this in the middle of a sequence, so the sampler is restored explicitly at the end.
old_kind <- RNGkind()
suppressWarnings(RNGkind(sample.kind = "Rounding"))

study_1 <- omnibus_balance_p(
  data = lcv_study_1,
  covariates = c("account_type", "num_followers", "days_on_twitter", "centrality"),
  permute = function() {
    complete_ra(N = nrow(lcv_study_1), m_each = arm_sizes,
                conditions = c("pub", "fol", "org"))
  }
)

study_2 <- omnibus_balance_p(
  data = lcv_study_2,
  covariates = c("account_type", "num_followers", "days_on_twitter", "centrality", "strata"),
  permute = function() strata_ra(stratum = lcv_study_2$strata, treat = lcv_study_2$treat)
)

# Restore the modern sampler for everything that runs after this ----
suppressWarnings(RNGkind(sample.kind = old_kind[3]))
stopifnot(RNGkind()[3] == old_kind[3])

# What the old sampler gives ----
# No published value appears here. The comparison against the appendix belongs to
# the ground truth, which reads these numbers back out of output/ and joins them
# to the printed ones.
check <- tibble(
  study = c("Study 1 (Table A2)", "Study 2 (Table A3)"),
  table_figure = c("table_a2", "table_a3"),
  permutation_source = c("randomizr::complete_ra()",
                         "strata_ra(), written out in LCVsource.r"),
  old_sampler = c(study_1$p_value, study_2$p_value),
  llr_observed = c(study_1$llr_obs, study_2$llr_obs),
  sims = c(study_1$sims, study_2$sims)
)

write_csv(check, here::here("maintained", "output", "text_sampler_reproducibility.csv"))
print(check)
