# coppock_guess_ternovski_2016/maintained/table_6_study1_network.R
# Output: output/table_6_study1_network.csv, output/table_6_study1_network.tex
# Depends on: clean_lcv.R output, helpers.R
# Description: Table 6: Study 1 effects of the tweet encouragement on the subjects'
#   own followers. First stage, intent-to-treat effects and complier average causal
#   effects, all weighted by the inverse probability of exposure.
source(here::here("maintained", "helpers.R"))

lcv_study_1 <- read_rds(here::here("maintained", "output", "lcv_study_1.rds"))

# The second-stage subjects are the users whose exposure was not determined by
# design: they followed at least one petition signer but not all of them.
exposed <- filter(lcv_study_1, probexposure > 0, probexposure < 1)

m1 <- lm_robust(actual_exposure ~ exposure, weights = weights, data = exposed)
m2 <- lm_robust(signed ~ exposure, weights = weights, data = exposed)
m3 <- iv_robust(signed ~ actual_exposure | exposure, weights = weights, data = exposed)
m4 <- lm_robust(tweeted ~ exposure, weights = weights, data = exposed)
m5 <- iv_robust(tweeted ~ actual_exposure | exposure, weights = weights, data = exposed)

results <- bind_rows(
  tidy_fit(m1, "M1 shown tweet, OLS"),
  tidy_fit(m2, "M2 signed, OLS"),
  tidy_fit(m3, "M3 signed, IV"),
  tidy_fit(m4, "M4 tweeted, OLS"),
  tidy_fit(m5, "M5 tweeted, IV")
)

write_csv(results, here::here("maintained", "output", "table_6_study1_network.csv"))

modelsummary(
  list("Shown tweet OLS (1)" = m1, "Signed OLS (2)" = m2, "Signed IV (3)" = m3,
       "Tweeted OLS (4)" = m4, "Tweeted IV (5)" = m5),
  fmt = 3,
  gof_map = c("nobs", "r.squared"),
  title = "Study 1: effects of tweet encouragement on subjects' followers.",
  notes = "Robust standard errors (HC2) in parentheses. All regressions weighted by the inverse probability of exposure.",
  output = here::here("maintained", "output", "table_6_study1_network.tex")
)
