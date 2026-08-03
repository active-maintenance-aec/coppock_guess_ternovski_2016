# coppock_guess_ternovski_2016/maintained/table_5_study1_encouragement.R
# Output: output/table_5_study1_encouragement.csv, output/table_5_study1_encouragement.tex
# Depends on: clean_lcv.R output, helpers.R
# Description: Table 5: Study 1 effect of the tweet encouragement on subsequent tweeting
#   among petition signers, alone and interacted with the direct message arm.
source(here::here("maintained", "helpers.R"))

lcv_study_1 <- read_rds(here::here("maintained", "output", "lcv_study_1.rds"))

# No subject in the public tweet arm signed the petition, so the second stage
# runs entirely within the two direct message arms and "fol" is the reference.
signers <- lcv_study_1 |>
  filter(signed == 1) |>
  mutate(treat = droplevels(treat))

m1 <- lm_robust(tweeted ~ tweetbutton, data = signers)
m2 <- lm_robust(tweeted ~ tweetbutton * treat, data = signers)

results <- bind_rows(tidy_fit(m1, "M1"), tidy_fit(m2, "M2"))

write_csv(results, here::here("maintained", "output", "table_5_study1_encouragement.csv"))

modelsummary(
  list("Tweeted (1)" = m1, "Tweeted (2)" = m2),
  fmt = 3,
  gof_map = c("nobs", "r.squared"),
  title = "Study 1: effects of tweet encouragement.",
  notes = "Robust standard errors (HC2) in parentheses. The constant is the no-encouragement follower arm.",
  output = here::here("maintained", "output", "table_5_study1_encouragement.tex")
)
