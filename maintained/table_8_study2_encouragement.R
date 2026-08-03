# coppock_guess_ternovski_2016/maintained/table_8_study2_encouragement.R
# Output: output/table_8_study2_encouragement.csv, output/table_8_study2_encouragement.tex
# Depends on: clean_lcv.R output, helpers.R
# Description: Table 8: Study 2 effect of the tweet button on subsequent tweeting among
#   petition signers, alone and interacted with the direct message arm.
source(here::here("maintained", "helpers.R"))

lcv_study_2 <- read_rds(here::here("maintained", "output", "lcv_study_2.rds"))

signers <- lcv_study_2 |>
  filter(signed == 1) |>
  mutate(treat = droplevels(treat))

m1 <- lm_robust(tweeted ~ tweetbutton, data = signers)
m2 <- lm_robust(tweeted ~ tweetbutton * treat, data = signers)

results <- bind_rows(tidy_fit(m1, "M1"), tidy_fit(m2, "M2"))

write_csv(results, here::here("maintained", "output", "table_8_study2_encouragement.csv"))

modelsummary(
  list("Tweeted (1)" = m1, "Tweeted (2)" = m2),
  fmt = 3,
  gof_map = c("nobs", "r.squared"),
  title = "Study 2: effects of the tweet button on subsequent tweets.",
  notes = "Robust standard errors (HC2) in parentheses. The constant is the no-encouragement follower arm.",
  output = here::here("maintained", "output", "table_8_study2_encouragement.tex")
)
