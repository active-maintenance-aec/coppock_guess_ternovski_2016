# coppock_guess_ternovski_2016/maintained/table_7_study2_dm_effects.R
# Output: output/table_7_study2_dm_effects.csv, output/table_7_study2_dm_effects.tex
# Depends on: clean_lcv.R output, helpers.R
# Description: Table 7: Study 2 effects of the direct message treatments on signing the
#   petition and on tweeting the link, unadjusted and covariate adjusted.
source(here::here("maintained", "helpers.R"))

lcv_study_2 <- read_rds(here::here("maintained", "output", "lcv_study_2.rds"))

covariates <- "account_type + centrality_centered + num_followers_centered + days_on_twitter_centered + days_missing"

# Models ----
m1 <- lm_robust(signed ~ treat, data = lcv_study_2)
m2 <- lm_robust(as.formula(paste("signed ~ treat +", covariates)), data = lcv_study_2)
m3 <- lm_robust(tweeted ~ treat, data = lcv_study_2)
m4 <- lm_robust(as.formula(paste("tweeted ~ treat +", covariates)), data = lcv_study_2)

# Follower against organizer ----
dm_only <- lcv_study_2 |>
  filter(treat != "pub") |>
  mutate(treat = droplevels(treat))

diff_signed <- lm_robust(signed ~ treat, data = dm_only)
diff_tweeted <- lm_robust(tweeted ~ treat, data = dm_only)

results <- bind_rows(
  tidy_fit(m1, "M1 signed"),
  tidy_fit(m2, "M2 signed adjusted"),
  tidy_fit(m3, "M3 tweeted"),
  tidy_fit(m4, "M4 tweeted adjusted"),
  tidy_fit(diff_signed, "Follower vs organizer, signed"),
  tidy_fit(diff_tweeted, "Follower vs organizer, tweeted")
)

write_csv(results, here::here("maintained", "output", "table_7_study2_dm_effects.csv"))

modelsummary(
  list("Signed (1)" = m1, "Signed (2)" = m2, "Tweeted (3)" = m3, "Tweeted (4)" = m4),
  fmt = 3,
  gof_map = c("nobs", "r.squared"),
  title = "Study 2: effects of direct message treatments on participation and tweeting.",
  notes = "Robust standard errors (HC2) in parentheses. Eigenvector centrality, number of followers and days on Twitter in standard units and centred at zero.",
  output = here::here("maintained", "output", "table_7_study2_dm_effects.tex")
)
