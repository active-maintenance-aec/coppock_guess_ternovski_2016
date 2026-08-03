# coppock_guess_ternovski_2016/maintained/clean_lcv.R
# Output: output/lcv_study_1.rds, output/lcv_study_2.rds
# Depends on: original/LCVexp1.rdata, original/LCVexp2.rdata, helpers.R
# Description: Turn the two deposited data frames into analysis-ready tibbles: explicit
#   factor levels for treatment and account type, and the centred covariates flattened
#   from single-column matrices to plain numeric vectors.
source(here::here("maintained", "helpers.R"))

load(here::here("original", "LCVexp1.rdata"))
load(here::here("original", "LCVexp2.rdata"))

# Shared recoding ----
# treat already carries the levels pub, fol, org in the deposit, so "pub" is the
# reference category in every regression, matching the published tables.
# account_type arrives as a character vector, which lm() would order
# alphabetically; naming the levels makes "female" the reference explicitly.
# scale() leaves its results as one-column matrices, which propagate a dim
# attribute through every model frame that touches them.
lcv_study_1 <- LCVexp1 |>
  as_tibble() |>
  mutate(
    treat = factor(treat, levels = c("pub", "fol", "org")),
    account_type = factor(account_type, levels = c("female", "male", "org", "unknown")),
    centrality_centered = as.numeric(centrality_centered),
    num_followers_centered = as.numeric(num_followers_centered),
    days_on_twitter_centered = as.numeric(days_on_twitter_centered)
  )

lcv_study_2 <- LCVexp2 |>
  as_tibble() |>
  mutate(
    treat = factor(treat, levels = c("pub", "fol", "org")),
    account_type = factor(account_type, levels = c("female", "male", "org", "unknown")),
    centrality_centered = as.numeric(centrality_centered),
    num_followers_centered = as.numeric(num_followers_centered),
    days_on_twitter_centered = as.numeric(days_on_twitter_centered)
  )

write_rds(lcv_study_1, here::here("maintained", "output", "lcv_study_1.rds"))
write_rds(lcv_study_2, here::here("maintained", "output", "lcv_study_2.rds"))
