# coppock_guess_ternovski_2016/maintained/table_a5_study2_heterogeneity.R
# Output: output/table_a5_study2_heterogeneity.csv, output/table_a5_study2_heterogeneity.tex
# Depends on: clean_lcv.R output, helpers.R
# Description: Table A5: Study 2 heterogeneous effects of the direct message treatments,
#   one model per moderator (eigenvector centrality, number of followers, days on Twitter
#   and account type) for each of the two outcomes.
source(here::here("maintained", "helpers.R"))

lcv_study_2 <- read_rds(here::here("maintained", "output", "lcv_study_2.rds"))

moderators <- c("centrality_centered", "num_followers_centered",
                "days_on_twitter_centered", "account_type")

# Models ----
# Each fit is treatment, one moderator, and their interaction. Nobody in the public
# tweet arm signed or tweeted, so the moderator main effects are estimated on an
# arm with no outcome variation. They come back as floating point noise around zero
# with a robust variance that is zero up to rounding and so a standard error of NaN;
# that is a property of the design rather than a fitting failure.
fits <- expand_grid(dv = c("signed", "tweeted"), moderator = moderators) |>
  mutate(
    label = str_glue("{dv} x {moderator}"),
    fit = map2(dv, moderator, \(y, m) lm_robust(
      as.formula(str_glue("{y} ~ treat * {m}")), data = lcv_study_2))
  )

results <- map2(fits$fit, fits$label, tidy_fit) |> list_rbind()

write_csv(results, here::here("maintained", "output", "table_a5_study2_heterogeneity.csv"))

modelsummary(
  set_names(fits$fit, paste0("(", seq_len(nrow(fits)), ")")),
  fmt = 3,
  gof_map = c("nobs", "r.squared"),
  title = "Study 2: heterogeneous effects of treatments. Columns 1 to 4 take signing as the outcome, columns 5 to 8 tweeting.",
  notes = "Robust standard errors (HC2) in parentheses. Eigenvector centrality, number of followers and days on Twitter in standard units and centred at zero.",
  output = here::here("maintained", "output", "table_a5_study2_heterogeneity.tex")
)
