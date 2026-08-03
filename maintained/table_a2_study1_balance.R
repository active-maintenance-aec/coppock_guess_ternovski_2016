# coppock_guess_ternovski_2016/maintained/table_a2_study1_balance.R
# Output: output/table_a2_study1_balance.csv, output/table_a2_study1_balance.tex
# Depends on: clean_lcv.R output, helpers.R
# Description: Table A2: Study 1 covariate balance across the three treatment arms, the
#   per-covariate tests of independence, and the omnibus randomization inference test.
#   Study 1 used complete random assignment, so the tests are unconditional.
source(here::here("maintained", "helpers.R"))

lcv_study_1 <- read_rds(here::here("maintained", "output", "lcv_study_1.rds"))

covariates <- c("Account type: female" = "fem_dum",
                "Account type: male" = "male_dum",
                "Account type: organization" = "org_dum",
                "Account type: unknown" = "unk_dum",
                "Number of followers" = "num_followers",
                "Days on Twitter" = "days_on_twitter",
                "Eigenvector centrality" = "centrality")

# Means and standard errors by arm ----
cells <- lcv_study_1 |>
  select(treat, all_of(unname(covariates))) |>
  pivot_longer(-treat, names_to = "covariate", values_to = "value") |>
  summarize(mean = mean(value, na.rm = TRUE), se = se_mean(value),
            .by = c(covariate, treat))

# Tests of independence ----
# The four account type indicators are one categorical covariate, so they share a
# single chi-square test, reported on the last of the four rows as in the article.
p_account_type <- chisq.test(table(lcv_study_1$account_type, lcv_study_1$treat))$p.value

p_values <- tibble(
  covariate = c("unk_dum", "num_followers", "days_on_twitter", "centrality"),
  p_value = c(
    p_account_type,
    f_test_p(lcv_study_1$num_followers, lcv_study_1$treat),
    f_test_p(lcv_study_1$days_on_twitter, lcv_study_1$treat),
    f_test_p(lcv_study_1$centrality, lcv_study_1$treat)
  )
)

# Omnibus test ----
# Permutes assignment 1,000 times under complete random assignment with the
# realised arm sizes, exactly the protocol the study used.
arm_sizes <- table(lcv_study_1$treat)
omnibus <- omnibus_balance_p(
  data = lcv_study_1,
  covariates = c("account_type", "num_followers", "days_on_twitter", "centrality"),
  permute = function() {
    complete_ra(N = nrow(lcv_study_1), m_each = arm_sizes,
                conditions = c("pub", "fol", "org"))
  }
)

table_a2 <- cells |>
  left_join(p_values, by = "covariate") |>
  mutate(label = names(covariates)[match(covariate, covariates)]) |>
  select(covariate, label, treat, mean, se, p_value) |>
  arrange(match(covariate, covariates), treat) |>
  bind_rows(
    lcv_study_1 |>
      summarize(mean = n(), .by = treat) |>
      mutate(covariate = "n", label = "N", se = NA, p_value = NA),
    tibble(covariate = "omnibus", label = "Omnibus p-value", treat = NA,
           mean = omnibus$p_value, se = NA, p_value = NA)
  )

write_csv(table_a2, here::here("maintained", "output", "table_a2_study1_balance.csv"))

table_a2 |>
  filter(covariate != "omnibus") |>
  mutate(entry = if_else(covariate == "n",
                         format(mean, big.mark = ""),
                         str_glue("{sprintf('%.3f', mean)} ({sprintf('%.3f', se)})"))) |>
  select(label, treat, entry, p_value) |>
  pivot_wider(names_from = treat, values_from = entry) |>
  mutate(p_value = if_else(is.na(p_value), "", sprintf("%.3f", p_value))) |>
  select(label, pub, fol, org, p_value) |>
  kable(format = "latex", booktabs = TRUE,
        col.names = c("", "Public tweet", "Follower", "Organizer", "p-value"),
        caption = str_glue("Study 1 balance. Omnibus p-value: {sprintf('%.3f', omnibus$p_value)}.")) |>
  write_lines(here::here("maintained", "output", "table_a2_study1_balance.tex"))
