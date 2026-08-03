# coppock_guess_ternovski_2016/maintained/table_a3_study2_balance.R
# Output: output/table_a3_study2_balance.csv, output/table_a3_study2_balance.tex
# Depends on: clean_lcv.R output, helpers.R
# Description: Table A3: Study 2 covariate balance across the three treatment arms. Study 2
#   assigned treatment within blocks, so each covariate is tested block by block and the
#   block-level p-values are combined by Fisher's method.
source(here::here("maintained", "helpers.R"))

lcv_study_2 <- read_rds(here::here("maintained", "output", "lcv_study_2.rds"))

covariates <- c("Account type: female" = "fem_dum",
                "Account type: male" = "male_dum",
                "Account type: organization" = "org_dum",
                "Account type: unknown" = "unk_dum",
                "Number of followers" = "num_followers",
                "Days on Twitter" = "days_on_twitter",
                "Eigenvector centrality" = "centrality")

# Means and standard errors by arm ----
cells <- lcv_study_2 |>
  select(treat, all_of(unname(covariates))) |>
  pivot_longer(-treat, names_to = "covariate", values_to = "value") |>
  summarize(mean = mean(value, na.rm = TRUE), se = se_mean(value),
            .by = c(covariate, treat))

# Tests of independence, conditioned on the assignment block ----
# Fisher's exact test replaces the chi-square for account type because single
# blocks hold too few subjects for the chi-square approximation; the margins are
# fixed by design, which is the assumption the exact test needs.
p_account_type <- lcv_study_2 |>
  summarize(p = fisher.test(table(treat, account_type))$p.value, .by = strata) |>
  pull(p) |>
  fisher_combine()

p_continuous <- function(x_name) {
  lcv_study_2 |>
    summarize(p = f_test_p(.data[[x_name]], treat), .by = strata) |>
    pull(p) |>
    fisher_combine()
}

p_values <- tibble(
  covariate = c("unk_dum", "num_followers", "days_on_twitter", "centrality"),
  p_value = c(p_account_type, p_continuous("num_followers"),
              p_continuous("days_on_twitter"), p_continuous("centrality"))
)

# Omnibus test ----
# Permutes assignment within block, holding each block's arm sizes at the realised
# counts. The archive's own strata_ra() does the same thing by hand, with a typo
# that labels the third arm "ord"; block_ra() is the maintained equivalent.
block_counts <- with(lcv_study_2, table(strata, treat))

omnibus <- omnibus_balance_p(
  data = lcv_study_2,
  covariates = c("account_type", "num_followers", "days_on_twitter", "centrality", "strata"),
  permute = function() {
    block_ra(blocks = lcv_study_2$strata, block_m_each = block_counts,
             conditions = c("pub", "fol", "org"))
  }
)

table_a3 <- cells |>
  left_join(p_values, by = "covariate") |>
  mutate(label = names(covariates)[match(covariate, covariates)]) |>
  select(covariate, label, treat, mean, se, p_value) |>
  arrange(match(covariate, covariates), treat) |>
  bind_rows(
    lcv_study_2 |>
      summarize(mean = n(), .by = treat) |>
      mutate(covariate = "n", label = "N", se = NA, p_value = NA),
    tibble(covariate = "omnibus", label = "Omnibus p-value", treat = NA,
           mean = omnibus$p_value, se = NA, p_value = NA)
  )

write_csv(table_a3, here::here("maintained", "output", "table_a3_study2_balance.csv"))

table_a3 |>
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
        caption = str_glue("Study 2 balance. Omnibus p-value: {sprintf('%.3f', omnibus$p_value)}.")) |>
  write_lines(here::here("maintained", "output", "table_a3_study2_balance.tex"))
