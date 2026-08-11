# coppock_guess_ternovski_2016/ground_truth/build_ground_truth.R
# Output: ground_truth/coppock_guess_ternovski_2016_ground_truth.csv
# Depends on: ground_truth/published_claims.csv, ground_truth/archive_values.csv,
#   ground_truth/archive_figure_3_values.csv, maintained/output/,
#   maintained/in_text_claims.R (read for its coverage markers, not run)
# Description: Assemble the ground truth table, which pairs every published claim with
#   the value the deposited code produces and the value the maintained rewrite produces.
#
#   The three columns come from three separate places and none of them is typed here.
#   value_paper is read from ground_truth/published_claims.csv, the hand-reviewed
#   extraction of every numeric token in the published article and its online appendix;
#   every entry was transcribed from a rendered page and from nothing else, and where
#   the article does not print a quantity the row carries no value and match is NA.
#   value_script is read from ground_truth/archive_values.csv, which
#   extract_archive_values.R writes by running the deposit in a scratch copy and reading
#   its own fitted models. value_rewrite is read out of maintained/output/. Run
#   run_all.R, which regenerates both inputs immediately before this file.
#
#   No published number is an input to any computation, here or in maintained/.

library(here)
library(tidyverse)

here::i_am("ground_truth/build_ground_truth.R")

out <- function(f) read_csv(here::here("maintained", "output", f), show_col_types = FALSE)
gt_file <- function(f) read_csv(here::here("ground_truth", f), show_col_types = FALSE)

# Published claims ----
# claim_id is the join key written out: float, row and quantity separated by bars. The
# extraction carries every numeric token in the article, so it holds claims no code can
# reach as well as claims the pipeline computes, and claim_type says which is which.
claims <- read_csv(here::here("ground_truth", "published_claims.csv"), col_types = "cccc")

paper <- claims |>
  separate_wider_delim(claim_id, delim = "|",
                       names = c("table_figure", "row_label", "quantity"),
                       cols_remove = FALSE) |>
  select(claim_id, table_figure, row_label, quantity, location, claim_type, value_paper)

stopifnot(!anyDuplicated(paper$claim_id),
          all(paper$claim_type %in% c("pipeline", "definitional", "structural",
                                      "transcribed", "descriptive")))

# What the deposit produces ----
script <- gt_file("archive_values.csv")
figure_3_archive <- gt_file("archive_figure_3_values.csv")

# One join helper ----
# Every join below pairs a published row with a computed one, and the failure that
# matters is silent: a mislabelled key finds nothing, the value arrives as NA, and the
# row lands in the unverifiable bucket beside quantities the pipeline genuinely cannot
# produce. Uniqueness on both sides, no unmatched left row, and a preserved row count.
join_one <- function(left, right, by, what) {
  stopifnot(!anyDuplicated(left[by]), !anyDuplicated(right[by]))
  unmatched <- anti_join(left, right, by = by)
  if (nrow(unmatched) > 0) {
    print(unmatched, n = 50)
    stop(nrow(unmatched), " published rows have no ", what, " counterpart")
  }
  joined <- left_join(left, right, by = by)
  stopifnot(nrow(joined) == nrow(left))
  joined
}

# The published claims include quantities no code in the deposit can reach, so this
# join has to allow an unmatched left row. What it may not allow is a duplicated key or
# a changed row count, and an unmatched row that is not accounted for further down is
# caught by the coverage gate at the end rather than here.
join_keep <- function(left, right, by) {
  stopifnot(!anyDuplicated(left[by]), !anyDuplicated(right[by]))
  joined <- left_join(left, right, by = by)
  stopifnot(nrow(joined) == nrow(left))
  joined
}

# What the maintained rewrite produced ----
arms_2 <- c("Public tweet" = "pub",
            "Organizer DM: tweet encouragement" = "org_enc",
            "Organizer DM: no encouragement" = "org_noenc",
            "Follower DM: tweet encouragement" = "fol_enc",
            "Follower DM: no encouragement" = "fol_noenc",
            "Total" = "total")
arms_3 <- c("Public tweet" = "pub", "Organizer DM" = "org", "Follower DM" = "fol",
            "Total" = "total", "Signers: tweet encouragement" = "signers_enc",
            "Signers: no encouragement" = "signers_noenc", "Signers: total" = "signers_total")

rewrite_descriptive <- function(file, table_figure, arm_map) {
  out(file) |>
    mutate(row_label = unname(arm_map[arm])) |>
    select(row_label, n, n_signed, pct_signed, n_tweeted, pct_tweeted) |>
    pivot_longer(-row_label, names_to = "quantity", values_to = "value_rewrite") |>
    mutate(table_figure = table_figure)
}

rewrite_regression <- function(file, table_figure, model_map, term_map) {
  d <- out(file) |>
    filter(model %in% names(model_map), term %in% names(term_map)) |>
    mutate(m = unname(model_map[model]), g = unname(term_map[term]))
  bind_rows(
    transmute(d, table_figure, row_label = paste0(m, ":", g), quantity = "coef",
              value_rewrite = estimate),
    transmute(d, table_figure, row_label = paste0(m, ":", g), quantity = "se",
              value_rewrite = std.error),
    distinct(d, m, nobs, r_squared) |>
      pivot_longer(c(nobs, r_squared), names_to = "quantity", values_to = "value_rewrite") |>
      transmute(table_figure, row_label = m,
                quantity = if_else(quantity == "nobs", "n", "r2"), value_rewrite)
  )
}

terms_main_rewrite <- c("(Intercept)" = "constant", "treatfol" = "treat_fol",
                        "treatorg" = "treat_org", "account_typemale" = "account_male",
                        "account_typeorg" = "account_org",
                        "account_typeunknown" = "account_unknown",
                        "centrality_centered" = "centrality",
                        "num_followers_centered" = "followers",
                        "days_on_twitter_centered" = "days",
                        "days_missingTRUE" = "days_missing")
models_main <- c("M1 signed" = "m1", "M2 signed adjusted" = "m2",
                 "M3 tweeted" = "m3", "M4 tweeted adjusted" = "m4")

terms_button_rewrite <- c("(Intercept)" = "constant", "tweetbutton" = "encouragement",
                          "treatorg" = "treat_org",
                          "tweetbutton:treatorg" = "encouragement_x_org")
models_button <- c("M1" = "m1", "M2" = "m2")

terms_network_rewrite <- c("(Intercept)" = "constant", "exposure" = "exposure",
                           "actual_exposure" = "actual_exposure")
models_network <- c("M1 shown tweet, OLS" = "m1", "M2 signed, OLS" = "m2",
                    "M3 signed, IV" = "m3", "M4 tweeted, OLS" = "m4",
                    "M5 tweeted, IV" = "m5")

terms_heterogeneity_rewrite <- c(
  "(Intercept)" = "constant", "treatfol" = "treat_fol", "treatorg" = "treat_org",
  "centrality_centered" = "centrality",
  "treatfol:centrality_centered" = "fol_x_centrality",
  "treatorg:centrality_centered" = "org_x_centrality",
  "num_followers_centered" = "followers",
  "treatfol:num_followers_centered" = "fol_x_followers",
  "treatorg:num_followers_centered" = "org_x_followers",
  "days_on_twitter_centered" = "days",
  "treatfol:days_on_twitter_centered" = "fol_x_days",
  "treatorg:days_on_twitter_centered" = "org_x_days",
  "account_typemale" = "account_male", "account_typeorg" = "account_org",
  "account_typeunknown" = "account_unknown",
  "treatfol:account_typemale" = "fol_x_male", "treatorg:account_typemale" = "org_x_male",
  "treatfol:account_typeorg" = "fol_x_org", "treatorg:account_typeorg" = "org_x_org",
  "treatfol:account_typeunknown" = "fol_x_unknown",
  "treatorg:account_typeunknown" = "org_x_unknown")
models_heterogeneity <- set_names(
  paste0("m", 1:8),
  c("signed x centrality_centered", "signed x num_followers_centered",
    "signed x days_on_twitter_centered", "signed x account_type",
    "tweeted x centrality_centered", "tweeted x num_followers_centered",
    "tweeted x days_on_twitter_centered", "tweeted x account_type"))

balance_covariates <- c(account_female = "fem_dum", account_male = "male_dum",
                        account_org = "org_dum", account_unknown = "unk_dum",
                        num_followers = "num_followers", days_on_twitter = "days_on_twitter",
                        centrality = "centrality")

rewrite_balance <- function(file, table_figure) {
  d <- out(file)
  cells <- d |>
    filter(covariate %in% balance_covariates) |>
    mutate(row_label = paste0(names(balance_covariates)[match(covariate, balance_covariates)],
                              ":", treat)) |>
    select(row_label, mean, se) |>
    pivot_longer(-row_label, names_to = "quantity", values_to = "value_rewrite") |>
    mutate(table_figure = table_figure)
  p_labels <- c(unk_dum = "account_type", num_followers = "num_followers",
                days_on_twitter = "days_on_twitter", centrality = "centrality")
  ps <- d |>
    filter(covariate %in% names(p_labels), !is.na(p_value)) |>
    distinct(covariate, p_value) |>
    transmute(table_figure, row_label = unname(p_labels[covariate]), quantity = "p_value",
              value_rewrite = p_value)
  ns <- d |>
    filter(covariate == "n") |>
    transmute(table_figure, row_label = treat, quantity = "n", value_rewrite = mean)
  omnibus <- d |>
    filter(covariate == "omnibus") |>
    transmute(table_figure, row_label = "omnibus", quantity = "p_value",
              value_rewrite = mean)
  bind_rows(cells, ps, ns, omnibus)
}

table_4_out <- out("table_4_study1_dm_effects.csv")
table_6_out <- out("table_6_study1_network.csv")
table_7_out <- out("table_7_study2_dm_effects.csv")
table_8_out <- out("table_8_study2_encouragement.csv")
table_9_out <- out("table_9_study2_network.csv")
effects_out <- out("text_treatment_effects.csv")
summary_out <- out("text_summary_stats.csv")

difference_p_rewrite <- function(d, model) {
  d |> filter(model == !!model, term == "treatorg") |> pull(p.value)
}
effect_rewrite <- function(study, pattern) {
  effects_out |> filter(study == !!study, str_detect(quantity, pattern)) |> pull(estimate)
}
summary_rewrite <- function(study, pattern) {
  summary_out |> filter(study == !!study, str_detect(quantity, pattern)) |> pull(value)
}

rewrite_text <- tribble(
  ~table_figure, ~row_label, ~quantity, ~value_rewrite,
  "text", "study1_p_fol_vs_org_signed", "p_value",
    difference_p_rewrite(table_4_out, "Follower vs organizer, signed"),
  "text", "study1_p_fol_vs_org_tweeted", "p_value",
    difference_p_rewrite(table_4_out, "Follower vs organizer, tweeted"),
  "text", "study2_p_fol_vs_org_signed", "p_value",
    difference_p_rewrite(table_7_out, "Follower vs organizer, signed"),
  "text", "study1_any_dm_signed_pp", "estimate",
    effect_rewrite("Study 1", "any direct message"),
  "text", "study1_org_minus_fol_tweeted_pp", "estimate",
    abs(effect_rewrite("Study 1", "Organizer minus follower")),
  "text", "study1_first_stage_pp", "estimate",
    100 * (table_6_out |> filter(model == "M1 shown tweet, OLS", term == "exposure") |>
             pull(estimate)),
  "text", "study2_encouragement_fol_pp", "estimate",
    effect_rewrite("Study 2", "in the fol arm"),
  "text", "study2_encouragement_org_pp", "estimate",
    effect_rewrite("Study 2", "in the org arm"),
  "text", "study1_button_imbalance_p", "p_value",
    effects_out |> filter(study == "Study 1",
                          str_detect(quantity, "assignment predicting signing")) |>
      pull(p_value),
  "text", "study1_signers", "n", summary_rewrite("Study 1", "Petition signers"),
  "text", "study1_second_stage_pool", "n",
    summary_rewrite("Study 1", "positive exposure probability"),
  "text", "study2_signers", "n", summary_rewrite("Study 2", "Petition signers"),
  "text", "study2_second_stage_pool", "n",
    summary_rewrite("Study 2", "positive exposure probability"),
  "text", "footnote_11_zero_clicks", "percent",
    summary_rewrite("Study 1", "zero clicks"),
  "figure_3", "study1_share_male", "percent", summary_rewrite("Study 1", "sample: male"),
  "figure_3", "study1_share_female", "percent", summary_rewrite("Study 1", "sample: female"),
  "figure_3", "study1_share_org", "percent", summary_rewrite("Study 1", "sample: org"),
  "figure_3", "study1_share_unknown", "percent",
    summary_rewrite("Study 1", "sample: unknown"),
  "figure_3", "study2_share_male", "percent", summary_rewrite("Study 2", "sample: male"),
  "figure_3", "study2_share_female", "percent", summary_rewrite("Study 2", "sample: female"),
  "figure_3", "study2_share_org", "percent", summary_rewrite("Study 2", "sample: org"),
  "figure_3", "study2_share_unknown", "percent",
    summary_rewrite("Study 2", "sample: unknown"),
  "table_a1", "study1_types_5_to_7", "percent", summary_rewrite("Study 1", "Types 5 to 7"),
  "table_a1", "study2_types_5_to_7", "percent", summary_rewrite("Study 2", "Types 5 to 7"),
  "table_a1", "type_8_lower", "proportion", summary_rewrite("Study 2", "Type 8"),
  "table_a1", "type_8_upper", "proportion", summary_rewrite("Study 1", "Type 8"),
  "text", "study1_network_members", "n", summary_rewrite("Study 1", "^Network members$"),
  "text", "study2_network_members", "n", summary_rewrite("Study 2", "^Network members$"),
  "figure_1", "network_members", "n", summary_rewrite("Study 1", "^Network members$"),
  "text", "study1_tweets_total", "n", summary_rewrite("Study 1", "^Tweets of the link"),
  "text", "study1_fol_signed_pp", "estimate",
    100 * (table_4_out |> filter(model == "M1 signed", term == "treatfol") |> pull(estimate)),
  "text", "study1_org_signed_pp", "estimate",
    100 * (table_4_out |> filter(model == "M1 signed", term == "treatorg") |> pull(estimate)),
  "text", "study1_fol_tweeted_pp", "estimate",
    100 * (table_4_out |> filter(model == "M3 tweeted", term == "treatfol") |> pull(estimate)),
  "text", "study2_fol_signed_pp", "estimate",
    100 * (table_7_out |> filter(model == "M1 signed", term == "treatfol") |> pull(estimate)),
  "text", "study2_org_signed_pp", "estimate",
    100 * (table_7_out |> filter(model == "M1 signed", term == "treatorg") |> pull(estimate)),
  "text", "study2_button_tweeted_pp", "estimate",
    100 * (table_8_out |> filter(model == "M1", term == "tweetbutton") |> pull(estimate)),
  "text", "study2_itt_signed_pp", "estimate",
    100 * (table_9_out |> filter(model == "M2 signed, OLS", term == "exposure") |>
             pull(estimate)),
  "text", "study2_cace_signed_pp", "estimate",
    100 * (table_9_out |> filter(model == "M3 signed, IV", term == "actual_exposure") |>
             pull(estimate))
)

# The qualitative claims, as truth values ----
# A descriptive claim carries value_paper = 1, meaning the article asserts it holds, so
# a computed 1 matches and a computed 0 is the article contradicting its own estimates.
rewrite_descriptive_claims <- out("text_descriptive_claims.csv") |>
  separate_wider_delim(claim_id, delim = "|",
                       names = c("table_figure", "row_label", "quantity")) |>
  transmute(table_figure, row_label, quantity, value_rewrite = as.numeric(holds))

rewrite <- bind_rows(
  rewrite_descriptive("table_2_study1_outcomes.csv", "table_2", arms_2),
  rewrite_descriptive("table_3_study2_outcomes.csv", "table_3", arms_3),
  rewrite_regression("table_4_study1_dm_effects.csv", "table_4",
                     models_main, terms_main_rewrite),
  rewrite_regression("table_5_study1_encouragement.csv", "table_5",
                     models_button, terms_button_rewrite),
  rewrite_regression("table_6_study1_network.csv", "table_6",
                     models_network, terms_network_rewrite),
  rewrite_regression("table_7_study2_dm_effects.csv", "table_7",
                     models_main, terms_main_rewrite),
  rewrite_regression("table_8_study2_encouragement.csv", "table_8",
                     models_button, terms_button_rewrite),
  rewrite_regression("table_9_study2_network.csv", "table_9",
                     models_network, terms_network_rewrite),
  rewrite_regression("table_a4_study1_heterogeneity.csv", "table_a4",
                     models_heterogeneity, terms_heterogeneity_rewrite),
  rewrite_regression("table_a5_study2_heterogeneity.csv", "table_a5",
                     models_heterogeneity, terms_heterogeneity_rewrite),
  rewrite_balance("table_a2_study1_balance.csv", "table_a2"),
  rewrite_balance("table_a3_study2_balance.csv", "table_a3"),
  rewrite_text,
  rewrite_descriptive_claims
)

# Readable claim labels ----
quantity_labels <- c(coef = "coefficient", se = "standard error", n = "N",
                     n_signed = "number signing", n_tweeted = "number tweeting",
                     pct_signed = "percent signing", pct_tweeted = "percent tweeting",
                     r2 = "R squared", mean = "mean", p_value = "p-value",
                     estimate = "estimate", percent = "percent",
                     proportion = "proportion")

term_labels <- c(treat_fol = "Treatment: follower", treat_org = "Treatment: organizer",
                 account_male = "Account type: male",
                 account_org = "Account type: organization",
                 account_unknown = "Account type: unknown",
                 centrality = "Eigenvector centrality", followers = "Number of followers",
                 days = "Days on Twitter", days_missing = "Days on Twitter missing",
                 constant = "Constant", encouragement = "Shown tweet encouragement",
                 encouragement_x_org = "Encouragement x organizer",
                 exposure = "Exposure: followed subject shown tweet encouragement",
                 actual_exposure = "Exposure: followed subject tweeted",
                 fol_x_centrality = "Follower x centrality",
                 org_x_centrality = "Organizer x centrality",
                 fol_x_followers = "Follower x followers",
                 org_x_followers = "Organizer x followers",
                 fol_x_days = "Follower x days on Twitter",
                 org_x_days = "Organizer x days on Twitter",
                 fol_x_male = "Follower x male", org_x_male = "Organizer x male",
                 fol_x_org = "Follower x organization",
                 org_x_org = "Organizer x organization",
                 fol_x_unknown = "Follower x unknown",
                 org_x_unknown = "Organizer x unknown",
                 account_female = "Account type: female",
                 num_followers = "Number of followers",
                 days_on_twitter = "Days on Twitter", account_type = "Account type",
                 pub = "Public tweet", fol = "Follower DM", org = "Organizer DM",
                 omnibus = "Omnibus", pct_signed = "percent signing")

arm_labels <- c(pub = "Public tweet", fol = "Follower DM", org = "Organizer DM",
                org_enc = "Organizer DM, tweet encouragement",
                org_noenc = "Organizer DM, no encouragement",
                fol_enc = "Follower DM, tweet encouragement",
                fol_noenc = "Follower DM, no encouragement", total = "Total",
                signers_enc = "Signers, tweet encouragement",
                signers_noenc = "Signers, no encouragement",
                signers_total = "Signers, total")

model_labels <- list(
  table_4 = c(m1 = "Column 1 (signed)", m2 = "Column 2 (signed, adjusted)",
              m3 = "Column 3 (tweeted)", m4 = "Column 4 (tweeted, adjusted)"),
  table_5 = c(m1 = "Column 1", m2 = "Column 2"),
  table_6 = c(m1 = "Column 1 (shown tweet, OLS)", m2 = "Column 2 (signed, OLS)",
              m3 = "Column 3 (signed, IV)", m4 = "Column 4 (tweeted, OLS)",
              m5 = "Column 5 (tweeted, IV)"),
  table_a4 = c(m1 = "Column 1 (signed, centrality)", m2 = "Column 2 (signed, followers)",
               m3 = "Column 3 (signed, days on Twitter)",
               m4 = "Column 4 (signed, account type)",
               m5 = "Column 5 (tweeted, centrality)", m6 = "Column 6 (tweeted, followers)",
               m7 = "Column 7 (tweeted, days on Twitter)",
               m8 = "Column 8 (tweeted, account type)")
)
model_labels$table_7 <- model_labels$table_4
model_labels$table_8 <- model_labels$table_5
model_labels$table_9 <- model_labels$table_6
model_labels$table_a5 <- model_labels$table_a4

# Labels for every claim that is not a cell of a regression or balance table ----
# Keyed by claim_id rather than by row label, because two different floats carry a
# quantity of the same name: Table 1's mean tweets per weekday among the top ten
# organizations and Figure 2's mean for the LCV account itself.
claim_labels <- c(
  "text|study1_p_fol_vs_org_signed|p_value" = "Study 1: p-value on the follower against organizer difference in signing",
  "text|study1_p_fol_vs_org_tweeted|p_value" = "Study 1: p-value on the follower against organizer difference in tweeting",
  "text|study2_p_fol_vs_org_signed|p_value" = "Study 2: p-value on the follower against organizer difference in signing",
  "text|study1_any_dm_signed_pp|estimate" = "Study 1: effect of receiving any direct message on signing, percentage points",
  "text|study1_org_minus_fol_tweeted_pp|estimate" = "Study 1: organizer against follower gap in tweeting, percentage points",
  "text|study1_first_stage_pp|estimate" = "Study 1: first stage, exposure to a retweeted message, percentage points",
  "text|study2_encouragement_fol_pp|estimate" = "Study 2: tweet encouragement effect in the follower arm, percentage points",
  "text|study2_encouragement_org_pp|estimate" = "Study 2: tweet encouragement effect in the organizer arm, percentage points",
  "text|study1_button_imbalance_p|p_value" = "Study 1: p-value on the tweet encouragement assignment predicting signing",
  "text|study1_signers|n" = "Study 1: petition signatures",
  "text|study1_second_stage_pool|n" = "Study 1: subjects of the second-stage experiment",
  "text|study2_signers|n" = "Study 2: petition signatures",
  "text|study2_second_stage_pool|n" = "Study 2: subjects of the second-stage experiment",
  "text|footnote_11_zero_clicks|percent" = "Footnote 11: chance of observing zero clicks at one click per 10,000 exposed, percent",
  "text|study1_network_members|n" = "Study 1: members of the scraped network",
  "text|study2_network_members|n" = "Study 2: members of the scraped network",
  "text|study1_tweets_total|n" = "Study 1: tweets of the petition link throughout the network",
  "text|study1_fol_signed_pp|estimate" = "Study 1: follower direct message effect on signing, percentage points",
  "text|study1_org_signed_pp|estimate" = "Study 1: organizer direct message effect on signing, percentage points",
  "text|study1_fol_tweeted_pp|estimate" = "Study 1: follower direct message effect on tweeting, percentage points",
  "text|study2_fol_signed_pp|estimate" = "Study 2: follower direct message effect on signing, percentage points",
  "text|study2_org_signed_pp|estimate" = "Study 2: organizer direct message effect on signing, percentage points",
  "text|study2_button_tweeted_pp|estimate" = "Study 2: tweet encouragement effect on tweeting, restricted model, percentage points",
  "text|study2_itt_signed_pp|estimate" = "Study 2: intent-to-treat effect of exposure on signing, percentage points",
  "text|study2_cace_signed_pp|estimate" = "Study 2: complier average causal effect of exposure on signing, percentage points",
  "text|intercoder_kappa|value" = "Intercoder reliability of the account type coding, Cohen's kappa",
  "text|study1_outside_retweeters|n" = "Study 1: users outside the LCV network who retweeted",
  "text|study2_outside_retweeters|n" = "Study 2: users outside the LCV network who retweeted",
  "text|lcv_follower_followers|n" = "Total followers of the LCV's followers",
  "text|twitter_char_limit|n" = "Twitter's character limit on a tweet",
  "text|study1_follower_cap|n" = "Follower count above which users were excluded from the sample",
  "text|twitter_dm_limit|n" = "Twitter's API limit on direct messages per application per day",
  "text|study1_dm_batches|n" = "Study 1: daily batches the direct messages were sent in",
  "text|study2_dm_batches|n" = "Study 2: batches the direct messages were sent in",
  "text|n_treatment_conditions|n" = "Treatment conditions a subject could be assigned to",
  "text|study2_months_later|n" = "Months between the two studies",
  "text|ci_level|percent" = "Confidence level of the intervals plotted in Figure 3",
  "text|intercoder_sample|n" = "Profiles double-coded for the intercoder reliability check",
  "text|omnibus_permutations|n" = "Permutations drawn for each omnibus balance test",
  "text|projected_dms_per_day|n" = "Discussion illustration: direct messages per day",
  "text|projected_days|n" = "Discussion illustration: days",
  "text|projected_signatures|n" = "Discussion illustration: signatures over a month",
  "table_1|cells|count" = "Descriptive statistics of eleven environmental organizations' Twitter accounts",
  "table_1|mean_followers_top_ten|n" = "Mean number of followers among the top ten organizations",
  "table_1|mean_tweets_per_weekday|n" = "Mean tweets per weekday among the top ten organizations",
  "table_1|median_creation_year|year" = "Median account creation year among the top ten organizations",
  "figure_1|network_members|n" = "Figure 1 caption: followers illustrated in the Study 1 network",
  "figure_1|communities|n" = "Communities in the Study 1 follower network under the Walktrap algorithm",
  "figure_1|modularity|value" = "Modularity of the Study 1 follower network",
  "figure_1|possible_connections|n" = "Possible connections between nodes in the Study 1 network",
  "figure_1|observed_edges|n" = "Observed edges in the Study 1 network",
  "figure_1|graph_density|value" = "Graph density of the Study 1 network",
  "figure_1|median_indegree|n" = "Median number of network members a follower follows",
  "figure_1|max_outdegree|n" = "Largest out-degree observed in the network",
  "figure_2|mean_tweets_per_weekday|value" = "LCV tweets and retweets per weekday, February 2013 to February 2015, mean",
  "figure_2|sd_tweets_per_weekday|value" = "LCV tweets and retweets per weekday, standard deviation",
  "figure_3|study1_share_male|percent" = "Figure 3 caption: Study 1 sample male, percent",
  "figure_3|study1_share_female|percent" = "Figure 3 caption: Study 1 sample female, percent",
  "figure_3|study1_share_org|percent" = "Figure 3 caption: Study 1 sample organizations, percent",
  "figure_3|study1_share_unknown|percent" = "Figure 3 caption: Study 1 sample unknown, percent",
  "figure_3|study2_share_male|percent" = "Figure 3 caption: Study 2 sample male, percent",
  "figure_3|study2_share_female|percent" = "Figure 3 caption: Study 2 sample female, percent",
  "figure_3|study2_share_org|percent" = "Figure 3 caption: Study 2 sample organizations, percent",
  "figure_3|study2_share_unknown|percent" = "Figure 3 caption: Study 2 sample unknown, percent",
  "table_a1|study1_types_5_to_7|percent" = "Table A1: types 5 to 7 combined, Study 1, percent of population",
  "table_a1|study2_types_5_to_7|percent" = "Table A1: types 5 to 7 combined, Study 2, percent of population",
  "table_a1|type_8_lower|proportion" = "Table A1: type 8 proportion, lower end of the printed interval",
  "table_a1|type_8_upper|proportion" = "Table A1: type 8 proportion, upper end of the printed interval",
  "figures_a1_a6|screenshots|count" = "Screenshots of the petitions, the tweet encouragement and the public tweets",
  "text|d_encouragement_one_third|holds" = "One third of each direct message arm was shown the tweet encouragement",
  "text|d_study1_pub_zero_signed|holds" = "Study 1: no subject in the public tweet arm signed the petition",
  "text|d_study1_dm_significant|holds" = "Study 1: both direct message effects on signing are positive and significant at p < .01",
  "text|d_study1_encouragement_half|holds" = "Study 1: the tweet encouragement moved nearly half of exposed signers",
  "text|d_study1_organizer_38pp|holds" = "Study 1: the organizer prime costs the encouragement more than 38 percentage points",
  "text|d_study1_follower_twice|holds" = "Study 1: the follower prime is more than twice as effective as the organizer prime",
  "text|d_study1_network_null|holds" = "Study 1: no significant second-stage effect on signing or tweeting",
  "text|d_study2_larger|holds" = "Study 2: the direct message effects on signing exceed Study 1's",
  "text|d_study2_follower_more_effective|holds" = "Study 2: the follower prime is more effective on tweeting but not significantly so",
  "text|d_study2_tweeted_network_null|holds" = "Study 2: no significant second-stage effect on tweeting",
  "text|d_study2_network_larger_than_main|holds" = "Study 2: the network effect on signing exceeds the direct message effects",
  "text|d_public_tweet_null|holds" = "Neither study's public tweet arm produced a signature or a retweet",
  "text|d_dm_click_4pp|holds" = "Direct messages produce approximately a 4 percentage point increase in signing",
  "text|d_dm_signatures_exceed_tweets|holds" = "In both studies the direct message effect on signing exceeds its effect on tweeting",
  "text|d_button_35_to_45|holds" = "The tweet encouragement effects lie between 35 and 45 percentage points",
  "figure_3|d_study1_org_smaller_signed|holds" = "Study 1 signing: organizations have the smallest conditional effects",
  "figure_3|d_study1_no_het_tweeted|holds" = "Study 1 tweeting: no account type heterogeneity",
  "figure_3|d_study2_org_smaller_signed|holds" = "Study 2: organizations have the smallest conditional effects on signing and none on tweeting",
  "figure_3|d_no_gender_pattern|holds" = "No consistent pattern in the conditional effects for men against women",
  "table_a1|d_types_1_to_4_zero|holds" = "Table A1: the population proportions of types 1 to 4 are all zero"
)

build_claim <- function(claim_id, table_figure, row_label, quantity) {
  head <- str_remove(row_label, ":.*$")
  tail <- if_else(str_detect(row_label, ":"), str_remove(row_label, "^[^:]*:"), NA_character_)
  q <- unname(quantity_labels[quantity])
  # case_when evaluates every branch over the whole vector, so the model label
  # lookup has to return a value for the descriptive and balance tables too,
  # which have no model columns and therefore no entry in model_labels.
  model_label <- function(tf, m) {
    map2_chr(tf, m, \(t, mm) {
      labels <- model_labels[[t]]
      if (is.null(labels) || is.na(mm) || !mm %in% names(labels)) NA_character_
      else unname(labels[[mm]])
    })
  }
  case_when(
    claim_id %in% names(claim_labels) ~ unname(claim_labels[claim_id]),
    table_figure %in% c("table_2", "table_3") ~ paste0(unname(arm_labels[head]), ", ", q),
    table_figure %in% c("table_a2", "table_a3") & row_label == "omnibus" ~ "Omnibus p-value",
    table_figure %in% c("table_a2", "table_a3") & is.na(tail) & quantity == "n" ~
      paste0(unname(arm_labels[head]), ", N"),
    table_figure %in% c("table_a2", "table_a3") & is.na(tail) ~
      paste0(unname(term_labels[head]), ", ", q),
    table_figure %in% c("table_a2", "table_a3") ~
      paste0(unname(term_labels[head]), ", ", unname(arm_labels[tail]), ", ", q),
    is.na(tail) ~ paste0(model_label(table_figure, head), ", ", q),
    .default = paste0(model_label(table_figure, head), ": ",
                      unname(term_labels[tail]), ", ", q)
  )
}

# Agreement at the precision the article prints ----
printed_decimals <- function(x) {
  d <- str_extract(x, "(?<=\\.)[0-9]+$")
  if_else(is.na(d), 0L, str_length(d))
}
rounds_to <- function(v, target, digits) {
  # A value that rounds to zero from below formats as "-0.000", which is the same
  # printed number as the "0.000" the article carries.
  drop_sign <- \(s) if_else(!is.na(s) & as.numeric(s) == 0, str_remove(s, "^-"), s)
  formatted <- if_else(is.na(v), NA_character_, sprintf(paste0("%.", digits, "f"), v))
  as.integer(drop_sign(formatted) == drop_sign(target))
}

key <- c("table_figure", "row_label", "quantity")

gt <- paper |>
  join_keep(script, by = key) |>
  join_keep(rewrite, by = key) |>
  mutate(
    digits = printed_decimals(value_paper),
    match = rounds_to(value_script, value_paper, digits),
    match_rewrite = rounds_to(value_rewrite, value_paper, digits),
    claim = build_claim(claim_id, table_figure, row_label, quantity)
  ) |>
  select(claim_id, table_figure, location, claim_type, claim, value_script, value_paper,
         match, value_rewrite, match_rewrite)

stopifnot(nrow(gt) == nrow(paper), !anyDuplicated(gt[c("table_figure", "claim")]),
          !any(is.na(gt$claim)))

# Figure 3, which prints no numbers ----
# The published figure carries no values to transcribe, so the comparison that can be
# made is between the estimates the rewrite plots and the ones the deposit plots. The
# join key has to identify a plotted estimate uniquely on both sides, or a many-to-many
# match would inflate the count and soften the maximum difference.
figure_3_keyed <- out("figure_3_het_account_type.csv") |>
  mutate(dv = str_to_lower(outcome_label),
         account_type = recode_values(str_to_lower(account_label),
                                      "organization" ~ "org",
                                      default = str_to_lower(account_label)),
         term = if_else(treatment == "Follower", "treatfol", "treatorg"))

figure_3_difference <- figure_3_keyed |>
  join_one(figure_3_archive, by = c("study", "dv", "account_type", "term"),
           what = "archive figure 3") |>
  summarize(max_difference = max(c(abs(estimate.x - estimate.y),
                                   abs(std.error - std_error))),
            pairs = n())

stopifnot(figure_3_difference$pairs == nrow(figure_3_archive))

# The archive re-run under the sampler R shipped before 3.6.0 ----
# The published value comes from published_claims.csv like every other value_paper, the
# re-run value from maintained/output/, and the verdict from comparing them, so
# nothing about this comparison is typed.
sampler_run <- out("text_sampler_reproducibility.csv")
sampler_published <- paper |>
  filter(row_label == "omnibus") |>
  select(table_figure, value_paper)

sampler <- sampler_run |>
  join_one(sampler_published, by = "table_figure", what = "published omnibus") |>
  mutate(
    claim_id = paste0(table_figure, "|omnibus|old_sampler"),
    location = if_else(table_figure == "table_a2", "Online Appendix 2, Table A2",
                       "Online Appendix 2, Table A3"),
    claim_type = "pipeline",
    claim = "Omnibus p-value under the pre-R-3.6 sampler",
    value_script = old_sampler,
    match = rounds_to(value_script, value_paper, printed_decimals(value_paper)),
    value_rewrite = NA_real_,
    match_rewrite = NA_integer_,
    notes = str_glue(
      "The deposit's own permutation code under RNGkind(sample.kind = \"Rounding\"), ",
      "permuting with {permutation_source} over {sims} draws, gives ",
      "{sprintf('%.3f', value_script)} against a published {value_paper}.")
  ) |>
  select(claim_id, table_figure, location, claim_type, claim, value_paper, value_script,
         value_rewrite, match, match_rewrite, notes)

figure_3_plotted <- tibble(
  claim_id = "figure_3|plotted|count",
  table_figure = "figure_3",
  location = "Figure 3, p. 124",
  claim_type = "pipeline",
  claim = as.character(str_glue("All {figure_3_difference$pairs} plotted conditional ",
                                "effects and their standard errors")),
  value_paper = NA_character_, value_script = NA_real_, value_rewrite = NA_real_,
  match = NA_integer_, match_rewrite = NA_integer_,
  notes = as.character(str_glue(
    "The figure prints no numbers, so there is nothing to compare against the article. ",
    "All {figure_3_difference$pairs} plotted estimate and standard error pairs agree ",
    "with the estimates the deposit hands to ggplot to within ",
    "{signif(figure_3_difference$max_difference, 2)}."))
)

# Why a published claim has no counterpart ----
# Keyed by claim_id, and carrying only the reason: the published value itself lives in
# published_claims.csv with every other value_paper, so no number is typed here.
no_counterpart <- tribble(
  ~claim_id, ~notes,
  "table_1|cells|count",
    "Collected from Twitter profiles in February 2015. The deposit holds only the two experimental data frames, so nothing in it can produce this table.",
  "table_1|mean_followers_top_ten|n",
    "Derived from the printed table; no deposited data behind it.",
  "table_1|mean_tweets_per_weekday|n",
    "Derived from the printed table; no deposited data behind it.",
  "table_1|median_creation_year|year",
    "Derived from the printed table; no deposited data behind it.",
  "figure_1|communities|n",
    "The network edge list is not deposited, so neither the figure nor its community count can be rebuilt.",
  "figure_1|modularity|value",
    "The network edge list is not deposited.",
  "figure_1|possible_connections|n",
    "The network edge list is not deposited.",
  "figure_1|observed_edges|n",
    "The network edge list is not deposited.",
  "figure_1|graph_density|value",
    "The network edge list is not deposited.",
  "figure_1|median_indegree|n",
    "The network edge list is not deposited.",
  "figure_1|max_outdegree|n",
    "The network edge list is not deposited.",
  "figure_2|mean_tweets_per_weekday|value", "The daily tweet series is not deposited.",
  "figure_2|sd_tweets_per_weekday|value", "The daily tweet series is not deposited.",
  "text|intercoder_kappa|value",
    "The coding was done by hand on 200 profiles and only the resolved account type is deposited, so the two coders' assignments no longer exist.",
  "text|study1_outside_retweeters|n",
    "Both data frames hold only LCV followers, so users outside the network are not in the deposit.",
  "text|study2_outside_retweeters|n",
    "Both data frames hold only LCV followers, so users outside the network are not in the deposit.",
  "text|lcv_follower_followers|n",
    "Stated as an order of magnitude in the text. The follower-of-follower counts are not deposited.",
  "figures_a1_a6|screenshots|count",
    "Six screenshots of experimental materials. No numbers and no data."
)

# Where a mismatch lives ----
# Every number inside a note is read back out of the table it annotates or out of the
# archive values, so a note cannot drift away from the verdict beside it.
diagnostic <- function(row_label) {
  script |> filter(table_figure == "diagnostic", row_label == !!row_label) |>
    pull(value_script)
}
computed <- function(claim_id, column) {
  gt |> filter(claim_id == !!claim_id) |> pull({{ column }})
}
second_stage_pool <- computed("text|study2_second_stage_pool|n", value_rewrite)
second_stage_sample <- diagnostic("study2_second_stage_sample")
study_2_signing_rate <- computed("table_a1|study2_types_5_to_7|percent", value_rewrite)

# Named apart from the notes column, which the join below brings in: inside mutate a
# column of the same name would shadow this vector and every index would return NA.
note_text <- c(
  "Randomization inference p-value over 1,000 permutations drawn with complete_ra(). The deposit's own comments install randomizr from GitHub rather than from CRAN and name no version, and the released package no longer accepts the argument the deposit passes it, so the stream behind the published value cannot be reconstructed, and the old sampler does not recover it either.",
  "Randomization inference p-value over 1,000 permutations. The deposit's own strata_ra() reproduces the published value exactly under RNGkind(sample.kind = 'Rounding'); both figures here use the sampler R has shipped since 3.6.0.",
  str_glue("The deposited data hold {second_stage_pool} Study 2 subjects with a positive probability of exposure, of whom the {second_stage_sample} whose probability is strictly below one are the sample Table 9 analyses."),
  str_glue("The signing rate among Study 2 direct message subjects is {sprintf('%.2f', study_2_signing_rate)} percent, which the appendix rounds up."),
  str_glue("The appendix prints an interval derived from rounded inputs. Both computed complements, {sprintf('%.4f', computed('table_a1|type_8_lower|proportion', value_rewrite))} for Study 2 and {sprintf('%.4f', computed('table_a1|type_8_upper|proportion', value_rewrite))} for Study 1, fall inside it."),
  str_glue("The article prints an inequality rather than a value, so there is nothing to compare at printed precision. The deposit's own lm() gives {signif(computed('text|study1_button_imbalance_p|p_value', value_script), 2)} and the rewrite's HC2 fit gives {signif(computed('text|study1_button_imbalance_p|p_value', value_rewrite), 2)}, both below the 0.01 the article asserts."),
  "The article's own arithmetic on a rounded input, not a pipeline quantity: 250 direct messages a day for 30 days at the four percentage point effect it quotes."
) |>
  as.character()

gt <- gt |>
  left_join(no_counterpart, by = "claim_id") |>
  mutate(
    notes = case_when(
      !is.na(notes) ~ notes,
      claim_id == "table_a2|omnibus|p_value" ~ note_text[1],
      claim_id == "table_a3|omnibus|p_value" ~ note_text[2],
      claim_id == "text|study2_second_stage_pool|n" ~ note_text[3],
      claim_id == "table_a1|study2_types_5_to_7|percent" ~ note_text[4],
      # Written per row rather than once: the deposit returns a numerically tiny
      # variance for some of these cells and the 0.00004 LCVsource.r substitutes
      # for a NaN in the others, and a single note would be false for half of them.
      str_detect(claim, "standard error$") & is.na(value_rewrite) ~ as.character(str_glue(
        "Standard error undefined: the coefficient is identified only within the ",
        "public tweet arm, where no subject signed or tweeted, so the robust variance ",
        "of the term is zero up to floating point and comes back very slightly ",
        "negative. The deposit's HC2 wrapper returns {signif(value_script, 3)}, which ",
        "prints as 0.000; lm_robust takes the square root and returns NaN, so the ",
        "rewrite records the cell as unverifiable.")),
      claim_id %in% c("table_a1|type_8_lower|proportion",
                      "table_a1|type_8_upper|proportion") ~ note_text[5],
      claim_id == "text|study1_button_imbalance_p|p_value" ~ note_text[6],
      claim_id == "text|projected_signatures|n" ~ note_text[7],
      .default = ""
    ),
    # The printed interval is a rounded envelope rather than a point, so agreement
    # to printed precision is the wrong test and both ends are recorded unverifiable.
    across(c(match, match_rewrite),
           \(m) if_else(claim_id %in% c("table_a1|type_8_lower|proportion",
                                        "table_a1|type_8_upper|proportion"),
                        NA_integer_, m))
  ) |>
  bind_rows(figure_3_plotted, sampler) |>
  mutate(
    paper_id = "coppock_guess_ternovski_2016",
    # A published claim the deposit cannot support is a fact about the deposit, not a
    # gap in this table, so it is named as such rather than left blank beside the
    # rows where the rewrite genuinely disagrees with the article.
    # A row is adverse when EITHER verdict is 0. Keying the locus on match_rewrite
    # alone leaves the archive-fails-and-the-rewrite-has-nothing-to-compare shape
    # with no locus at all, which is what the old-sampler row below used to be.
    adverse = (!is.na(match) & match == 0) | (!is.na(match_rewrite) & match_rewrite == 0),
    defect_locus = case_when(
      claim_id %in% no_counterpart$claim_id ~ "archive",
      !adverse ~ NA_character_,
      claim_id == "table_a2|omnibus|p_value" ~ "archive",
      claim_id == "table_a3|omnibus|p_value" ~ "environment",
      # The old-sampler rows ask whether R's pre-3.6 sampler recovers the published
      # p-value. Where it does not, the cause is the unpinned randomizr named above,
      # and this row is the evidence ruling the sampler out as the explanation.
      claim_id == "table_a2|omnibus|old_sampler" ~ "archive",
      .default = "paper_internal"
    )
  ) |>
  select(paper_id, claim_id, table_figure, location, claim_type, claim, value_script,
         value_paper, match, value_rewrite, match_rewrite, defect_locus, notes)

# Coverage of the published float list ----
# The list is the article's and the appendix's own inventory, read off their pages:
# nine numbered tables and three numbered figures in the main text, five appendix
# tables and six appendix figures. Coverage is a gate rather than a summary, so a
# float nothing in the pipeline reaches has to be named as uncovered instead of
# silently absent.
published_floats <- c(paste0("table_", 1:9), paste0("table_a", 1:5),
                      paste0("figure_", 1:3), "figures_a1_a6")
stopifnot(
  all(published_floats %in% gt$table_figure),
  all(gt$table_figure %in% c(published_floats, "text"))
)

# Coverage of the extraction ----
# Every pipeline and descriptive claim must have a computed counterpart or a stated
# reason it has none, and must also be printed by maintained/in_text_claims.R. The
# second half is read out of that file's "covers:" lines, so adding a claim to the
# extraction without adding a block to the claims script stops the build.
covered_patterns <- read_lines(here::here("maintained", "in_text_claims.R")) |>
  str_subset("^# covers: ") |>
  str_remove("^# covers: ") |>
  str_split("\\s+") |>
  list_c()
stopifnot(length(covered_patterns) > 0)

is_covered <- function(claim_id) {
  any(map_lgl(covered_patterns, function(p) {
    if (str_ends(p, fixed("*"))) str_starts(claim_id, fixed(str_remove(p, fixed("*"))))
    else identical(claim_id, p)
  }))
}

must_be_computed <- gt |> filter(claim_type %in% c("pipeline", "descriptive"))
missing_value <- must_be_computed |> filter(is.na(value_rewrite), notes == "")
missing_block <- must_be_computed |> filter(!map_lgl(claim_id, is_covered))

if (nrow(missing_value) > 0) {
  print(missing_value |> select(claim_id, claim), n = 50)
  stop(nrow(missing_value), " pipeline or descriptive claims have neither a computed ",
       "value nor a stated reason")
}
if (nrow(missing_block) > 0) {
  print(missing_block |> select(claim_id, claim), n = 50)
  stop(nrow(missing_block), " pipeline or descriptive claims have no block in ",
       "maintained/in_text_claims.R")
}

# A note explains why a value does not reproduce, so a row that does reproduce must
# not carry one: that is the shape a note contradicting its own verdict would take.
# The locus rule has three states: an adverse row, meaning either verdict is 0, must
# name where the fault lies; a clean match, meaning both verdicts are 1, must not; a
# row with no verdict may. Gating on match_rewrite == 0 alone tests only part of it.
gt_adverse <- (!is.na(gt$match) & gt$match == 0) |
  (!is.na(gt$match_rewrite) & gt$match_rewrite == 0)
gt_clean <- !is.na(gt$match) & gt$match == 1 &
  !is.na(gt$match_rewrite) & gt$match_rewrite == 1
stopifnot(
  !any(gt$match_rewrite == 1 & gt$notes != "", na.rm = TRUE),
  all(!is.na(gt$defect_locus[gt_adverse])),
  all(is.na(gt$defect_locus[gt_clean]))
)

# Errata spine gate ----
# Every claim id an errata entry names has to exist here. A missing one is a typo or a
# claim that has since been renamed, and a published correction pointing at a row that is
# not in the table is a dangling reference the build should refuse to carry.
errata_path <- here::here("errata_entries.csv")
if (file.exists(errata_path)) {
  errata_spine <- read_csv(errata_path, show_col_types = FALSE)
  # Both entries here name no row, so the written column is empty and reads back as a
  # logical vector. as.character() is what makes the gate survive that and still fire
  # the moment an entry does name one.
  cited_claim_ids <- errata_spine$claim_ids |>
    as.character() |>
    str_split(";") |>
    unlist() |>
    str_trim()
  cited_claim_ids <- cited_claim_ids[!is.na(cited_claim_ids) & cited_claim_ids != ""]
  if (length(setdiff(cited_claim_ids, gt$claim_id)) > 0) {
    print(setdiff(cited_claim_ids, gt$claim_id))
  }
  stopifnot(length(setdiff(cited_claim_ids, gt$claim_id)) == 0)
}

write_csv(gt, here::here("ground_truth", "coppock_guess_ternovski_2016_ground_truth.csv"))

print(gt |> filter(is.na(match) | match == 0 | is.na(match_rewrite) | match_rewrite == 0) |>
        select(table_figure, claim, value_paper, value_script, value_rewrite,
               match, match_rewrite, defect_locus),
      n = 80, width = 250)
print(gt |> count(claim_type), n = 10)
print(str_glue(
  "rows: {nrow(gt)}; archive comparable: {sum(!is.na(gt$match))}, ",
  "matching: {sum(gt$match == 1, na.rm = TRUE)}; ",
  "rewrite comparable: {sum(!is.na(gt$match_rewrite))}, ",
  "matching: {sum(gt$match_rewrite == 1, na.rm = TRUE)}"))
