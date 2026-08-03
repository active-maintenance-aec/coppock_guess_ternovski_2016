# coppock_guess_ternovski_2016/maintained/text_descriptive_claims.R
# Output: output/text_descriptive_claims.csv
# Depends on: every table and figure script's csv output, helpers.R
# Description: Truth values for the article's qualitative claims, the ones that assert a
#   shape, a sign, a threshold or a comparison rather than a number. They cannot be
#   checked by transcribing a value, so each is turned into a statement about the
#   estimates and evaluated. Nothing is refitted here: every quantity is read back out
#   of the csv a table or figure script already wrote, so a claim is judged against the
#   same numbers the published float carries.
source(here::here("maintained", "helpers.R"))

out <- function(f) read_csv(here::here("maintained", "output", f), show_col_types = FALSE)

table_2 <- out("table_2_study1_outcomes.csv")
table_3 <- out("table_3_study2_outcomes.csv")
table_4 <- out("table_4_study1_dm_effects.csv")
table_5 <- out("table_5_study1_encouragement.csv")
table_6 <- out("table_6_study1_network.csv")
table_7 <- out("table_7_study2_dm_effects.csv")
table_8 <- out("table_8_study2_encouragement.csv")
table_9 <- out("table_9_study2_network.csv")
table_a4 <- out("table_a4_study1_heterogeneity.csv")
table_a5 <- out("table_a5_study2_heterogeneity.csv")
figure_3 <- out("figure_3_het_account_type.csv")
effects <- out("text_treatment_effects.csv")
lcv_study_1 <- read_rds(here::here("maintained", "output", "lcv_study_1.rds"))
lcv_study_2 <- read_rds(here::here("maintained", "output", "lcv_study_2.rds"))

# One accessor for every coefficient the claims below reason about ----
# Each claim names a model and a term, so a claim can never read the wrong row by
# accident and an empty selection stops the script instead of returning NA.
cell <- function(d, model, term, column) {
  hit <- d |> filter(model == !!model, term == !!term) |> pull({{ column }})
  stopifnot(length(hit) == 1, !is.na(hit))
  hit
}
effect <- function(study, pattern, column) {
  hit <- effects |> filter(study == !!study, str_detect(quantity, pattern)) |> pull({{ column }})
  stopifnot(length(hit) == 1)
  hit
}

claim <- function(claim_id, holds, evidence) {
  tibble(claim_id, holds = as.integer(holds), evidence = as.character(evidence))
}

# Design and non-interference ----
share_encouraged <- lcv_study_1 |>
  filter(treat != "pub") |>
  summarize(share = mean(tweetbutton), .by = treat) |>
  pull(share)

pub_1 <- table_2 |> filter(arm == "Public tweet")
pub_2 <- table_3 |> filter(arm == "Public tweet")

d_encouragement_one_third <- claim(
  "text|d_encouragement_one_third|holds",
  all(abs(share_encouraged - 1 / 3) < 1e-12),
  str_glue("Share shown the encouragement within each Study 1 direct message arm: ",
           "{paste(sprintf('%.4f', share_encouraged), collapse = ', ')}."))

d_study1_pub_zero_signed <- claim(
  "text|d_study1_pub_zero_signed|holds",
  pub_1$n_signed == 0,
  str_glue("Study 1 public tweet arm: {pub_1$n_signed} signatures among {pub_1$n} subjects."))

d_public_tweet_null <- claim(
  "text|d_public_tweet_null|holds",
  pub_1$n_signed == 0 && pub_1$n_tweeted == 0 &&
    pub_2$n_signed == 0 && pub_2$n_tweeted == 0,
  str_glue("Public tweet arms: Study 1 {pub_1$n_signed} signed and {pub_1$n_tweeted} ",
           "tweeted of {pub_1$n}; Study 2 {pub_2$n_signed} signed and ",
           "{pub_2$n_tweeted} tweeted of {pub_2$n}."))

d_types_1_to_4_zero <- claim(
  "table_a1|d_types_1_to_4_zero|holds",
  pub_1$n_signed == 0 && pub_2$n_signed == 0,
  str_glue("Signatures in the public tweet arms: {pub_1$n_signed} in Study 1 and ",
           "{pub_2$n_signed} in Study 2."))

# Study 1 main effects ----
fol_1 <- cell(table_4, "M1 signed", "treatfol", estimate)
org_1 <- cell(table_4, "M1 signed", "treatorg", estimate)
p_fol_1 <- cell(table_4, "M1 signed", "treatfol", p.value)
p_org_1 <- cell(table_4, "M1 signed", "treatorg", p.value)

d_study1_dm_significant <- claim(
  "text|d_study1_dm_significant|holds",
  fol_1 > 0 && org_1 > 0 && p_fol_1 < 0.01 && p_org_1 < 0.01,
  str_glue("Study 1 signing: follower {sprintf('%.3f', fol_1)} (p = ",
           "{signif(p_fol_1, 2)}), organizer {sprintf('%.3f', org_1)} (p = ",
           "{signif(p_org_1, 2)}); both positive and both below 0.01."))

# Study 1 tweet encouragement ----
button_1 <- cell(table_5, "M1", "tweetbutton", estimate)
interaction_1 <- cell(table_5, "M2", "tweetbutton:treatorg", estimate)
enc_fol_1 <- effect("Study 1", "in the fol arm", estimate)
enc_org_1 <- effect("Study 1", "in the org arm", estimate)

d_study1_encouragement_half <- claim(
  "text|d_study1_encouragement_half|holds",
  button_1 >= 0.40 && button_1 < 0.50,
  str_glue("Study 1 encouragement effect on tweeting among signers: ",
           "{sprintf('%.3f', button_1)}, read as nearly half."))

d_study1_organizer_38pp <- claim(
  "text|d_study1_organizer_38pp|holds",
  interaction_1 < -0.38,
  str_glue("Encouragement by organizer interaction, Study 1: ",
           "{sprintf('%.3f', interaction_1)}, that is ",
           "{sprintf('%.1f', -100 * interaction_1)} percentage points."))

d_study1_follower_twice <- claim(
  "text|d_study1_follower_twice|holds",
  enc_fol_1 > 2 * enc_org_1,
  str_glue("Study 1 encouragement effect: {sprintf('%.1f', enc_fol_1)} percentage ",
           "points in the follower arm against {sprintf('%.1f', enc_org_1)} in the ",
           "organizer arm, a ratio of {sprintf('%.2f', enc_fol_1 / enc_org_1)}."))

# Study 1 network effects ----
network_1_p <- table_6 |>
  filter(model != "M1 shown tweet, OLS", term %in% c("exposure", "actual_exposure")) |>
  pull(p.value)

d_study1_network_null <- claim(
  "text|d_study1_network_null|holds",
  all(network_1_p >= 0.05),
  str_glue("Study 1 second stage, columns 2 to 5, p-values: ",
           "{paste(signif(network_1_p, 2), collapse = ', ')}."))

# Study 2 main effects ----
fol_2 <- cell(table_7, "M1 signed", "treatfol", estimate)
org_2 <- cell(table_7, "M1 signed", "treatorg", estimate)
fol_tweeted_2 <- cell(table_7, "M3 tweeted", "treatfol", estimate)
org_tweeted_2 <- cell(table_7, "M3 tweeted", "treatorg", estimate)
p_diff_tweeted_2 <- cell(table_7, "Follower vs organizer, tweeted", "treatorg", p.value)

d_study2_larger <- claim(
  "text|d_study2_larger|holds",
  fol_2 > fol_1 && org_2 > org_1,
  str_glue("Signing effects: follower {sprintf('%.3f', fol_1)} in Study 1 against ",
           "{sprintf('%.3f', fol_2)} in Study 2; organizer {sprintf('%.3f', org_1)} ",
           "against {sprintf('%.3f', org_2)}."))

d_study2_follower_more_effective <- claim(
  "text|d_study2_follower_more_effective|holds",
  fol_tweeted_2 > org_tweeted_2 && p_diff_tweeted_2 >= 0.05,
  str_glue("Study 2 tweeting: follower {sprintf('%.3f', fol_tweeted_2)} against ",
           "organizer {sprintf('%.3f', org_tweeted_2)}, difference p = ",
           "{signif(p_diff_tweeted_2, 2)}."))

# Study 2 network effects ----
network_2_tweeted_p <- table_9 |>
  filter(model %in% c("M4 tweeted, OLS", "M5 tweeted, IV"),
         term %in% c("exposure", "actual_exposure")) |>
  pull(p.value)
cace_2 <- cell(table_9, "M3 signed, IV", "actual_exposure", estimate)

d_study2_tweeted_network_null <- claim(
  "text|d_study2_tweeted_network_null|holds",
  all(network_2_tweeted_p >= 0.05),
  str_glue("Study 2 second stage, tweeting, columns 4 and 5, p-values: ",
           "{paste(signif(network_2_tweeted_p, 2), collapse = ', ')}."))

d_study2_network_larger_than_main <- claim(
  "text|d_study2_network_larger_than_main|holds",
  cace_2 > fol_2 && cace_2 > org_2,
  str_glue("Study 2 complier effect on signing {sprintf('%.3f', cace_2)} against ",
           "direct message effects of {sprintf('%.3f', fol_2)} and ",
           "{sprintf('%.3f', org_2)}."))

# Discussion ----
dm_1 <- effect("Study 1", "any direct message on signing", estimate)
dm_2 <- effect("Study 2", "any direct message on signing", estimate)
dm_tweeted_1 <- 100 * mean(c(cell(table_4, "M3 tweeted", "treatfol", estimate),
                             cell(table_4, "M3 tweeted", "treatorg", estimate)))
dm_tweeted_2 <- 100 * mean(c(fol_tweeted_2, org_tweeted_2))
button_2 <- cell(table_8, "M1", "tweetbutton", estimate)

d_dm_click_4pp <- claim(
  "text|d_dm_click_4pp|holds",
  all(round(c(dm_1, dm_2)) == 4),
  str_glue("Effect of any direct message on signing: {sprintf('%.1f', dm_1)} ",
           "percentage points in Study 1 and {sprintf('%.1f', dm_2)} in Study 2, ",
           "both rounding to 4."))

d_dm_signatures_exceed_tweets <- claim(
  "text|d_dm_signatures_exceed_tweets|holds",
  dm_1 > dm_tweeted_1 && dm_2 > dm_tweeted_2,
  str_glue("Study 1: signing {sprintf('%.1f', dm_1)} against tweeting ",
           "{sprintf('%.1f', dm_tweeted_1)} percentage points. Study 2: ",
           "{sprintf('%.1f', dm_2)} against {sprintf('%.1f', dm_tweeted_2)}."))

# The article prints the range as whole percentage points, so the endpoints are
# compared at that precision: the Study 1 effect is 45.4 and rounds to the 45 the
# article gives, which a raw comparison against 45 would count as a mismatch.
button_range <- round(100 * range(c(button_1, button_2)))

d_button_35_to_45 <- claim(
  "text|d_button_35_to_45|holds",
  all(button_range == c(35, 45)),
  str_glue("Tweet encouragement effect on tweeting among signers: ",
           "{sprintf('%.1f', 100 * button_2)} percentage points in Study 2 and ",
           "{sprintf('%.1f', 100 * button_1)} in Study 1, a range of ",
           "{button_range[1]} to {button_range[2]} at the precision the article prints."))

# Heterogeneity by account type ----
# The figure plots one conditional effect per account type, treatment, outcome and
# study, with the classical intervals the article shows, so significance here is
# whether that interval excludes zero.
cate <- figure_3 |>
  mutate(significant = conf.low > 0 | conf.high < 0)

smallest_is_org <- function(study, outcome) {
  cate |>
    filter(study == !!study, outcome_label == !!outcome) |>
    summarize(smallest = account_label[which.min(estimate)] == "Organization",
              .by = treatment) |>
    pull(smallest) |>
    all()
}
org_gap <- function(study, outcome) {
  cate |>
    filter(study == !!study, outcome_label == !!outcome) |>
    summarize(gap = estimate[account_label == "Organization"] -
                mean(estimate[account_label != "Organization"]), .by = treatment) |>
    pull(gap)
}

interaction_p <- function(d, dv) {
  d |>
    filter(model == paste(dv, "x account_type"), str_detect(term, "account_type")) |>
    filter(str_detect(term, ":")) |>
    pull(p.value)
}

d_study1_org_smaller_signed <- claim(
  "figure_3|d_study1_org_smaller_signed|holds",
  smallest_is_org("Study 1", "Signed"),
  str_glue("Study 1 signing: the organization conditional effect is the smallest of ",
           "the four account types for both treatments, by ",
           "{paste(sprintf('%.3f', org_gap('Study 1', 'Signed')), collapse = ' and ')} ",
           "against the mean of the others."))

d_study1_no_het_tweeted <- claim(
  "figure_3|d_study1_no_het_tweeted|holds",
  all(interaction_p(table_a4, "tweeted") >= 0.05),
  str_glue("Study 1 tweeting, treatment by account type interactions, p-values: ",
           "{paste(signif(interaction_p(table_a4, 'tweeted'), 2), collapse = ', ')}."))

d_study2_org_smaller_signed <- claim(
  "figure_3|d_study2_org_smaller_signed|holds",
  smallest_is_org("Study 2", "Signed") && all(interaction_p(table_a5, "tweeted") >= 0.05),
  str_glue("Study 2 signing: the organization conditional effect is the smallest of ",
           "the four account types for both treatments, by ",
           "{paste(sprintf('%.3f', org_gap('Study 2', 'Signed')), collapse = ' and ')}. ",
           "Study 2 tweeting interactions, p-values: ",
           "{paste(signif(interaction_p(table_a5, 'tweeted'), 2), collapse = ', ')}."))

male_minus_female <- cate |>
  filter(account_label %in% c("Male", "Female")) |>
  summarize(gap = estimate[account_label == "Male"] - estimate[account_label == "Female"],
            .by = c(study, outcome_label, treatment))

d_no_gender_pattern <- claim(
  "figure_3|d_no_gender_pattern|holds",
  length(unique(sign(male_minus_female$gap))) > 1,
  str_glue("Male minus female conditional effects across the ",
           "{nrow(male_minus_female)} study by outcome by treatment cells: ",
           "{sum(male_minus_female$gap > 0)} positive and ",
           "{sum(male_minus_female$gap < 0)} negative."))

text_descriptive_claims <- bind_rows(
  d_encouragement_one_third, d_study1_pub_zero_signed, d_study1_dm_significant,
  d_study1_encouragement_half, d_study1_organizer_38pp, d_study1_follower_twice,
  d_study1_network_null, d_study2_larger, d_study2_follower_more_effective,
  d_study2_tweeted_network_null, d_study2_network_larger_than_main,
  d_public_tweet_null, d_dm_click_4pp, d_dm_signatures_exceed_tweets,
  d_button_35_to_45, d_study1_org_smaller_signed, d_study1_no_het_tweeted,
  d_study2_org_smaller_signed, d_no_gender_pattern, d_types_1_to_4_zero
)

write_csv(text_descriptive_claims,
          here::here("maintained", "output", "text_descriptive_claims.csv"))
print(text_descriptive_claims, n = 30, width = 200)
