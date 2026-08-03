# coppock_guess_ternovski_2016/maintained/text_treatment_effects.R
# Output: output/text_treatment_effects.csv
# Depends on: clean_lcv.R output, helpers.R
# Description: Treatment effects the article states in running text rather than in a table:
#   the effect of receiving any direct message, the follower minus organizer gap in
#   tweeting, the effect of the tweet encouragement within each direct message arm, and
#   the Study 1 balance anomaly the article reports on the encouragement assignment.
source(here::here("maintained", "helpers.R"))

lcv_study_1 <- read_rds(here::here("maintained", "output", "lcv_study_1.rds"))
lcv_study_2 <- read_rds(here::here("maintained", "output", "lcv_study_2.rds"))

# Effect of receiving any direct message ----
# Collapsing the two direct message arms gives the "3.6-percentage-point increase
# in petition signing" of the Study 1 results section.
any_dm <- function(data, study) {
  fit <- lm_robust(signed ~ dm, data = mutate(data, dm = as.numeric(treat != "pub")))
  tidy(fit) |>
    filter(term == "dm") |>
    transmute(study, quantity = "Effect of any direct message on signing, percentage points",
              estimate = 100 * estimate, std_error = 100 * std.error, p_value = p.value)
}

# Follower minus organizer, tweeting ----
# Restricting to the two direct message arms puts the gap on the organizer
# coefficient, with the follower arm as the reference.
dm_gap <- function(data, study) {
  dm_only <- data |>
    filter(treat != "pub") |>
    mutate(treat = droplevels(treat))
  tidy(lm_robust(tweeted ~ treat, data = dm_only)) |>
    filter(term == "treatorg") |>
    transmute(study, quantity = "Organizer minus follower effect on tweeting, percentage points",
              estimate = 100 * estimate, std_error = 100 * std.error, p_value = p.value)
}

# Tweet encouragement within each direct message arm ----
# Refitting with each arm as the reference reads the arm-specific encouragement
# effect straight off the tweetbutton coefficient, standard error included.
encouragement_by_arm <- function(data, study) {
  signers <- data |>
    filter(signed == 1) |>
    mutate(treat = droplevels(treat))
  map(c("fol", "org"), function(reference) {
    fit <- lm_robust(tweeted ~ tweetbutton * treat,
                     data = mutate(signers, treat = relevel(treat, ref = reference)))
    tidy(fit) |>
      filter(term == "tweetbutton") |>
      transmute(study,
                quantity = str_glue("Tweet encouragement effect in the {reference} arm, percentage points"),
                estimate = 100 * estimate, std_error = 100 * std.error, p_value = p.value)
  }) |>
    list_rbind()
}

# The Study 1 balance anomaly ----
# The article reports that assignment to the tweet encouragement predicted signing in
# Study 1, which should be impossible because the encouragement was shown only after a
# subject had signed. The public tweet arm was never randomised into it, so its
# tweetbutton is missing and the regression runs on the two direct message arms.
encouragement_balance <- function(data, study) {
  assigned <- filter(data, !is.na(tweetbutton))
  tidy(lm_robust(signed ~ tweetbutton, data = assigned)) |>
    filter(term == "tweetbutton") |>
    transmute(study,
              quantity = "Tweet encouragement assignment predicting signing, percentage points",
              estimate = 100 * estimate, std_error = 100 * std.error, p_value = p.value)
}

text_treatment_effects <- bind_rows(
  any_dm(lcv_study_1, "Study 1"),
  any_dm(lcv_study_2, "Study 2"),
  dm_gap(lcv_study_1, "Study 1"),
  dm_gap(lcv_study_2, "Study 2"),
  encouragement_by_arm(lcv_study_1, "Study 1"),
  encouragement_by_arm(lcv_study_2, "Study 2"),
  encouragement_balance(lcv_study_1, "Study 1")
)

write_csv(text_treatment_effects,
          here::here("maintained", "output", "text_treatment_effects.csv"))
