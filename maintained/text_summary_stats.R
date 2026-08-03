# coppock_guess_ternovski_2016/maintained/text_summary_stats.R
# Output: output/text_summary_stats.csv
# Depends on: clean_lcv.R output, helpers.R
# Description: Descriptive quantities the article states in running text rather than in a
#   table: the account type composition reported in the Figure 3 caption, the size of each
#   study's second-stage subject pool, and the subject type proportions of Appendix 1.
source(here::here("maintained", "helpers.R"))

lcv_study_1 <- read_rds(here::here("maintained", "output", "lcv_study_1.rds"))
lcv_study_2 <- read_rds(here::here("maintained", "output", "lcv_study_2.rds"))

# Figure 3 caption: account type composition ----
composition <- bind_rows(
  count(lcv_study_1, account_type) |> mutate(study = "Study 1"),
  count(lcv_study_2, account_type) |> mutate(study = "Study 2")
) |>
  mutate(percent = 100 * n / sum(n), .by = study) |>
  transmute(study, quantity = str_glue("Share of sample: {account_type}"), value = percent)

# Network size and total tweets ----
# The article gives each study's subject pool as the size of the scraped network, and
# uses the Study 1 tweet count to argue that the public tweet reached the network while
# producing no signatures in the public tweet arm.
network_size <- tibble(
  study = c("Study 1", "Study 2"),
  quantity = "Network members",
  value = c(nrow(lcv_study_1), nrow(lcv_study_2))
)

tweets_of_link <- tibble(
  study = "Study 1",
  quantity = "Tweets of the link throughout the network",
  value = sum(lcv_study_1$tweeted)
)

# Second-stage subject pools ----
# The second-stage sample is the set of users whose exposure probability the design
# left strictly between zero and one; a handful of Study 2 subjects followed enough
# signers that exposure was certain, and those are excluded from Table 9.
second_stage <- tibble(
  study = c("Study 1", "Study 1", "Study 1", "Study 2", "Study 2", "Study 2"),
  quantity = rep(c("Petition signers",
                   "Followers with positive exposure probability",
                   "Second-stage analysis sample"), 2),
  value = c(
    sum(lcv_study_1$signed),
    sum(lcv_study_1$probexposure > 0, na.rm = TRUE),
    sum(lcv_study_1$probexposure > 0 & lcv_study_1$probexposure < 1, na.rm = TRUE),
    sum(lcv_study_2$signed),
    sum(lcv_study_2$probexposure > 0, na.rm = TRUE),
    sum(lcv_study_2$probexposure > 0 & lcv_study_2$probexposure < 1, na.rm = TRUE)
  )
)

# Appendix 1: subject type proportions ----
# No subject assigned to the public tweet signed, so types 1 to 4 have proportion
# zero and types 5 to 7 together are the signing rate among direct message
# subjects. Type 8 is the remainder.
signing_rate_dm <- c(
  "Study 1" = mean(lcv_study_1$signed[lcv_study_1$treat != "pub"]),
  "Study 2" = mean(lcv_study_2$signed[lcv_study_2$treat != "pub"])
)

subject_types <- tibble(
  study = names(signing_rate_dm),
  quantity = "Types 5 to 7 combined, percent of population",
  value = 100 * unname(signing_rate_dm)
) |>
  bind_rows(tibble(
    study = names(signing_rate_dm),
    quantity = "Type 8, proportion of population",
    value = 1 - unname(signing_rate_dm)
  ))

# Footnote 11: the power calculation behind the null public tweet result ----
# The footnote asks what would follow if a public tweet generated one click per
# 10,000 followers exposed, and reports how often a study of Study 1's size would
# then see no clicks at all. The rate is the footnote's own hypothetical; the
# sample size comes from the data.
zero_clicks <- tibble(
  study = "Study 1",
  quantity = "Probability of zero clicks at one click per 10,000 exposed, percent",
  value = 100 * (1 - 1 / 10000)^nrow(lcv_study_1)
)

text_summary_stats <- bind_rows(composition, network_size, tweets_of_link, second_stage,
                                subject_types, zero_clicks)

write_csv(text_summary_stats, here::here("maintained", "output", "text_summary_stats.csv"))
