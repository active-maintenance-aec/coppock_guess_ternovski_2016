# coppock_guess_ternovski_2016/maintained/table_2_study1_outcomes.R
# Output: output/table_2_study1_outcomes.csv, output/table_2_study1_outcomes.tex
# Depends on: clean_lcv.R output, helpers.R
# Description: Table 2: Study 1 design and outcomes, by treatment arm and tweet
#   encouragement, with counts and percentages signing and tweeting.
source(here::here("maintained", "helpers.R"))

lcv_study_1 <- read_rds(here::here("maintained", "output", "lcv_study_1.rds"))

# Cells ----
# The public tweet arm was never randomised into the encouragement, so its
# tweetbutton is missing and it forms a single row.
cells <- lcv_study_1 |>
  group_by(treat, tweetbutton) |>
  summarize(
    n = n(),
    n_signed = sum(signed),
    n_tweeted = sum(tweeted),
    .groups = "drop"
  )

table_2 <- cells |>
  bind_rows(summarize(lcv_study_1, treat = NA, tweetbutton = NA, n = n(),
                      n_signed = sum(signed), n_tweeted = sum(tweeted))) |>
  mutate(
    arm = case_when(
      is.na(treat) ~ "Total",
      treat == "pub" ~ "Public tweet",
      treat == "org" & tweetbutton == 1 ~ "Organizer DM: tweet encouragement",
      treat == "org" & tweetbutton == 0 ~ "Organizer DM: no encouragement",
      treat == "fol" & tweetbutton == 1 ~ "Follower DM: tweet encouragement",
      treat == "fol" & tweetbutton == 0 ~ "Follower DM: no encouragement"
    ),
    pct_signed = 100 * n_signed / n,
    pct_tweeted = 100 * n_tweeted / n
  ) |>
  select(arm, n, n_signed, pct_signed, n_tweeted, pct_tweeted) |>
  slice(match(c("Public tweet",
                "Organizer DM: tweet encouragement", "Organizer DM: no encouragement",
                "Follower DM: tweet encouragement", "Follower DM: no encouragement",
                "Total"), arm))

write_csv(table_2, here::here("maintained", "output", "table_2_study1_outcomes.csv"))

table_2 |>
  mutate(
    signed = str_glue("{n_signed} ({sprintf('%.1f', pct_signed)})"),
    tweeted = str_glue("{n_tweeted} ({sprintf('%.1f', pct_tweeted)})")
  ) |>
  select(arm, n, signed, tweeted) |>
  kable(format = "latex", booktabs = TRUE,
        col.names = c("Treatment group", "N", "Signed (\\%)", "Tweeted (\\%)"),
        escape = FALSE, caption = "Study 1: design and outcomes.") |>
  write_lines(here::here("maintained", "output", "table_2_study1_outcomes.tex"))
