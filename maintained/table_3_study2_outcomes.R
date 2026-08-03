# coppock_guess_ternovski_2016/maintained/table_3_study2_outcomes.R
# Output: output/table_3_study2_outcomes.csv, output/table_3_study2_outcomes.tex
# Depends on: clean_lcv.R output, helpers.R
# Description: Table 3: Study 2 design and outcomes, by treatment arm, followed by the
#   tweet encouragement panel among petition signers.
source(here::here("maintained", "helpers.R"))

lcv_study_2 <- read_rds(here::here("maintained", "output", "lcv_study_2.rds"))

# Upper panel: the three direct message arms ----
by_arm <- lcv_study_2 |>
  group_by(treat) |>
  summarize(n = n(), n_signed = sum(signed), n_tweeted = sum(tweeted), .groups = "drop") |>
  mutate(arm = recode_values(as.character(treat),
                             "pub" ~ "Public tweet",
                             "org" ~ "Organizer DM",
                             "fol" ~ "Follower DM")) |>
  select(arm, n, n_signed, n_tweeted) |>
  slice(match(c("Public tweet", "Organizer DM", "Follower DM"), arm)) |>
  bind_rows(summarize(lcv_study_2, arm = "Total", n = n(),
                      n_signed = sum(signed), n_tweeted = sum(tweeted)))

# Lower panel: the second stage, which only signers entered ----
signers <- filter(lcv_study_2, signed == 1)

by_button <- signers |>
  group_by(tweetbutton) |>
  summarize(n = n(), n_signed = sum(signed), n_tweeted = sum(tweeted), .groups = "drop") |>
  mutate(arm = if_else(tweetbutton == 1,
                       "Signers: tweet encouragement", "Signers: no encouragement")) |>
  select(arm, n, n_signed, n_tweeted) |>
  slice(match(c("Signers: tweet encouragement", "Signers: no encouragement"), arm)) |>
  bind_rows(summarize(signers, arm = "Signers: total", n = n(),
                      n_signed = sum(signed), n_tweeted = sum(tweeted)))

table_3 <- bind_rows(by_arm, by_button) |>
  mutate(pct_signed = 100 * n_signed / n, pct_tweeted = 100 * n_tweeted / n) |>
  select(arm, n, n_signed, pct_signed, n_tweeted, pct_tweeted)

write_csv(table_3, here::here("maintained", "output", "table_3_study2_outcomes.csv"))

table_3 |>
  mutate(
    signed = str_glue("{n_signed} ({sprintf('%.1f', pct_signed)})"),
    tweeted = str_glue("{n_tweeted} ({sprintf('%.1f', pct_tweeted)})")
  ) |>
  select(arm, n, signed, tweeted) |>
  kable(format = "latex", booktabs = TRUE,
        col.names = c("Treatment group", "N", "Signed (\\%)", "Tweeted (\\%)"),
        escape = FALSE, caption = "Study 2: design and outcomes.") |>
  write_lines(here::here("maintained", "output", "table_3_study2_outcomes.tex"))
