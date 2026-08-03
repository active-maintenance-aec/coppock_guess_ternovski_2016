# coppock_guess_ternovski_2016/maintained/in_text_claims.R
# Output: printed to stdout
# Depends on: every table, figure and text script's output in maintained/output/
# Description: Every number the article prints, paired with the sentence or the float
#   cell it comes from, computed here from maintained/output/ and printed in the units
#   and to the precision the article uses.
#
#   This file recomputes. It does not read the ground truth, and it repeats none of the
#   ground truth's filtering, unit conversion or rounding. Two independent paths run
#   from the same pipeline outputs to the same claimed number, and a disagreement
#   between them is a finding about one of the two.
#
#   Every block carries a "covers:" line naming the published claims it prints, as a
#   claim_id or a prefix followed by *. build_ground_truth.R reads those lines and stops
#   if any pipeline or descriptive claim in ground_truth/published_claims.csv has no
#   block here, so coverage is a gate rather than an intention.
#
#   cat() is used throughout, which is allowed here and nowhere else: the whole output
#   is a human-read audit trail and every value needs a visible label beside it.
source(here::here("maintained", "helpers.R"))

options(width = 200)

out <- function(f) read_csv(here::here("maintained", "output", f), show_col_types = FALSE)

# Printing helpers ----
# A claim prints its label and its value on one line, and a claim the deposit cannot
# support says so in the same place rather than being silently absent. No published
# number is typed anywhere in this file outside the block comments: the sentence is
# the anchor, the printed value is always computed.
say <- function(label, value) cat(sprintf("%-72s %s\n", paste0(label, ":"), value))
unsupported <- function(label, reason) {
  cat(sprintf("%-72s no counterpart in the deposit: %s\n", paste0(label, ":"), reason))
}
checked <- function(label, source) {
  cat(sprintf("%-72s not a pipeline quantity, checked against %s\n",
              paste0(label, ":"), source))
}
heading <- function(x) cat("\n", x, "\n", strrep("-", nchar(x)), "\n", sep = "")

table_2 <- out("table_2_study1_outcomes.csv")
table_3 <- out("table_3_study2_outcomes.csv")
table_4 <- out("table_4_study1_dm_effects.csv")
table_5 <- out("table_5_study1_encouragement.csv")
table_6 <- out("table_6_study1_network.csv")
table_7 <- out("table_7_study2_dm_effects.csv")
table_8 <- out("table_8_study2_encouragement.csv")
table_9 <- out("table_9_study2_network.csv")
table_a2 <- out("table_a2_study1_balance.csv")
table_a3 <- out("table_a3_study2_balance.csv")
table_a4 <- out("table_a4_study1_heterogeneity.csv")
table_a5 <- out("table_a5_study2_heterogeneity.csv")
figure_3 <- out("figure_3_het_account_type.csv")
summary_stats <- out("text_summary_stats.csv")
effects <- out("text_treatment_effects.csv")
descriptive <- out("text_descriptive_claims.csv")
sampler <- out("text_sampler_reproducibility.csv")

# Each accessor names a model and a term, so a block cannot read the wrong row and
# print a right-looking number by coincidence: an empty selection stops the script.
coefficient <- function(d, model, term) {
  hit <- d |> filter(model == !!model, term == !!term) |> pull(estimate)
  stopifnot(length(hit) == 1, !is.na(hit))
  hit
}
standard_error <- function(d, model, term) {
  hit <- d |> filter(model == !!model, term == !!term) |> pull(std.error)
  stopifnot(length(hit) == 1)
  hit
}
p_value <- function(d, model, term) {
  hit <- d |> filter(model == !!model, term == !!term) |> pull(p.value)
  stopifnot(length(hit) == 1, !is.na(hit))
  hit
}
stat <- function(study, pattern) {
  hit <- summary_stats |> filter(study == !!study, str_detect(quantity, pattern)) |> pull(value)
  stopifnot(length(hit) == 1)
  hit
}
pp <- function(study, pattern) {
  hit <- effects |> filter(study == !!study, str_detect(quantity, pattern)) |> pull(estimate)
  stopifnot(length(hit) == 1)
  hit
}
verdict <- function(claim_id) {
  hit <- descriptive |> filter(claim_id == !!claim_id)
  stopifnot(nrow(hit) == 1)
  paste0(if_else(hit$holds == 1, "HOLDS", "DOES NOT HOLD"), ". ", hit$evidence)
}

# A regression table prints coefficients over standard errors with N and R squared
# underneath, so one block per float rebuilds the published column rather than
# printing 60 separate lines.
published_column <- function(d, model, terms) {
  tibble(term = terms) |>
    mutate(
      cell = map_chr(term, function(g) {
        b <- coefficient(d, model, g)
        s <- standard_error(d, model, g)
        sprintf("%.3f (%s)", b, if_else(is.na(s), "NaN", sprintf("%.3f", s)))
      }),
      n = d |> filter(model == !!model) |> distinct(nobs) |> pull(nobs),
      r2 = sprintf("%.3f", d |> filter(model == !!model) |> distinct(r_squared) |> pull(r_squared))
    )
}

# Section 4: The organization and its network ----

# covers: figure_1|network_members|n text|study1_network_members|n text|study2_network_members|n
# "For Study 1, we constructed our universe of subjects by scraping the Twitter ID
#  numbers of LCV's followers, excluding those who had more than 5000 followers of
#  their own. ... The resulting network contained 6687 members. For Study 2, conducted
#  five months later, we repeated this procedure and obtained a network with 8507
#  members." Figure 1's caption repeats the first: "An illustration of the network of
#  6687 followers of the advocacy organization's Twitter account scraped before Study 1."
heading("Section 4: The organization and its network")
say("Study 1 network members", stat("Study 1", "^Network members$"))
say("Study 2 network members", stat("Study 2", "^Network members$"))
say("Figure 1 caption, followers illustrated", stat("Study 1", "^Network members$"))

# covers: figure_1|possible_connections|n figure_1|observed_edges|n figure_1|graph_density|value
# covers: figure_1|median_indegree|n figure_1|communities|n figure_1|modularity|value
# covers: figure_1|max_outdegree|n figure_2|mean_tweets_per_weekday|value
# covers: figure_2|sd_tweets_per_weekday|value table_1|* text|intercoder_kappa|value
# "out of a possible 44,709,282 connections between nodes, there were 131,474 such
#  edges when the network was scraped before Study 1, yielding a graph density of
#  0.0029." / "The Walktrap algorithm (with standard defaults) finds 22 communities in
#  the network, for a modularity of 0.3." / "LCV's Twitter account is fairly active,
#  sending out an average of 6.07 tweets and retweets per weekday from February 2013 to
#  February 2015. However ... day-to-day variation in the number of tweets posted is
#  high (s.d. = 10.05)." / "The mean number of followers among this group, 118,988, is
#  an order of magnitude higher than LCV's number of followers, and average tweet
#  frequency, about 13 per weekday, is slightly more than twice as high."
# The deposit is two experimental data frames, three scripts and a README. It carries
# no edge list, no daily tweet series and no account panel, so none of these can be
# recomputed from it.
edge_list <- "the network edge list is not deposited"
walk(c("Possible connections between nodes", "Observed edges", "Graph density",
       "Median in-degree within the network", "Walktrap communities", "Modularity",
       "Largest out-degree observed"),
     \(label) unsupported(label, edge_list))
walk(c("LCV tweets and retweets per weekday, mean",
       "LCV tweets and retweets per weekday, s.d."),
     \(label) unsupported(label, "the daily tweet series is not deposited"))
walk(c("Table 1, top ten organizations, 33 cells",
       "Mean followers among the top ten organizations",
       "Mean tweets per weekday among the top ten",
       "Median account creation year, top ten organizations"),
     \(label) unsupported(label, "the account panel is not deposited"))
unsupported("Intercoder reliability of the account type coding",
            "only the resolved account type is deposited, not the two coders' assignments")

# Section 5: Research design ----

# covers: text|twitter_char_limit|n text|study1_follower_cap|n text|twitter_dm_limit|n
# covers: text|study1_dm_batches|n text|study2_dm_batches|n text|n_treatment_conditions|n
# covers: text|study2_months_later|n text|ci_level|percent text|intercoder_sample|n
# covers: text|omnibus_permutations|n text|projected_dms_per_day|n text|projected_days|n
# covers: text|projected_signatures|n
# "The atomic unit of Twitter is the tweet, usually restricted to 140 characters" /
#  "we constructed our universe of subjects by scraping the Twitter ID numbers of LCV's
#  followers, excluding those who had more than 5000 followers of their own" / footnote
#  6: "Twitter's API limits the number of DMs that an application can send to 250 per
#  day." / "the DMs, which had to be sent in 12 daily batches" / "DMs were sent in 20
#  batches beginning that day" / "subjects were randomly assigned to one of five
#  treatment conditions" / "For Study 2, conducted five months later, we repeated this
#  procedure" / "on a sample of 200 profiles, our inter-coder reliability was" /
#  "conditional average treatment effects (CATEs) and 95 % confidence intervals are
#  presented" / "If an organization were to send out 250 direct messages (the maximum)
#  per day for 30 days, they could expect to collect 250 x 30 x 0.04 = 300 signatures
#  over the course of a month."
# Definitional and structural numbers, none of which is an estimate. Most cannot drift
# when the pipeline moves and are verified once against the source named beside them.
# The two that maintained/output/ does record are read back out of it instead.
heading("Section 5: Research design (definitional and structural)")
checked("Twitter's character limit on a tweet", "Twitter's published platform limit")
checked("Twitter's API limit on direct messages per day", "the article's own footnote 6")
checked("Follower count above which users were excluded",
        "the sampling rule stated in Section 4")
checked("Daily batches the direct messages were sent in, Study 1 and Study 2",
        "the design description in Section 5")
checked("Treatment conditions a subject could be assigned to",
        "the five arms of Tables 2 and 3")
checked("Months between the two studies", "the February and July 2014 field dates")
checked("Profiles double-coded for the intercoder reliability check",
        "the coding procedure described in Section 8")
checked("Discussion illustration: direct messages per day, days, signatures",
        "the article's own arithmetic on a rounded effect")

# The interval Figure 3 plots is a t interval, so its half width in standard errors
# moves a little with each fit's residual degrees of freedom. Reading the level back off
# the plotted bounds is what turns the caption's "95 %" into a checked number rather
# than an asserted one.
half_width <- (figure_3$conf.high - figure_3$estimate) / figure_3$std.error
say("Confidence level implied by the intervals Figure 3 plots, percent",
    sprintf("%.1f", 100 * (2 * pnorm(mean(half_width)) - 1)))
say("Half width of those intervals in standard errors, range",
    paste(sprintf("%.3f", range(half_width)), collapse = " to "))
say("Permutations drawn for each omnibus balance test",
    paste(unique(sampler$sims), collapse = ", "))

# covers: table_2|*
# Table 2, Study 1 design and outcomes.
heading("Table 2: Study 1 design and outcomes")
table_2 |>
  transmute(`Treatment group` = arm, N = n,
            `Signed (%)` = sprintf("%d (%.1f)", n_signed, pct_signed),
            `Tweeted (%)` = sprintf("%d (%.1f)", n_tweeted, pct_tweeted)) |>
  print(n = 20)

# covers: text|study1_button_imbalance_p|p_value text|d_encouragement_one_third|holds
# "The initial randomization procedure assigned one-third of the subjects in each of the
#  DM conditions to be shown a tweet encouragement after submitting the petition
#  signature, using complete random assignment." and "Table 2 also reveals an anomalous
#  finding that suggests a potential issue with the randomization: assignment to be
#  shown the tweet encouragement predicted petition signatures (p < .01)."
say("Encouragement assignment predicting signing, Study 1, p",
    signif(effects |> filter(study == "Study 1",
                             str_detect(quantity, "assignment predicting signing")) |>
             pull(p_value), 2))
say("One third shown the encouragement in each DM arm",
    verdict("text|d_encouragement_one_third|holds"))

# covers: text|study1_tweets_total|n text|d_study1_pub_zero_signed|holds
# "Table 2 contains some indication that this assumption is not wholly unwarranted:
#  Despite 65 tweets of the link throughout the network, a grand total of zero subjects
#  in the public tweet condition signed the petition."
say("Tweets of the link throughout the Study 1 network", stat("Study 1", "^Tweets of the link"))
say("Zero signatures in the Study 1 public tweet arm",
    verdict("text|d_study1_pub_zero_signed|holds"))

# covers: text|study1_signers|n text|study1_second_stage_pool|n
# "The petition received 109 signatures; these 109 users were followed by a total of
#  1176 other LCV followers. The 1176 were the pool of subjects randomly assigned to the
#  condition of following someone exposed to the tweet encouragement."
say("Study 1 petition signatures", stat("Study 1", "^Petition signers$"))
say("Study 1 followers of signers", stat("Study 1", "positive exposure probability"))

# covers: text|study1_outside_retweeters|n text|study2_outside_retweeters|n
# covers: text|lcv_follower_followers|n
# "There is one significant exception to this finding, however: in Study 1, five users
#  not in the LCV's network retweeted either the public tweet or a tweet from one of the
#  followers. In Study 2 this number was 7. With over seven million total followers of
#  the LCV's followers, these magnitudes are minuscule, but they are greater than zero."
walk(c("Retweeters outside the LCV network, Study 1",
       "Retweeters outside the LCV network, Study 2"),
     \(label) unsupported(label, "both data frames hold only LCV followers"))
unsupported("Total followers of the LCV's followers",
            "the follower-of-follower counts are not deposited")

# covers: table_3|* text|study2_signers|n text|study2_second_stage_pool|n
# "Between them, the 221 petition signers were followed by 1990 other users, who
#  constitute the subjects of the second-stage experiment in Study 2."
heading("Table 3: Study 2 design and outcomes")
table_3 |>
  transmute(`Treatment group` = arm, N = n,
            `Signed (%)` = sprintf("%d (%.1f)", n_signed, pct_signed),
            `Tweeted (%)` = sprintf("%d (%.1f)", n_tweeted, pct_tweeted)) |>
  print(n = 20)
say("Study 2 petition signatures", stat("Study 2", "^Petition signers$"))
say("Study 2 followers of signers", stat("Study 2", "positive exposure probability"))

# Section 6: Study 1 results ----

# covers: table_4|* text|study1_fol_signed_pp|estimate text|study1_org_signed_pp|estimate
# covers: text|study1_p_fol_vs_org_signed|p_value text|study1_any_dm_signed_pp|estimate
# covers: text|d_study1_dm_significant|holds
# "As the first two columns of Table 4 show, the 'follower' and 'organizer' treatments
#  both had positive and significant effects at the p < .01 level. The follower DM
#  caused an estimated 3.9-percentage-point increase in the proportion of subjects who
#  signed the petition. The organizer DM caused a 3.3-percentage-point increase in
#  participation; these effects are not significantly different from each other
#  (p = 0.38). Thus, the best interpretation of the evidence from Study 1 is that
#  receiving any direct message (after potentially seeing a similar public tweet) caused
#  a 3.6-percentage-point increase in petition signing."
heading("Section 6: Study 1, main effects (Table 4)")
walk(c("M1 signed", "M2 signed adjusted", "M3 tweeted", "M4 tweeted adjusted"), function(m) {
  terms <- table_4 |> filter(model == m) |> pull(term)
  cat("\n", m, "\n", sep = "")
  print(published_column(table_4, m, terms), n = 20)
})
say("Follower DM effect on signing, percentage points",
    sprintf("%.1f", 100 * coefficient(table_4, "M1 signed", "treatfol")))
say("Organizer DM effect on signing, percentage points",
    sprintf("%.1f", 100 * coefficient(table_4, "M1 signed", "treatorg")))
say("Follower against organizer, signing, p",
    sprintf("%.2f", p_value(table_4, "Follower vs organizer, signed", "treatorg")))
say("Any direct message effect on signing, percentage points",
    sprintf("%.1f", pp("Study 1", "any direct message on signing")))
say("Both DM effects positive and significant at p < .01",
    verdict("text|d_study1_dm_significant|holds"))

# covers: text|study1_fol_tweeted_pp|estimate text|study1_org_minus_fol_tweeted_pp|estimate
# covers: text|study1_p_fol_vs_org_tweeted|p_value
# "The 'organizer' message caused fewer tweets than the 'follower' message: 1.1
#  percentage points fewer than the 2.7 percentage point boost generated by the follower
#  message among subjects assigned to the DM conditions (p = 0.03)."
say("Follower DM effect on tweeting, percentage points",
    sprintf("%.1f", 100 * coefficient(table_4, "M3 tweeted", "treatfol")))
say("Organizer minus follower, tweeting, percentage points",
    sprintf("%.1f", abs(pp("Study 1", "Organizer minus follower"))))
say("Follower against organizer, tweeting, p",
    sprintf("%.2f", p_value(table_4, "Follower vs organizer, tweeted", "treatorg")))

# covers: table_5|* text|d_study1_encouragement_half|holds
# covers: text|d_study1_organizer_38pp|holds text|d_study1_follower_twice|holds
# "Table 5 shows that the among those who completed the petition, the effect of being
#  shown a tweet button was large: the treatment caused nearly half of exposed subjects
#  to click and tweet a message about the petition to their followers." and "being
#  primed as an 'organizer' caused the encouragement to be less effective to the tune of
#  more than 38 percentage points. In other words, the 'follower' prime was more than
#  twice as effective at encouraging future tweets as the 'organizer' prime."
heading("Section 6: Study 1, tweet encouragement (Table 5)")
walk(c("M1", "M2"), function(m) {
  terms <- table_5 |> filter(model == m) |> pull(term)
  cat("\n", m, "\n", sep = "")
  print(published_column(table_5, m, terms), n = 20)
})
say("Encouragement effect is nearly half", verdict("text|d_study1_encouragement_half|holds"))
say("Organizer prime costs more than 38 percentage points",
    verdict("text|d_study1_organizer_38pp|holds"))
say("Follower prime more than twice as effective",
    verdict("text|d_study1_follower_twice|holds"))

# covers: table_6|* text|study1_first_stage_pp|estimate text|d_study1_network_null|holds
# "Column 1 of Table 6 shows that this manipulation was quite effective, those who
#  followed exposed subjects were 63 percentage points more likely to have seen a
#  retweeted message. However, despite being potentially exposed to retweets, columns
#  two through five show that treated subjects were not significantly more likely to
#  sign or tweet the message themselves."
heading("Section 6: Study 1, network effects (Table 6)")
walk(unique(table_6$model), function(m) {
  terms <- table_6 |> filter(model == m) |> pull(term)
  cat("\n", m, "\n", sep = "")
  print(published_column(table_6, m, terms), n = 20)
})
say("First stage, exposure to a retweeted message, percentage points",
    sprintf("%.0f", 100 * coefficient(table_6, "M1 shown tweet, OLS", "exposure")))
say("Columns 2 to 5 not significant", verdict("text|d_study1_network_null|holds"))

# covers: text|footnote_11_zero_clicks|percent
# "Suppose that the true treatment effect is that a public tweet generates a single
#  click per 10,000 followers exposed. With a sample size of 6687, we would expect to
#  observe zero clicks about 51 % of the time."
say("Footnote 11, chance of observing zero clicks, percent",
    sprintf("%.0f", stat("Study 1", "zero clicks")))

# Section 7: Study 2 results ----

# covers: table_7|* text|study2_fol_signed_pp|estimate text|study2_org_signed_pp|estimate
# covers: text|study2_p_fol_vs_org_signed|p_value text|d_study2_larger|holds
# covers: text|d_study2_follower_more_effective|holds
# "The 'follower' and 'organizer' messages boosted petition signatures by 4.6 and 4.3
#  percentage points, respectively (see Table 7). The effects of the direct messages on
#  signing were not significantly different from each other (p = 0.60). ... As in Study
#  1, priming the 'follower' identity was more effective than the 'organizer' identity,
#  though the difference is no longer statistically significant."
heading("Section 7: Study 2, main effects (Table 7)")
walk(c("M1 signed", "M2 signed adjusted", "M3 tweeted", "M4 tweeted adjusted"), function(m) {
  terms <- table_7 |> filter(model == m) |> pull(term)
  cat("\n", m, "\n", sep = "")
  print(published_column(table_7, m, terms), n = 20)
})
say("Follower DM effect on signing, percentage points",
    sprintf("%.1f", 100 * coefficient(table_7, "M1 signed", "treatfol")))
say("Organizer DM effect on signing, percentage points",
    sprintf("%.1f", 100 * coefficient(table_7, "M1 signed", "treatorg")))
say("Follower against organizer, signing, p",
    sprintf("%.2f", p_value(table_7, "Follower vs organizer, signed", "treatorg")))
say("Study 2 effects larger than Study 1", verdict("text|d_study2_larger|holds"))
say("Follower prime more effective, difference not significant",
    verdict("text|d_study2_follower_more_effective|holds"))

# covers: table_8|* text|study2_button_tweeted_pp|estimate
# covers: text|study2_encouragement_fol_pp|estimate text|study2_encouragement_org_pp|estimate
# "The tweet link caused a 35.0-percentage-point increase in tweeting behavior in the
#  restricted model shown in Table 8, but, as in Study 1, there were differential effects
#  by DM condition: the button increased tweets by 47.2 percentage points among subjects
#  sent the 'follower' message and by 23.1 percentage points among subjects sent the
#  'organizer' message."
heading("Section 7: Study 2, tweet encouragement (Table 8)")
walk(c("M1", "M2"), function(m) {
  terms <- table_8 |> filter(model == m) |> pull(term)
  cat("\n", m, "\n", sep = "")
  print(published_column(table_8, m, terms), n = 20)
})
say("Tweet encouragement effect, restricted model, percentage points",
    sprintf("%.1f", 100 * coefficient(table_8, "M1", "tweetbutton")))
say("Encouragement effect in the follower arm, percentage points",
    sprintf("%.1f", pp("Study 2", "in the fol arm")))
say("Encouragement effect in the organizer arm, percentage points",
    sprintf("%.1f", pp("Study 2", "in the org arm")))

# covers: table_9|* text|study2_itt_signed_pp|estimate text|study2_cace_signed_pp|estimate
# covers: text|d_study2_tweeted_network_null|holds text|d_study2_network_larger_than_main|holds
# "Column 2 shows an ITT effect of the tweet encouragement of 2.0 percentage points.
#  Column 3 shows the estimated effect among compliers: subjects who followed others who
#  tweet if and only if they are shown the tweet encouragement were 5.6 percentage
#  points more likely to sign the petition. ... Columns 4 and 5 repeat the analyses for
#  the 'tweeted' dependent variable: we observe no significant differences by exposure
#  condition at the p < 0.05 level."
heading("Section 7: Study 2, network effects (Table 9)")
walk(unique(table_9$model), function(m) {
  terms <- table_9 |> filter(model == m) |> pull(term)
  cat("\n", m, "\n", sep = "")
  print(published_column(table_9, m, terms), n = 20)
})
say("ITT effect on signing, percentage points",
    sprintf("%.1f", 100 * coefficient(table_9, "M2 signed, OLS", "exposure")))
say("Complier effect on signing, percentage points",
    sprintf("%.1f", 100 * coefficient(table_9, "M3 signed, IV", "actual_exposure")))
say("Tweeting columns not significant at p < 0.05",
    verdict("text|d_study2_tweeted_network_null|holds"))
say("Network effect larger than the direct contact effect",
    verdict("text|d_study2_network_larger_than_main|holds"))

# Section 8: Heterogeneous effects ----

# covers: figure_3|study1_share_male|percent figure_3|study1_share_female|percent
# covers: figure_3|study1_share_org|percent figure_3|study1_share_unknown|percent
# covers: figure_3|study2_share_male|percent figure_3|study2_share_female|percent
# covers: figure_3|study2_share_org|percent figure_3|study2_share_unknown|percent
# "Fig. 3 Entries are conditional differences-in-means with 95 % confidence intervals.
#  In Study 1, the sample was 38.4 % male, 30.4 % female, 24.4 % organizations, and
#  6.8 % unknown; in Study 2, the sample was 39.3 % male, 32.9 % female, 22.1 %
#  organizations, and 5.7 % unknown"
heading("Section 8: Figure 3 caption, sample composition")
summary_stats |>
  filter(str_detect(quantity, "^Share of sample")) |>
  transmute(study, account_type = str_remove(quantity, "^Share of sample: "),
            percent = sprintf("%.1f", value)) |>
  print(n = 10)

# covers: figure_3|d_study1_org_smaller_signed|holds figure_3|d_study1_no_het_tweeted|holds
# covers: figure_3|d_study2_org_smaller_signed|holds figure_3|d_no_gender_pattern|holds
# "In Study 1, we observe some treatment effect heterogeneity on the 'signed' dependent
#  variable: treatment effects are much smaller for organizations compared to
#  individuals. We observe no such heterogeneity for the 'tweeted' dependent variable.
#  The second row presents the estimates for Study 2. We see nearly the identical
#  pattern: on the 'signed' dependent variable, organizations have much smaller
#  treatment effects than individuals, but this difference is not apparent for the
#  'tweeted' dependent variable. Interestingly, there is no consistent pattern for the
#  relative size of treatment effects among men and women; the treatments appear to work
#  equally well for both, regardless of dependent variable."
say("Study 1 signing, organizations smallest",
    verdict("figure_3|d_study1_org_smaller_signed|holds"))
say("Study 1 tweeting, no heterogeneity", verdict("figure_3|d_study1_no_het_tweeted|holds"))
say("Study 2 signing, organizations smallest",
    verdict("figure_3|d_study2_org_smaller_signed|holds"))
say("No consistent pattern by gender", verdict("figure_3|d_no_gender_pattern|holds"))

# covers: figure_3|plotted|count
# Figure 3 itself prints no numbers. Its plotted conditional effects are listed so the
# figure can be read against the panel it appears in.
heading("Figure 3: plotted conditional effects")
figure_3 |>
  transmute(study, outcome_label, account_label, treatment,
            cell = sprintf("%.3f (%.3f)", estimate, std.error)) |>
  print(n = 40)

# Section 9: Discussion ----

# covers: text|d_public_tweet_null|holds text|d_dm_click_4pp|holds
# covers: text|d_dm_signatures_exceed_tweets|holds text|d_button_35_to_45|holds
# "In both of our experiments, not a single subject assigned to be exposed only to the
#  public tweet signed or retweeted the petition. We find that DMs produce approximately
#  a 4-percentage-point increase in clicks." and "DMs of both types cause an increase in
#  petition signatures greater than the effect on overall tweets to the petition link.
#  However, compared to the effect of randomly assigning subjects who had already
#  completed the petition to see the tweet button (35 to 45 percentage points) ..."
heading("Section 9: Discussion")
say("No public tweet subject signed or retweeted", verdict("text|d_public_tweet_null|holds"))
say("Direct messages produce about 4 percentage points", verdict("text|d_dm_click_4pp|holds"))
say("Signing effect exceeds tweeting effect",
    verdict("text|d_dm_signatures_exceed_tweets|holds"))
say("Tweet button effect between 35 and 45 percentage points",
    verdict("text|d_button_35_to_45|holds"))

# Online Appendix 1: possible subject types ----

# covers: table_a1|*
# "Together, types 5 though 7 account for approximately 3.6% of the population in Study
#  1 and approximately 4.5% of the population in Study 2; type 8 accounts for the
#  remainder." Table A1 gives the type 8 proportion as the interval [0.955, 0.965], and
#  the first four types as exactly zero: "We know that the proportions of types 1
#  through 4 in the population are all equal to zero: no subjects in the public tweet
#  conditions signed the petitions."
heading("Online Appendix 1: possible subject types")
say("Types 5 to 7 combined, Study 1, percent",
    sprintf("%.1f", stat("Study 1", "Types 5 to 7")))
say("Types 5 to 7 combined, Study 2, percent",
    sprintf("%.1f", stat("Study 2", "Types 5 to 7")))
say("Type 8 proportion, Study 1", sprintf("%.4f", stat("Study 1", "Type 8")))
say("Type 8 proportion, Study 2", sprintf("%.4f", stat("Study 2", "Type 8")))
say("Types 1 to 4 are zero", verdict("table_a1|d_types_1_to_4_zero|holds"))

# Online Appendix 2: randomization checks ----

# covers: table_a2|* table_a3|*
# The two balance tables print a mean and a standard error per covariate per arm, a
# test of independence per covariate, and one omnibus randomization inference p-value.
heading("Online Appendix 2, Table A2: Study 1 balance")
table_a2 |>
  filter(covariate != "omnibus") |>
  transmute(label, treat,
            cell = if_else(covariate == "n", format(mean),
                           sprintf("%.3f (%.3f)", mean, se)),
            p = if_else(is.na(p_value), "", sprintf("%.3f", p_value))) |>
  print(n = 30)
say("Table A2 omnibus p-value",
    sprintf("%.3f", table_a2 |> filter(covariate == "omnibus") |> pull(mean)))

heading("Online Appendix 2, Table A3: Study 2 balance")
table_a3 |>
  filter(covariate != "omnibus") |>
  transmute(label, treat,
            cell = if_else(covariate == "n", format(mean),
                           sprintf("%.3f (%.3f)", mean, se)),
            p = if_else(is.na(p_value), "", sprintf("%.3f", p_value))) |>
  print(n = 30)
say("Table A3 omnibus p-value",
    sprintf("%.3f", table_a3 |> filter(covariate == "omnibus") |> pull(mean)))
say("Table A2 omnibus under the pre-R-3.6 sampler",
    sprintf("%.3f", sampler |> filter(table_figure == "table_a2") |> pull(old_sampler)))
say("Table A3 omnibus under the pre-R-3.6 sampler",
    sprintf("%.3f", sampler |> filter(table_figure == "table_a3") |> pull(old_sampler)))

# Online Appendix 3: heterogeneous effects ----

# covers: table_a4|* table_a5|*
# Eight models per study, treatment interacted with one moderator at a time.
heading("Online Appendix 3, Table A4: Study 1 heterogeneous effects")
walk(unique(table_a4$model), function(m) {
  terms <- table_a4 |> filter(model == m) |> pull(term)
  cat("\n", m, "\n", sep = "")
  print(published_column(table_a4, m, terms), n = 20)
})

heading("Online Appendix 3, Table A5: Study 2 heterogeneous effects")
walk(unique(table_a5$model), function(m) {
  terms <- table_a5 |> filter(model == m) |> pull(term)
  cat("\n", m, "\n", sep = "")
  print(published_column(table_a5, m, terms), n = 20)
})

# Online Appendix 4: experimental materials ----

# covers: figures_a1_a6|*
# Six screenshots of the petitions, the tweet encouragement and the public tweets.
# No numbers and no data.
heading("Online Appendix 4: experimental materials")
unsupported("Figures A1 to A6, screenshots", "no numbers and no data")
