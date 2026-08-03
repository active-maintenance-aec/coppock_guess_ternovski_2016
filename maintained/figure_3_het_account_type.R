# coppock_guess_ternovski_2016/maintained/figure_3_het_account_type.R
# Output: output/figure_3_het_account_type.pdf, output/figure_3_het_account_type.png,
#   output/figure_3_het_account_type.csv
# Depends on: clean_lcv.R output, helpers.R
# Description: Figure 3: conditional average treatment effects of the two direct message
#   treatments on signing and on tweeting, estimated separately within each account type.
source(here::here("maintained", "helpers.R"))

lcv_study_1 <- read_rds(here::here("maintained", "output", "lcv_study_1.rds"))
lcv_study_2 <- read_rds(here::here("maintained", "output", "lcv_study_2.rds"))

# Conditional effects ----
# The published figure plots conventional standard errors, not the HC2 errors the
# tables use, so se_type = "classical" is what reproduces it. Elsewhere in this
# rewrite lm_robust runs at its HC2 default.
cate_by_type <- function(data, dv, study) {
  data |>
    nest(.by = account_type) |>
    mutate(fit = map(data, \(d) tidy(lm_robust(as.formula(paste(dv, "~ treat")),
                                               data = d, se_type = "classical")))) |>
    select(account_type, fit) |>
    unnest(fit) |>
    filter(term != "(Intercept)") |>
    mutate(dv = dv, study = study)
}

gg_df <- bind_rows(
  cate_by_type(lcv_study_1, "signed", "Study 1"),
  cate_by_type(lcv_study_1, "tweeted", "Study 1"),
  cate_by_type(lcv_study_2, "signed", "Study 2"),
  cate_by_type(lcv_study_2, "tweeted", "Study 2")
) |>
  mutate(
    treatment = if_else(term == "treatfol", "Follower", "Organizer"),
    account_label = factor(account_type,
                           levels = c("unknown", "org", "male", "female"),
                           labels = c("Unknown", "Organization", "Male", "Female")),
    outcome_label = factor(dv, levels = c("signed", "tweeted"),
                           labels = c("Signed", "Tweeted"))
  ) |>
  select(study, outcome_label, account_label, treatment, estimate, std.error,
         conf.low, conf.high)

write_csv(gg_df, here::here("maintained", "output", "figure_3_het_account_type.csv"))

# Direct labels replace the legend, placed once in the upper left panel to the
# right of the two estimates for unknown accounts.
label_df <- gg_df |>
  filter(study == "Study 1", outcome_label == "Signed", account_label == "Unknown")

figure_3 <- ggplot(gg_df, aes(x = estimate, y = account_label, shape = treatment)) +
  geom_vline(xintercept = 0, colour = "grey50", linetype = 2) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5), fill = "white", size = 2) +
  geom_text(data = label_df, aes(x = conf.high, label = treatment),
            position = position_dodge(width = 0.5),
            hjust = -0.15, size = 3) +
  scale_shape_manual(values = c(Follower = 19, Organizer = 21)) +
  scale_x_continuous(limits = c(-0.025, 0.10)) +
  labs(x = "Conditional average treatment effect", y = "Account type") +
  facet_grid(study ~ outcome_label) +
  theme_bw() +
  theme(legend.position = "none")

ggsave(here::here("maintained", "output", "figure_3_het_account_type.pdf"),
       plot = figure_3, width = 7, height = 5)
ggsave(here::here("maintained", "output", "figure_3_het_account_type.png"),
       plot = figure_3, width = 7, height = 5, dpi = 300)
