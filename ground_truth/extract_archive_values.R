# coppock_guess_ternovski_2016/ground_truth/extract_archive_values.R
# Output: ground_truth/archive_values.csv, ground_truth/archive_figure_3_values.csv,
#   ground_truth/archive_run_status.csv
# Depends on: original/ (the deposited archive), original_manifest.csv
# Description: Run the deposited archive in a scratch copy and read its own numbers out
#   of its own objects. Everything the ground truth records as value_script comes from
#   here, so no archive number is ever transcribed or reimplemented in the build script.
#
#   The deposit is never run in place. ARCHIVE_RUN_DIR names the working directory and
#   defaults to a folder inside tempdir(), so a stranger can run this file unchanged.
#   The deposit's two analysis scripts write four files when they run (a figure PNG, two
#   LaTeX fragments and one to a path on the authors' own machine), which is the reason
#   the copy exists.
#
#   Both scripts are run twice. As shipped, with source(), which records where each one
#   stops. Then expression by expression on a repaired copy, which records the status of
#   every top-level expression and leaves the fitted models behind to be read. Running
#   expression by expression is what separates a printing failure from an estimation
#   failure: the deposit's tables are built by stargazer calls that fail on current R
#   while the models they would have printed fit without complaint.

library(here)
library(tidyverse)

here::i_am("ground_truth/extract_archive_values.R")

# Where to run the deposit ----
archive_run_dir <- Sys.getenv("ARCHIVE_RUN_DIR",
                              unset = file.path(tempdir(), "cgt2016_archive_run"))
stopifnot(is.character(archive_run_dir), length(archive_run_dir) == 1,
          nzchar(archive_run_dir))

if (dir.exists(archive_run_dir)) unlink(archive_run_dir, recursive = TRUE)
dir.create(archive_run_dir, recursive = TRUE)

manifest <- read_csv(here::here("original_manifest.csv"), show_col_types = FALSE)
walk(manifest$file,
     \(f) file.copy(here::here("original", f), file.path(archive_run_dir, f)))
stopifnot(all(file.exists(file.path(archive_run_dir, manifest$file))))

# The deposit ships data, code and a README and no stored intermediates, so the copy
# stripped to data plus code is the copy: there is no result here that a script could
# load instead of computing. Asserted rather than described, because a deposit that did
# ship intermediates would let a script pass while the script feeding it had failed.
stopifnot(all(str_detect(manifest$file, "\\.(r|R|rdata|txt)$")))

main_script <- "LCV Experiments 1 and 2 analysis.R"
appendix_script <- "LCV Appendix analysis.R"

# Paths must not leak out of the scratch directory into a committed file ----
# tempdir() resolves through /private on macOS and can come back with a doubled
# slash, so the scrub is a pattern over the path separators rather than a literal.
scrub <- function(x) {
  pattern <- str_replace_all(str_escape(archive_run_dir), "/", "/+")
  x |>
    str_replace_all(paste0("(/private)?", pattern), "<archive run directory>") |>
    str_replace_all(str_escape(here::here()), "<repository>")
}

# Running the deposit ----
# An expression is labelled by what it does rather than by its position, so the
# report can count the stargazer calls that fail without anyone typing the number.
expression_kind <- function(text) {
  case_when(
    str_detect(text, "^stargazer\\(") ~ "stargazer call",
    str_detect(text, "^print\\.xtable\\(") ~ "xtable call",
    str_detect(text, "^ggsave\\(") ~ "ggsave call",
    str_detect(text, "^(library|require)\\(") ~ "package load",
    str_detect(text, "^(load|source)\\(") ~ "data or source load",
    str_detect(text, "^(hist|abline|plot)\\(") ~ "base graphics call",
    str_detect(text, "^for ?\\(") ~ "simulation loop",
    .default = "computation"
  )
}

run_expression_by_expression <- function(file, variant, env) {
  path <- file.path(archive_run_dir, file)
  exprs <- parse(path, keep.source = TRUE)
  first_line <- map_int(attr(exprs, "srcref"), \(s) as.integer(s)[1])
  text <- map_chr(exprs, \(e) paste(deparse(e), collapse = " "))

  # Named apart from the status column below: a tibble() column can see a column
  # defined above it, so reusing the name would leave every message reading "error".
  outcome <- map_chr(seq_along(exprs), function(i) {
    tryCatch({
      withCallingHandlers(eval(exprs[[i]], envir = env),
                          warning = function(w) invokeRestart("muffleWarning"))
      "ok"
    }, error = function(e) paste0("error: ", conditionMessage(e)))
  })

  tibble(file = file, variant = variant, expression = seq_along(exprs),
         first_line = first_line, kind = expression_kind(text),
         status = if_else(outcome == "ok", "ok", "error"),
         message = if_else(outcome == "ok", "", scrub(str_remove(outcome, "^error: "))))
}

run_as_shipped <- function(file) {
  path <- file.path(archive_run_dir, file)
  env <- new.env(parent = globalenv())
  outcome <- tryCatch({
    withCallingHandlers(source(path, local = env, chdir = TRUE),
                        warning = function(w) invokeRestart("muffleWarning"))
    list(status = "ok", message = "")
  }, error = function(e) list(status = "error", message = scrub(conditionMessage(e))))
  tibble(file = file, variant = "as shipped", expression = NA_integer_,
         first_line = NA_integer_, kind = "whole script",
         status = outcome$status, message = outcome$message)
}

# The repairs ----
# Two edits, both of them the smallest change that lets the expression run. Neither
# touches an estimate. The stargazer failures are left alone: they are printing
# failures, the models behind them fit, and expression-by-expression evaluation
# records each one without stopping the harvest.
repair <- function(file) {
  repaired <- str_replace(file, "\\.R$", " REPAIRED.R")
  read_lines(file.path(archive_run_dir, file)) |>
    str_replace_all(fixed("condition_names = c(\"pub\", \"fol\", \"org\")"),
                    "conditions = c(\"pub\", \"fol\", \"org\")") |>
    str_replace_all(fixed(paste0("file = \"/Users/Alex/Documents/Dropbox/Columbia/",
                                 "Collaboration/ai twitter experiments/PB R and R/",
                                 "CGT Twitter RandR Revised Version/exp2balance.tex\"")),
                    "file = \"exp2balance.tex\"") |>
    write_lines(file.path(archive_run_dir, repaired))
  repaired
}

main_repaired <- repair(main_script)
appendix_repaired <- repair(appendix_script)

# The repair has to have changed something, or the edits above have gone stale
# against the deposit and the "repaired" run is the shipped run under another name.
stopifnot(
  !identical(read_lines(file.path(archive_run_dir, main_script)),
             read_lines(file.path(archive_run_dir, main_repaired))) ||
    !identical(read_lines(file.path(archive_run_dir, appendix_script)),
               read_lines(file.path(archive_run_dir, appendix_repaired))),
  !identical(read_lines(file.path(archive_run_dir, appendix_script)),
             read_lines(file.path(archive_run_dir, appendix_repaired)))
)

old_wd <- getwd()
setwd(archive_run_dir)

main_env <- new.env(parent = globalenv())
appendix_env <- new.env(parent = globalenv())

status_shipped <- map(c(main_script, appendix_script), run_as_shipped) |> list_rbind()
status_main <- run_expression_by_expression(main_repaired, "repaired", main_env)

# Snapshots inside the appendix run ----
# The appendix script analyses Study 1 and then Study 2 with the same object names, so
# the Study 1 values exist only between two points in the run. The cut points are found
# by searching the file rather than typed, so an edit to the deposit would move them.
appendix_lines <- read_lines(file.path(archive_run_dir, appendix_repaired))
cut_balance <- str_which(appendix_lines, fixed("## Experiment 2: Balance ####"))
cut_het <- str_which(appendix_lines, "^het_fit_1 <- ")[2]
stopifnot(length(cut_balance) == 1, !is.na(cut_het), cut_het > cut_balance)

appendix_exprs <- parse(file.path(archive_run_dir, appendix_repaired), keep.source = TRUE)
appendix_first_line <- map_int(attr(appendix_exprs, "srcref"), \(s) as.integer(s)[1])
snapshot_after_balance <- max(which(appendix_first_line < cut_balance))
snapshot_after_het <- max(which(appendix_first_line < cut_het))

study_1_balance <- NULL
study_1_het <- NULL
appendix_status <- map_chr(seq_along(appendix_exprs), function(i) {
  out <- tryCatch({
    withCallingHandlers(eval(appendix_exprs[[i]], envir = appendix_env),
                        warning = function(w) invokeRestart("muffleWarning"))
    "ok"
  }, error = function(e) paste0("error: ", conditionMessage(e)))
  if (i == snapshot_after_balance) {
    study_1_balance <<- mget(c("p_account_type", "p_num_followers", "p_num_days",
                               "p_centrality", "p_omnibus"), envir = appendix_env)
  }
  if (i == snapshot_after_het) {
    study_1_het <<- mget(paste0("het_fit_", 1:8), envir = appendix_env)
  }
  out
})

status_appendix <- tibble(
  file = appendix_repaired, variant = "repaired",
  expression = seq_along(appendix_exprs), first_line = appendix_first_line,
  kind = expression_kind(map_chr(appendix_exprs,
                                 \(e) paste(deparse(e), collapse = " "))),
  status = if_else(appendix_status == "ok", "ok", "error"),
  message = if_else(appendix_status == "ok", "",
                    scrub(str_remove(appendix_status, "^error: "))))

setwd(old_wd)

stopifnot(!is.null(study_1_balance), !is.null(study_1_het))

run_status <- bind_rows(status_shipped, status_main, status_appendix) |>
  mutate(file = str_remove(file, " REPAIRED"))

write_csv(run_status, here::here("ground_truth", "archive_run_status.csv"))

# Reading the deposit's own numbers out of its own objects ----
LCVexp1 <- get("LCVexp1", envir = main_env)
LCVexp2 <- get("LCVexp2", envir = main_env)
getcoefs <- get("getcoefs", envir = main_env)
getrobustses <- get("getrobustses", envir = main_env)
se_mean <- get("se_mean", envir = main_env)
fit_from <- function(env, name) get(name, envir = env)

value <- function(table_figure, row_label, quantity, value) {
  tibble(table_figure, row_label, quantity, value_script = as.numeric(value))
}

# getcoefs() and getrobustses() are the deposit's wrappers around
# coeftest(fit, vcovHC(fit, type = "HC2")), so every coefficient and standard error
# below is the archive's own, taken from the model object the archive fitted.
fit_rows <- function(table_figure, model, fit, term_map) {
  coefs <- getcoefs(fit)
  ses <- getrobustses(fit)
  bind_rows(
    map(names(term_map),
        \(g) value(table_figure, paste0(model, ":", g), "coef", coefs[[term_map[[g]]]])) |>
      list_rbind(),
    map(names(term_map),
        \(g) value(table_figure, paste0(model, ":", g), "se", ses[[term_map[[g]]]])) |>
      list_rbind(),
    value(table_figure, model, "n", length(residuals(fit))),
    value(table_figure, model, "r2", summary(fit)$r.squared)
  )
}

# Tables 2 and 3: design and outcomes ----
# The deposit prints these cells from a group_by chain it never assigns, so they are
# recomputed here from the deposited data frames with the deposit's own expressions.
descriptive <- function(table_figure, arms) {
  map(names(arms), function(label) {
    d <- arms[[label]]
    bind_rows(
      value(table_figure, label, "n", nrow(d)),
      value(table_figure, label, "n_signed", sum(d$signed)),
      value(table_figure, label, "pct_signed", 100 * mean(d$signed)),
      value(table_figure, label, "n_tweeted", sum(d$tweeted)),
      value(table_figure, label, "pct_tweeted", 100 * mean(d$tweeted))
    )
  }) |>
    list_rbind()
}

script_2 <- descriptive("table_2", list(
  pub = subset(LCVexp1, treat == "pub"),
  org_enc = subset(LCVexp1, treat == "org" & tweetbutton == 1),
  org_noenc = subset(LCVexp1, treat == "org" & tweetbutton == 0),
  fol_enc = subset(LCVexp1, treat == "fol" & tweetbutton == 1),
  fol_noenc = subset(LCVexp1, treat == "fol" & tweetbutton == 0),
  total = LCVexp1))

script_3 <- descriptive("table_3", list(
  pub = subset(LCVexp2, treat == "pub"),
  org = subset(LCVexp2, treat == "org"),
  fol = subset(LCVexp2, treat == "fol"),
  total = LCVexp2,
  signers_enc = subset(LCVexp2, signed == 1 & tweetbutton == 1),
  signers_noenc = subset(LCVexp2, signed == 1 & tweetbutton == 0),
  signers_total = subset(LCVexp2, signed == 1)))

# Tables 4 and 7: direct message effects ----
terms_unadjusted <- c(treat_fol = "as.factor(treat)fol", treat_org = "as.factor(treat)org",
                      constant = "(Intercept)")
terms_adjusted <- c(treat_fol = "as.factor(treat)fol", treat_org = "as.factor(treat)org",
                    account_male = "account_typemale", account_org = "account_typeorg",
                    account_unknown = "account_typeunknown",
                    centrality = "centrality_centered", followers = "num_followers_centered",
                    days = "days_on_twitter_centered", days_missing = "days_missingTRUE",
                    constant = "(Intercept)")

dm_effects <- function(table_figure, suffix) {
  bind_rows(
    fit_rows(table_figure, "m1", fit_from(main_env, paste0("treat.signed.", suffix)),
             terms_unadjusted),
    fit_rows(table_figure, "m2", fit_from(main_env, paste0("treat.signed.cov.", suffix)),
             terms_adjusted),
    fit_rows(table_figure, "m3", fit_from(main_env, paste0("treat.tweeted.", suffix)),
             terms_unadjusted),
    fit_rows(table_figure, "m4", fit_from(main_env, paste0("treat.tweeted.cov.", suffix)),
             terms_adjusted)
  )
}

script_4 <- dm_effects("table_4", "1")
script_7 <- dm_effects("table_7", "2")

# Tables 5 and 8: tweet encouragement ----
terms_button <- c(encouragement = "as.factor(tweetbutton)1", constant = "(Intercept)")
terms_button_by_arm <- c(encouragement = "as.factor(tweetbutton)1",
                         treat_org = "as.factor(treat)org",
                         encouragement_x_org = "as.factor(tweetbutton)1:as.factor(treat)org",
                         constant = "(Intercept)")

encouragement <- function(table_figure, suffix) {
  bind_rows(
    fit_rows(table_figure, "m1", fit_from(main_env, paste0("button.tweeted.", suffix)),
             terms_button),
    fit_rows(table_figure, "m2",
             fit_from(main_env, paste0("buttonbytreat.tweeted.", suffix)),
             terms_button_by_arm)
  )
}

script_5 <- encouragement("table_5", "1")
script_8 <- encouragement("table_8", "2")

# Tables 6 and 9: network effects ----
terms_ols <- c(exposure = "exposure", constant = "(Intercept)")
terms_iv <- c(actual_exposure = "actual_exposure", constant = "(Intercept)")

network <- function(table_figure, suffix) {
  bind_rows(
    fit_rows(table_figure, "m1", fit_from(main_env, paste0("exposure.", suffix)),
             terms_ols),
    fit_rows(table_figure, "m2", fit_from(main_env, paste0("exposure.signed.", suffix)),
             terms_ols),
    fit_rows(table_figure, "m3", fit_from(main_env, paste0("exposure.signed.iv.", suffix)),
             terms_iv),
    fit_rows(table_figure, "m4", fit_from(main_env, paste0("exposure.tweeted.", suffix)),
             terms_ols),
    fit_rows(table_figure, "m5", fit_from(main_env, paste0("exposure.tweeted.iv.", suffix)),
             terms_iv)
  )
}

script_6 <- network("table_6", "1")
script_9 <- network("table_9", "2")

# Tables A2 and A3: balance ----
# The deposit's balance_table is a character matrix already rounded to three decimals
# by its own format_num(), so the cells are recomputed from the deposited data with the
# deposit's se_mean(). The Study 1 and Study 2 blocks differ in one detail that is
# carried over rather than tidied: Study 1 takes the centrality mean with na.rm = TRUE
# and Study 2 without it.
balance_covariates <- c(account_female = "fem_dum", account_male = "male_dum",
                        account_org = "org_dum", account_unknown = "unk_dum",
                        num_followers = "num_followers", days_on_twitter = "days_on_twitter",
                        centrality = "centrality")

balance_cells <- function(table_figure, d, centrality_na_rm) {
  arms <- c("pub", "fol", "org")
  cells <- map(names(balance_covariates), function(label) {
    map(arms, function(arm) {
      x <- d[[balance_covariates[[label]]]][d$treat == arm]
      na_rm <- if (label == "centrality") centrality_na_rm else TRUE
      bind_rows(
        value(table_figure, paste0(label, ":", arm), "mean", mean(x, na.rm = na_rm)),
        value(table_figure, paste0(label, ":", arm), "se", se_mean(x))
      )
    }) |>
      list_rbind()
  }) |>
    list_rbind()
  ns <- map(arms, \(arm) value(table_figure, arm, "n", sum(d$treat == arm))) |> list_rbind()
  bind_rows(cells, ns)
}

script_a2 <- bind_rows(
  balance_cells("table_a2", LCVexp1, centrality_na_rm = TRUE),
  value("table_a2", "account_type", "p_value", study_1_balance$p_account_type),
  value("table_a2", "num_followers", "p_value", study_1_balance$p_num_followers),
  value("table_a2", "days_on_twitter", "p_value", study_1_balance$p_num_days),
  value("table_a2", "centrality", "p_value", study_1_balance$p_centrality),
  value("table_a2", "omnibus", "p_value", study_1_balance$p_omnibus)
)

script_a3 <- bind_rows(
  balance_cells("table_a3", LCVexp2, centrality_na_rm = FALSE),
  value("table_a3", "account_type", "p_value", get("p_account_type", envir = appendix_env)),
  value("table_a3", "num_followers", "p_value", get("p_num_followers", envir = appendix_env)),
  value("table_a3", "days_on_twitter", "p_value", get("p_num_days", envir = appendix_env)),
  value("table_a3", "centrality", "p_value", get("p_centrality", envir = appendix_env)),
  value("table_a3", "omnibus", "p_value", get("p_omnibus", envir = appendix_env))
)

# Tables A4 and A5: heterogeneous effects ----
terms_heterogeneity <- function(moderator) {
  base <- c(treat_fol = "treatfol", treat_org = "treatorg")
  interactions <- switch(
    moderator,
    centrality_centered = c(centrality = "centrality_centered",
                            fol_x_centrality = "treatfol:centrality_centered",
                            org_x_centrality = "treatorg:centrality_centered"),
    num_followers_centered = c(followers = "num_followers_centered",
                               fol_x_followers = "treatfol:num_followers_centered",
                               org_x_followers = "treatorg:num_followers_centered"),
    days_on_twitter_centered = c(days = "days_on_twitter_centered",
                                 fol_x_days = "treatfol:days_on_twitter_centered",
                                 org_x_days = "treatorg:days_on_twitter_centered"),
    account_type = c(account_male = "account_typemale", account_org = "account_typeorg",
                     account_unknown = "account_typeunknown",
                     fol_x_male = "treatfol:account_typemale",
                     org_x_male = "treatorg:account_typemale",
                     fol_x_org = "treatfol:account_typeorg",
                     org_x_org = "treatorg:account_typeorg",
                     fol_x_unknown = "treatfol:account_typeunknown",
                     org_x_unknown = "treatorg:account_typeunknown")
  )
  c(base, interactions, constant = "(Intercept)")
}

# The deposit fits the eight models in the order signed then tweeted, each cycling
# through the four moderators, and names them het_fit_1 to het_fit_8 in that order.
heterogeneity_moderators <- rep(c("centrality_centered", "num_followers_centered",
                                  "days_on_twitter_centered", "account_type"), 2)

heterogeneity <- function(table_figure, fits) {
  map(seq_along(fits), function(i) {
    fit_rows(table_figure, paste0("m", i), fits[[i]],
             terms_heterogeneity(heterogeneity_moderators[i]))
  }) |>
    list_rbind()
}

script_a4 <- heterogeneity("table_a4", study_1_het)
script_a5 <- heterogeneity("table_a5",
                           mget(paste0("het_fit_", 1:8), envir = appendix_env))

# Running text, the Figure 3 caption and Table A1 ----
share <- function(d, type) 100 * mean(d$account_type == type)

difference_p <- function(fit) coef(summary(fit))["as.factor(treat)org", "Pr(>|t|)"]

script_text <- bind_rows(
  value("text", "study1_p_fol_vs_org_signed", "p_value",
        difference_p(fit_from(main_env, "treat.signed.diff.1"))),
  value("text", "study1_p_fol_vs_org_tweeted", "p_value",
        difference_p(fit_from(main_env, "treat.tweeted.diff.1"))),
  value("text", "study2_p_fol_vs_org_signed", "p_value",
        difference_p(fit_from(main_env, "treat.signed.diff.2"))),
  value("text", "study1_any_dm_signed_pp", "estimate",
        -100 * coef(fit_from(main_env, "treat.signed.dm.1"))[2]),
  value("text", "study1_org_minus_fol_tweeted_pp", "estimate",
        -100 * coef(fit_from(main_env, "treat.tweeted.diff.1"))[2]),
  value("text", "study1_first_stage_pp", "estimate",
        100 * coef(fit_from(main_env, "exposure.1"))[2]),
  value("text", "study2_encouragement_fol_pp", "estimate",
        100 * coef(fit_from(main_env, "buttonbytreat.tweeted.2"))[2]),
  value("text", "study2_encouragement_org_pp", "estimate",
        100 * sum(coef(fit_from(main_env, "buttonbytreat.tweeted.2"))[c(2, 4)])),
  # The deposit fits its Study 1 imbalance check with plain lm(), so the p-value it
  # reports is the classical one rather than the HC2 the rest of the paper uses.
  value("text", "study1_button_imbalance_p", "p_value",
        coef(summary(fit_from(main_env, "button.signed.1")))[2, 4]),
  value("text", "study1_signers", "n", sum(LCVexp1$signed)),
  value("text", "study1_second_stage_pool", "n", sum(LCVexp1$probexposure > 0, na.rm = TRUE)),
  value("text", "study2_signers", "n", sum(LCVexp2$signed)),
  value("text", "study2_second_stage_pool", "n", sum(LCVexp2$probexposure > 0, na.rm = TRUE)),
  value("text", "footnote_11_zero_clicks", "percent", 100 * (1 - 1 / 10000)^nrow(LCVexp1)),
  value("figure_3", "study1_share_male", "percent", share(LCVexp1, "male")),
  value("figure_3", "study1_share_female", "percent", share(LCVexp1, "female")),
  value("figure_3", "study1_share_org", "percent", share(LCVexp1, "org")),
  value("figure_3", "study1_share_unknown", "percent", share(LCVexp1, "unknown")),
  value("figure_3", "study2_share_male", "percent", share(LCVexp2, "male")),
  value("figure_3", "study2_share_female", "percent", share(LCVexp2, "female")),
  value("figure_3", "study2_share_org", "percent", share(LCVexp2, "org")),
  value("figure_3", "study2_share_unknown", "percent", share(LCVexp2, "unknown")),
  value("table_a1", "study1_types_5_to_7", "percent",
        100 * mean(LCVexp1$signed[LCVexp1$treat != "pub"])),
  value("table_a1", "study2_types_5_to_7", "percent",
        100 * mean(LCVexp2$signed[LCVexp2$treat != "pub"])),
  value("table_a1", "type_8_lower", "proportion",
        1 - mean(LCVexp2$signed[LCVexp2$treat != "pub"])),
  value("table_a1", "type_8_upper", "proportion",
        1 - mean(LCVexp1$signed[LCVexp1$treat != "pub"])),
  value("text", "study1_network_members", "n", nrow(LCVexp1)),
  value("text", "study2_network_members", "n", nrow(LCVexp2)),
  value("figure_1", "network_members", "n", nrow(LCVexp1)),
  value("text", "study1_tweets_total", "n", sum(LCVexp1$tweeted)),
  value("text", "study1_fol_signed_pp", "estimate",
        100 * coef(fit_from(main_env, "treat.signed.1"))[["as.factor(treat)fol"]]),
  value("text", "study1_org_signed_pp", "estimate",
        100 * coef(fit_from(main_env, "treat.signed.1"))[["as.factor(treat)org"]]),
  value("text", "study1_fol_tweeted_pp", "estimate",
        100 * coef(fit_from(main_env, "treat.tweeted.1"))[["as.factor(treat)fol"]]),
  value("text", "study2_fol_signed_pp", "estimate",
        100 * coef(fit_from(main_env, "treat.signed.2"))[["as.factor(treat)fol"]]),
  value("text", "study2_org_signed_pp", "estimate",
        100 * coef(fit_from(main_env, "treat.signed.2"))[["as.factor(treat)org"]]),
  value("text", "study2_button_tweeted_pp", "estimate",
        100 * coef(fit_from(main_env, "button.tweeted.2"))[["as.factor(tweetbutton)1"]]),
  value("text", "study2_itt_signed_pp", "estimate",
        100 * coef(fit_from(main_env, "exposure.signed.2"))[["exposure"]]),
  value("text", "study2_cace_signed_pp", "estimate",
        100 * coef(fit_from(main_env, "exposure.signed.iv.2"))[["actual_exposure"]])
)

# The article's qualitative claims, evaluated against the deposit ----
# Each claim is a statement about signs, thresholds or orderings rather than a number,
# so the deposit's answer is a truth value. These read the deposit's own fitted models,
# whose p-values are the classical ones lm() reports rather than the HC2 the tables
# print, which is the deposit's own choice and is left alone.
holds <- function(row_label, condition) {
  value("text", row_label, "holds", as.integer(condition))
}
archive_p <- function(name, term) coef(summary(fit_from(main_env, name)))[term, "Pr(>|t|)"]
archive_b <- function(name, term) coef(fit_from(main_env, name))[[term]]

share_encouraged <- LCVexp1 |>
  filter(treat != "pub") |>
  summarize(share = mean(tweetbutton), .by = treat) |>
  pull(share)
enc_fol_1 <- archive_b("buttonbytreat.tweeted.1", "as.factor(tweetbutton)1")
enc_org_1 <- enc_fol_1 +
  archive_b("buttonbytreat.tweeted.1", "as.factor(tweetbutton)1:as.factor(treat)org")
network_1_p <- map_dbl(
  list(c("exposure.signed.1", "exposure"), c("exposure.signed.iv.1", "actual_exposure"),
       c("exposure.tweeted.1", "exposure"), c("exposure.tweeted.iv.1", "actual_exposure")),
  \(s) coef(summary(fit_from(main_env, s[1])))[s[2], 4])
network_2_tweeted_p <- map_dbl(
  list(c("exposure.tweeted.2", "exposure"), c("exposure.tweeted.iv.2", "actual_exposure")),
  \(s) coef(summary(fit_from(main_env, s[1])))[s[2], 4])
dm_1 <- -100 * archive_b("treat.signed.dm.1", "treat == \"pub\"TRUE")
dm_2 <- 100 * mean(c(archive_b("treat.signed.2", "as.factor(treat)fol"),
                     archive_b("treat.signed.2", "as.factor(treat)org")))
cate <- function(d, dv, type, term) {
  fit <- summary(lm(as.formula(paste(dv, "~ treat")), data = subset(d, account_type == type)))
  coef(fit)[term, ]
}
cate_frame <- map(list(list(LCVexp1, "Study 1"), list(LCVexp2, "Study 2")), function(spec) {
  expand_grid(dv = c("signed", "tweeted"),
              type = c("female", "male", "org", "unknown"),
              term = c("treatfol", "treatorg")) |>
    mutate(study = spec[[2]],
           estimate = pmap_dbl(list(dv, type, term),
                               \(a, b, c) cate(spec[[1]], a, b, c)[["Estimate"]]))
}) |>
  list_rbind()
smallest_is_org <- function(study_label, dv_label) {
  cate_frame |>
    filter(study == study_label, dv == dv_label) |>
    summarize(smallest = type[which.min(estimate)] == "org", .by = term) |>
    pull(smallest) |>
    all()
}
gender_gap <- cate_frame |>
  filter(type %in% c("male", "female")) |>
  summarize(gap = estimate[type == "male"] - estimate[type == "female"],
            .by = c(study, dv, term)) |>
  pull(gap)
het_interaction_p <- function(env_fits, index) {
  fit <- summary(env_fits[[index]])
  coef(fit)[str_detect(rownames(coef(fit)), ":account_type"), "Pr(>|t|)"]
}

script_descriptive <- bind_rows(
  holds("d_encouragement_one_third", all(abs(share_encouraged - 1 / 3) < 1e-12)),
  holds("d_study1_pub_zero_signed", sum(subset(LCVexp1, treat == "pub")$signed) == 0),
  holds("d_study1_dm_significant",
        archive_b("treat.signed.1", "as.factor(treat)fol") > 0 &&
          archive_b("treat.signed.1", "as.factor(treat)org") > 0 &&
          archive_p("treat.signed.1", "as.factor(treat)fol") < 0.01 &&
          archive_p("treat.signed.1", "as.factor(treat)org") < 0.01),
  holds("d_study1_encouragement_half",
        archive_b("button.tweeted.1", "as.factor(tweetbutton)1") >= 0.40 &&
          archive_b("button.tweeted.1", "as.factor(tweetbutton)1") < 0.50),
  holds("d_study1_organizer_38pp",
        archive_b("buttonbytreat.tweeted.1",
                  "as.factor(tweetbutton)1:as.factor(treat)org") < -0.38),
  holds("d_study1_follower_twice", enc_fol_1 > 2 * enc_org_1),
  holds("d_study1_network_null", all(network_1_p >= 0.05)),
  holds("d_study2_larger",
        archive_b("treat.signed.2", "as.factor(treat)fol") >
          archive_b("treat.signed.1", "as.factor(treat)fol") &&
          archive_b("treat.signed.2", "as.factor(treat)org") >
          archive_b("treat.signed.1", "as.factor(treat)org")),
  holds("d_study2_follower_more_effective",
        archive_b("treat.tweeted.2", "as.factor(treat)fol") >
          archive_b("treat.tweeted.2", "as.factor(treat)org") &&
          archive_p("treat.tweeted.diff.2", "as.factor(treat)org") >= 0.05),
  holds("d_study2_tweeted_network_null", all(network_2_tweeted_p >= 0.05)),
  holds("d_study2_network_larger_than_main",
        archive_b("exposure.signed.iv.2", "actual_exposure") >
          archive_b("treat.signed.2", "as.factor(treat)fol") &&
          archive_b("exposure.signed.iv.2", "actual_exposure") >
          archive_b("treat.signed.2", "as.factor(treat)org")),
  holds("d_public_tweet_null",
        sum(subset(LCVexp1, treat == "pub")$signed) == 0 &&
          sum(subset(LCVexp1, treat == "pub")$tweeted) == 0 &&
          sum(subset(LCVexp2, treat == "pub")$signed) == 0 &&
          sum(subset(LCVexp2, treat == "pub")$tweeted) == 0),
  holds("d_dm_click_4pp", all(round(c(dm_1, dm_2)) == 4)),
  holds("d_dm_signatures_exceed_tweets",
        dm_1 > -100 * archive_b("treat.tweeted.dm.1", "treat == \"pub\"TRUE") &&
          dm_2 > 100 * mean(c(archive_b("treat.tweeted.2", "as.factor(treat)fol"),
                              archive_b("treat.tweeted.2", "as.factor(treat)org")))),
  # Compared at the whole percentage points the article prints the range in.
  holds("d_button_35_to_45",
        all(round(100 * range(c(archive_b("button.tweeted.1", "as.factor(tweetbutton)1"),
                                archive_b("button.tweeted.2", "as.factor(tweetbutton)1")))) ==
              c(35, 45))),
  value("figure_3", "d_study1_org_smaller_signed", "holds",
        as.integer(smallest_is_org("Study 1", "signed"))),
  value("figure_3", "d_study1_no_het_tweeted", "holds",
        as.integer(all(het_interaction_p(study_1_het, 8) >= 0.05))),
  value("figure_3", "d_study2_org_smaller_signed", "holds",
        as.integer(smallest_is_org("Study 2", "signed") &&
                     all(het_interaction_p(mget(paste0("het_fit_", 1:8),
                                                envir = appendix_env), 8) >= 0.05))),
  value("figure_3", "d_no_gender_pattern", "holds",
        as.integer(length(unique(sign(gender_gap))) > 1)),
  value("table_a1", "d_types_1_to_4_zero", "holds",
        as.integer(sum(subset(LCVexp1, treat == "pub")$signed) == 0 &&
                     sum(subset(LCVexp2, treat == "pub")$signed) == 0))
)

# Quantities the report and the ground truth's notes describe but the article never
# prints, so they carry no published counterpart and join to nothing.
script_diagnostic <- bind_rows(
  value("diagnostic", "study1_second_stage_sample", "n",
        sum(LCVexp1$probexposure > 0 & LCVexp1$probexposure < 1, na.rm = TRUE)),
  value("diagnostic", "study2_second_stage_sample", "n",
        sum(LCVexp2$probexposure > 0 & LCVexp2$probexposure < 1, na.rm = TRUE))
)

archive_values <- bind_rows(script_2, script_3, script_4, script_5, script_6, script_7,
                            script_8, script_9, script_a2, script_a3, script_a4,
                            script_a5, script_text, script_descriptive, script_diagnostic)

stopifnot(!anyDuplicated(archive_values[c("table_figure", "row_label", "quantity")]))

write_csv(archive_values, here::here("ground_truth", "archive_values.csv"))

# Figure 3, as the deposit plots it ----
# The deposit assembles the plotted estimates into a data frame called data and hands
# it to ggplot, so the figure's numbers can be read off rather than refitted.
figure_3_archive <- get("data", envir = main_env) |>
  as_tibble() |>
  transmute(
    study,
    dv = DV,
    account_type = recode_values(as.character(type),
                                 "Organization" ~ "org",
                                 default = str_to_lower(as.character(type))),
    term = if_else(Treatment == "Follower", "treatfol", "treatorg"),
    estimate = CATE,
    std_error = SE
  )

stopifnot(nrow(figure_3_archive) == 32,
          !anyDuplicated(figure_3_archive[c("study", "dv", "account_type", "term")]))

write_csv(figure_3_archive, here::here("ground_truth", "archive_figure_3_values.csv"))

print(run_status |> filter(status == "error") |> select(file, variant, first_line, kind),
      n = 30)
print(str_glue("Archive values: {nrow(archive_values)} rows from the deposit's own ",
               "objects; {sum(run_status$status == 'error')} of ",
               "{nrow(run_status)} recorded runs and expressions failed."))
