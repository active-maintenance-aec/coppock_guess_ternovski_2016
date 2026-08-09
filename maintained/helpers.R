# coppock_guess_ternovski_2016/maintained/helpers.R
# Output: none
# Depends on: nothing
# Description: Shared packages and helper functions sourced by every script in maintained/.

library(here)
library(tidyverse)
library(estimatr)
library(randomizr)
library(nnet)
library(modelsummary)
library(knitr)
library(kableExtra)

# Anchoring here from the helpers is what lets any single analysis script be run on
# its own, rather than only through run_all.R.
here::i_am("maintained/helpers.R")

# modelsummary writes siunitx markup by default, which needs a package the report
# does not load. Plain numerics render everywhere.
options(modelsummary_format_numeric_latex = "plain")

# Standard error of a mean ----
# Used once per covariate per treatment arm in the two balance tables. Reimplements
# se_mean() from the archive's LCVsource.r, which drops missing values from the
# count as well as from the sum. Only days_on_twitter is ever missing, so the two
# behave identically on every other covariate.
se_mean <- function(x) {
  x <- x[!is.na(x)]
  sd(x) / sqrt(length(x))
}

# p-value of the joint F test that a covariate is unrelated to assignment ----
# Reimplements f_tester() from LCVsource.r. The perfect-fit guard is load-bearing:
# it fires on one of the 300 block-by-covariate tests Table A3 runs, where the
# covariate is constant within the block and the F statistic is undefined rather
# than large.
f_test_p <- function(x, z) {
  fit <- lm(x ~ z)
  if (sum(residuals(fit)^2) < 1e-10) return(1)
  f_stat <- summary(fit)$fstatistic
  unname(pf(q = f_stat[1], df1 = f_stat[2], df2 = f_stat[3], lower.tail = FALSE))
}

# Fisher's method for combining independent p-values ----
# Study 2 assigned treatment within blocks, so each covariate gets one test per
# block and the block-level p-values are combined into one.
fisher_combine <- function(p) {
  pchisq(-2 * sum(log(p)), df = 2 * length(p), lower.tail = FALSE)
}

# Tidy estimates plus the fit statistics the published tables print ----
# Every regression table in the paper reports coefficients, robust standard
# errors, N and R squared together, and broom splits those across tidy() and
# glance(). This helper joins them and labels the column the table calls the model.
tidy_fit <- function(fit, model) {
  tidy(fit) |>
    mutate(model = model, nobs = fit$nobs, r_squared = fit$r.squared) |>
    select(model, term, estimate, std.error, statistic, p.value,
           conf.low, conf.high, nobs, r_squared)
}

# Randomization inference p-value for the omnibus balance test ----
# Fits the multinomial logit of assignment on all covariates and on an intercept
# alone, takes the deviance difference as the likelihood ratio statistic, then
# repeats it against permuted assignments drawn under the study's own assignment
# protocol. Returns the observed statistic and the share of permutations that
# exceed it. `permute` is a function of no arguments returning one draw.
omnibus_balance_p <- function(data, covariates, permute, sims = 1000, seed = 343) {
  formula_full <- as.formula(paste("z_ ~", paste(covariates, collapse = " + ")))
  data$z_ <- as.character(data$treat)
  llr_obs <- multinom(z_ ~ 1, data = data, trace = FALSE)$deviance -
    multinom(formula_full, data = data, trace = FALSE)$deviance

  set.seed(seed)
  llr_sim <- map_dbl(seq_len(sims), function(i) {
    data$z_ <- permute()
    multinom(z_ ~ 1, data = data, trace = FALSE)$deviance -
      multinom(formula_full, data = data, trace = FALSE)$deviance
  })

  list(llr_obs = llr_obs, p_value = mean(llr_sim > llr_obs), sims = sims)
}

# Blank a figure PDF's embedded timestamps ----
# R's pdf() device stamps /CreationDate and /ModDate with the wall clock, so an
# otherwise deterministic pipeline writes a different file on every run. The epoch
# string is the same width as what it replaces, which keeps the cross-reference byte
# offsets valid, and a file with no timestamp is left alone.
blank_pdf_timestamps <- function(path) {
  epoch <- charToRaw("D:19700101000000")
  raw_pdf <- readBin(path, "raw", file.size(path))
  hits <- grepRaw("D:[0-9]{14}", raw_pdf, all = TRUE)
  if (length(hits) == 0) return(invisible(path))
  for (h in hits) raw_pdf[h:(h + length(epoch) - 1L)] <- epoch
  writeBin(raw_pdf, path)
  invisible(path)
}
