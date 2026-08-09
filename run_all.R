# coppock_guess_ternovski_2016/run_all.R
# Runs the whole reproduction in order: fetch and verify the deposited archive, build the
# two analysis frames, then every published table, figure and in-text quantity.
# Every script is self-contained and can also be run on its own.

library(here)
here::i_am("run_all.R")

# Deposited archive ----
# Downloads from Dataverse on a fresh clone. Either way it stops unless every file
# matches its published checksum and byte size and original/ holds nothing else.
# Sourced again as the last step of this file.
source(here::here("download_original.R"))

# Cleaning ----
source(here::here("maintained", "clean_lcv.R"))

# Main text tables ----
source(here::here("maintained", "table_2_study1_outcomes.R"))
source(here::here("maintained", "table_3_study2_outcomes.R"))
source(here::here("maintained", "table_4_study1_dm_effects.R"))
source(here::here("maintained", "table_5_study1_encouragement.R"))
source(here::here("maintained", "table_6_study1_network.R"))
source(here::here("maintained", "table_7_study2_dm_effects.R"))
source(here::here("maintained", "table_8_study2_encouragement.R"))
source(here::here("maintained", "table_9_study2_network.R"))

# Appendix tables ----
# The two balance scripts each permute assignment 1,000 times and refit a pair of
# multinomial logits on every draw, which is what makes them slow: around two
# minutes for the pair, against a few seconds for every other table script.
source(here::here("maintained", "table_a2_study1_balance.R"))
source(here::here("maintained", "table_a3_study2_balance.R"))
source(here::here("maintained", "table_a4_study1_heterogeneity.R"))
source(here::here("maintained", "table_a5_study2_heterogeneity.R"))

# Figures ----
source(here::here("maintained", "figure_3_het_account_type.R"))

# In-text quantities ----
source(here::here("maintained", "text_summary_stats.R"))
source(here::here("maintained", "text_treatment_effects.R"))

# Qualitative claims ----
# Truth values for the article's claims about shape, sign and threshold, evaluated
# against the estimates the scripts above have already written. Runs after them
# because it reads their output rather than refitting anything.
source(here::here("maintained", "text_descriptive_claims.R"))

# Computational reproducibility of the archive ----
# Re-runs the two omnibus balance tests under the sampler R shipped before 3.6.0,
# which is a question about the deposit rather than about the rewrite. It restores
# the modern sampler before it returns. Another 2,000 pairs of multinomial logits,
# so around two minutes.
source(here::here("maintained", "text_sampler_reproducibility.R"))

# The deposit's own numbers ----
# Copies the deposit into a scratch directory, runs both of its analysis scripts as
# shipped and then expression by expression on a repaired copy, and reads the archive's
# values out of its own fitted models. It runs the deposit's Study 2 permutation loop
# as written, so this is another two minutes.
source(here::here("ground_truth", "extract_archive_values.R"))

# Figure timestamps ----
# R's pdf() device stamps a wall-clock /CreationDate and /ModDate into every figure it
# writes, and those two fields are the only reason two runs of this pipeline produce
# differing files. Blanking them lets the determinism check cover every file the
# pipeline writes rather than all but the figures.
source(here::here("maintained", "helpers.R"))
walk(
  list.files(here::here("maintained", "output"), pattern = "\\.pdf$", full.names = TRUE),
  blank_pdf_timestamps
)

# Ground truth ----
# Joins the published values to the deposit's output and to the rewrite's output.
# Runs last because it reads every file the scripts above have just written.
source(here::here("ground_truth", "build_ground_truth.R"))

# In-text claims ----
# Every published number printed beside the sentence it comes from, recomputed from
# maintained/output/ by a path independent of the ground truth's. Runs after the
# ground truth because the ground truth is what gates its coverage.
source(here::here("maintained", "in_text_claims.R"))

# Deposited archive, again ----
# The check at the top of this file is a precondition. Re-sourcing the download script
# at the end is what would catch a script that wrote into original/ during the run,
# which is how a deposit gets silently overwritten by its own analysis code.
source(here::here("download_original.R"))
