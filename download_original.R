# coppock_guess_ternovski_2016/download_original.R
# Output: original/ (the deposited replication archive, not redistributed in this repo)
# Depends on: original_manifest.csv
# Description: Fetch the deposited archive from Harvard Dataverse and verify every file.
#   Run this once before running anything in maintained/. Re-running is free: files
#   already present with the right checksum are not downloaded again.
#
#   The manifest carries two checksums per file. md5_served is the MD5 of the bytes
#   Dataverse returns for `?format=original`, which is what this code was written
#   against. md5_published is the checksum Dataverse displays. Here all six agree, but
#   they do not always: another deposit in this program carries three published
#   checksums that verify neither the original nor the derived tabular file, so
#   verification runs against md5_served and any disagreement is reported.
#
#   None of the six files was ingested into a tabular representation, so served_as
#   repeats the deposited name in every row and `?format=original` and the plain
#   download return the same bytes.
#
#   Sourcing this file leaves verify_original() behind, and run_all.R calls it again
#   once every script has run. The check here is only a precondition: a script that
#   damaged original/ mid-run would otherwise go unnoticed until the next run.

library(tidyverse)
library(here)

here::i_am("download_original.R")

dataset_doi <- "doi:10.7910/DVN/29548"
base_url <- "https://dataverse.harvard.edu/api/access/datafile"

# Manifest ----
manifest <- read_csv(here::here("original_manifest.csv"), show_col_types = FALSE)

dir.create(here::here("original"), showWarnings = FALSE)

# Verification ----
# Both published checksums and the byte size on every file the manifest lists, plus
# the assertion that original/ holds nothing else. The emptiness check is what a
# name-only check misses: a deposited file renamed by hand still matches its own
# checksum under the new name, works on this machine, and breaks every clone.
verify_original <- function() {
  verified <- manifest |>
    mutate(
      path = here::here("original", file),
      md5_local = unname(tools::md5sum(path)),
      bytes_local = file.size(path),
      md5_ok = !is.na(md5_local) & md5_local == md5_served,
      bytes_ok = !is.na(bytes_local) & bytes_local == bytes,
      published_agrees = md5_served == md5_published
    ) |>
    select(file, bytes, bytes_local, md5_served, md5_local, md5_ok, bytes_ok,
           published_agrees)

  # all.files = TRUE is not optional. Without it a stray dotfile passes silently, and
  # dotfiles can be genuine deposited members in other archives.
  extra <- setdiff(
    list.files(here::here("original"), recursive = TRUE, all.files = TRUE, no.. = TRUE),
    manifest$file)

  print(verified, n = nrow(verified))

  if (!all(verified$md5_ok)) {
    stop("Checksum mismatch in original/: ",
         paste(verified$file[!verified$md5_ok], collapse = ", "))
  }
  if (!all(verified$bytes_ok)) {
    stop("Byte size mismatch in original/: ",
         paste(verified$file[!verified$bytes_ok], collapse = ", "))
  }
  if (length(extra) > 0) {
    stop("original/ holds files the manifest does not list: ",
         paste(extra, collapse = ", "))
  }

  print(str_glue(
    "All {nrow(verified)} files match on checksum and byte size, and original/ holds ",
    "nothing else. {sum(!verified$published_agrees)} carry a published checksum that ",
    "disagrees with what Dataverse serves."))
  invisible(verified)
}

# Download what is absent ----
# format=original asks for the deposited bytes rather than the tabular
# representation Dataverse derives for ingested files.
#
# A file that is present with the wrong checksum is not silently re-fetched. The
# deposit does not change, so a local copy that disagrees with it was damaged here,
# and quietly replacing it would hide the failure this gate exists to catch: an
# analysis script writing into original/ part way through a run, which run_all.R
# looks for by sourcing this file again at the end. Delete the named file and re-run.
planned <- manifest |>
  mutate(
    path = here::here("original", file),
    url = str_glue("{base_url}/{dataverse_file_id}?format=original"),
    md5_local = unname(tools::md5sum(path)),
    present = !is.na(md5_local),
    damaged = present & md5_local != md5_served
  )

if (any(planned$damaged)) {
  stop("original/ holds a copy that does not match the deposit: ",
       paste(planned$file[planned$damaged], collapse = ", "),
       ". Delete it and run this script again.")
}

walk2(
  planned$url[!planned$present],
  planned$path[!planned$present],
  function(url, path) download.file(url, destfile = path, mode = "wb", quiet = TRUE)
)

print(str_glue("Downloaded {sum(!planned$present)} of {nrow(planned)} files; ",
               "{sum(planned$present)} already present and verified."))

verify_original()
print(str_glue("Archive: {dataset_doi}"))
