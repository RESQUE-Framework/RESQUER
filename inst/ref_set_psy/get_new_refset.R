library(OAmetrics)

# ---------------------------------------------------------------------------
# Settings
# ---------------------------------------------------------------------------
years      <- 2001:2025
n_per_year <- 100000
concept_id <- "C15744967"   # OpenAlex concept "Psychology"
work_type  <- "article"
seed       <- 42

# Delete the intermediate folder after the final RDS has been written?
cleanup_on_success <- TRUE

# ---------------------------------------------------------------------------
# Crash-safe intermediate folder
# ---------------------------------------------------------------------------
# tempdir() itself is a per-session folder (".../RtmpXXXXXX") with a random
# name. R deletes it on a normal exit, and after a crash a new session cannot
# find it again. We therefore use a folder with a *fixed* name in the parent
# of tempdir(), i.e. the system/user temp directory ($TMPDIR on macOS). R never
# deletes this folder, and a re-run after a crash finds it again.
# The folder name encodes the sampling parameters, so results from runs with
# other settings are never mixed in.
# Note: the OS may purge the system temp dir (macOS deletes files there that
# were not accessed for ~3 days, and on some systems it is cleared at reboot).
# To use a fully persistent location instead, set e.g.
#   options(RESQUER.refset_dir = tools::R_user_dir("RESQUER", "cache"))
# before sourcing this script.
tmp_dir <- getOption(
  "RESQUER.refset_dir",
  default = file.path(
    dirname(normalizePath(tempdir())),
    sprintf("RESQUER_refset_%s_%s_n%d_seed%d", concept_id, work_type, n_per_year, seed)
  )
)
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

year_file <- function(y) file.path(tmp_dir, paste0("refset_", y, ".RDS"))

existing <- years[file.exists(vapply(years, year_file, character(1)))]
message(
  "\n------------------------------------------------------------\n",
  "Intermediate results (one refset_<year>.RDS per completed year)\n",
  "are stored in:\n  ", tmp_dir, "\n",
  "Inspect them with:\n",
  "  list.files(\"", tmp_dir, "\")\n",
  "  readRDS(\"", year_file(years[1]), "\")\n",
  "or open the folder in Finder: system2(\"open\", shQuote(\"", tmp_dir, "\"))\n",
  if (length(existing) > 0) {
    paste0("Already downloaded (will be skipped): ", paste(existing, collapse = ", "), "\n")
  } else {
    "No intermediate results found yet - starting from scratch.\n"
  },
  "------------------------------------------------------------"
)

# A year counts as done only if its file exists *and* can be read
# (a crash during saveRDS() could leave a truncated file).
year_done <- function(y) {
  f <- year_file(y)
  file.exists(f) && !inherits(try(readRDS(f), silent = TRUE), "try-error")
}

# ---------------------------------------------------------------------------
# Download, one year at a time (resumes after a crash)
# ---------------------------------------------------------------------------
# get_reference_set() uses a slice seed of seed + year*100 + month, so calling
# it per year yields exactly the same sample as a single call over all years.
start <- Sys.time()
for (y in years) {
  if (year_done(y)) {
    message("Year ", y, ": already downloaded - skipping.")
    next
  }
  get_reference_set(
    years             = y,
    n_per_year        = n_per_year,
    concept.id        = concept_id,
    type              = work_type,
    seed              = seed,
    verbose           = TRUE,
    save_intermediate = tmp_dir   # writes refset_<y>.RDS when the year is complete
  )
}

# ---------------------------------------------------------------------------
# Recombine the yearly sets (only the requested years)
# ---------------------------------------------------------------------------
missing_years <- years[!vapply(years, year_done, logical(1))]
if (length(missing_years) > 0) {
  stop("Missing intermediate results for year(s): ", paste(missing_years, collapse = ", "))
}
refset <- data.table::rbindlist(lapply(years, function(y) readRDS(year_file(y))))

saveRDS(refset, file = "inst/ref_set_psy/c_counts_psy_2001_2025.RDS")

if (cleanup_on_success) unlink(tmp_dir, recursive = TRUE)

end <- Sys.time()
print(end - start)
