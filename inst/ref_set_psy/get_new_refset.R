library(OAmetrics)

start <- Sys.time()
refset <- get_reference_set(
    years = 2022:2025,
    n_per_year    = 100000,
    concept.id    = "C15744967",
    type          = "article",
    seed          = 42,
    verbose       = TRUE,
    save_intermediate = "~/refset_temp"
)

# recombine the single intermediate sets
refset <- get_reference_set_from_files("~/refset_temp")

saveRDS(refset, file = "inst/ref_set_psy/c_counts_psy_2001_2025.RDS")


end <- Sys.time()
print(end-start)
