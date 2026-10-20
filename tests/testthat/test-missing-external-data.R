test_that("preprocess creates empty tables when external data are unavailable", {
  applicant <- read_RESQUE(
    system.file(
      "extdata/demo_profiles/resque_Schoenbrodt.json",
      package = "RESQUER"
    )
  )

  result <- suppressWarnings(
    preprocess(applicant, get_BIP = FALSE, get_OpenAlex = FALSE)
  )

  expect_s3_class(result$BIP, "data.frame")
  expect_s3_class(result$OAlex_papers, "data.frame")
  expect_s3_class(result$OAlex_papers_unique, "data.frame")
  expect_s3_class(result$FNCS, "data.frame")
  expect_equal(nrow(result$OAlex_papers_unique), 0L)
  expect_named(result$BIP, c("doi", "three_year_cc", "cc", "pop_class"))
  expect_named(result$FNCS, c("doi", "FNCS", "FNPR"))
})

test_that("preprocess handles an empty BIP response", {
  applicant <- read_RESQUE(
    system.file(
      "extdata/demo_profiles/resque_Schoenbrodt.json",
      package = "RESQUER"
    )
  )

  result <- suppressWarnings(
    with_mocked_bindings(
      preprocess(applicant, get_BIP = TRUE, get_OpenAlex = FALSE),
      get_BIP = function(...) NULL,
      .package = "RESQUER"
    )
  )

  expect_equal(nrow(result$BIP), 0L)
  expect_named(result$BIP, c("doi", "three_year_cc", "cc", "pop_class"))
})

test_that("preprocess continues when OpenAlex returns no works", {
  applicant <- read_RESQUE(
    system.file(
      "extdata/demo_profiles/resque_Schoenbrodt.json",
      package = "RESQUER"
    )
  )

  result <- suppressWarnings(
    with_mocked_bindings(
      preprocess(applicant, get_BIP = FALSE, get_OpenAlex = TRUE),
      oa_fetch = function(...) NULL,
      .package = "RESQUER"
    )
  )

  expect_equal(nrow(result$OAlex_papers_unique), 0L)
  expect_named(result$FNCS, c("doi", "FNCS", "FNPR"))
  expect_equal(result$internationalization$international_evenness, NA)
})

test_that("get_RRS_tablerow returns N/A values without an RRS result", {
  result <- get_RRS_tablerow(
    list(
      meta = list(LastName = "Doe"),
      RRS = NA
    ),
    add_scores = TRUE
  )

  expect_equal(
    result$`Relative Rigor Score`,
    "<span style='color:gray;'>N/A</span>"
  )
  expect_equal(
    unname(unlist(result[c(
      "RRS_overall",
      "RRS_OD",
      "RRS_OM",
      "RRS_OC",
      "RRS_PreReg"
    )])),
    rep(NA_real_, 5)
  )
})
