test_that("RRS_radarchart handles missing publication years", {
  radar_dat <- data.frame(
    dimension = factor(c("Open Data", "Open Materials")),
    max_points = c(10, 10),
    rel_score = c(0.5, 0.75),
    xstart = c(0, 1),
    xend = c(1, 2),
    xmid = c(0.5, 1.5)
  )

  rrs <- list(
    radar_dat = radar_dat,
    overall_score = 0.625,
    n_papers = 2,
    publication_years = c(NA_real_, NA_real_)
  )

  expect_s3_class(RRS_radarchart(rrs), "ggplot")
})

test_that("RRS_radarchart handles an absent publication year field", {
  radar_dat <- data.frame(
    dimension = factor(c("Open Data", "Open Materials")),
    max_points = c(10, 10),
    rel_score = c(0.5, 0.75),
    xstart = c(0, 1),
    xend = c(1, 2),
    xmid = c(0.5, 1.5)
  )
  rrs <- list(
    radar_dat = radar_dat,
    overall_score = 0.625,
    n_papers = 2
  )

  expect_s3_class(RRS_radarchart(rrs), "ggplot")
})
