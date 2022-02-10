test_that("fotmob_get_matches_by_date() works", {
  testthat::skip_on_cran()
  results <- fotmob_get_matches_by_date(date = c("20210925", "20210926"))
  expect_equal(nrow(results), 268)
  expect_equal(ncol(results), 12)
})

test_that("fotmob_get_match_details() works", {
  testthat::skip_on_cran()
  details <- fotmob_get_match_details(c(3609987, 3609979))
  expect_equal(nrow(details), 2)
  expect_equal(ncol(details), 15)
})


test_that("fotmob_get_match_players() works", {
  testthat::skip_on_cran()
  players <- fotmob_get_match_players(c(3609987, 3609979))
  expect_equal(nrow(players), 80)
  expect_equal(ncol(players), 29)
})
