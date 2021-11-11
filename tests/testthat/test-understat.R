context("Testing Understat functions")


test_that("understat_league_match_results() works", {
  epl_results <- understat_league_match_results(league = "EPL", season_start_year = 2020)
  expect_type(epl_results, "list")
})
