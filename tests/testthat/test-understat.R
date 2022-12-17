context("Testing Understat functions")


test_that("understat_league_match_results() works", {
  testthat::skip_on_cran()
  epl_results <- understat_league_match_results(league = "EPL", season_start_year = 2020)
  expect_type(epl_results, "list")
  expect_error(understat_league_match_results(league = "foo", season_start_year = 2022))
})


test_that("understat_team_season_shots() works", {
  testthat::skip_on_cran()
  team_shots <- understat_team_season_shots(team_url = "https://understat.com/team/Manchester_City/2020")
  expect_type(team_shots, "list")
  expect_equal(ncol(team_shots), 20)
})


test_that("understat_match_shots() works", {
  testthat::skip_on_cran()
  match_shots <- understat_match_shots(match_url = "https://understat.com/match/14789")
  expect_type(match_shots, "list")
  expect_equal(ncol(match_shots), 20)
})


test_that("understat_player_shots() works", {
  testthat::skip_on_cran()
  sterling <- understat_player_shots(player_url = "https://understat.com/player/618")
  expect_type(sterling, "list")
  expect_equal(ncol(sterling), 20)
})

test_that("understat_team_players_stats() works", {
  testthat::skip_on_cran()
  team_players_stats <- understat_team_players_stats(team_url = c("https://understat.com/team/Liverpool/2020", "https://understat.com/team/Manchester_City/2020"))
  expect_true(any("data.frame" == class(team_players_stats)))
  expect_equal(ncol(team_players_stats), 19)
  expect_gt(nrow(team_players_stats), 0)
})

test_that("understat_team_stats_breakdown() works", {
  testthat::skip_on_cran()
  team_stats <- understat_team_stats_breakdown(team_urls = c("https://understat.com/team/Liverpool/2020", "https://understat.com/team/Manchester_City/2020"))
  expect_true(any("data.frame" == class(team_stats)))
  expect_equal(ncol(team_stats), 11)
  expect_gt(nrow(team_stats), 0)
})

test_that("understat_league_season_shots() works", {
  testthat::skip_on_cran()
})
