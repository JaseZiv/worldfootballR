context("Testing Understat functions")


test_that("understat_league_match_results() works", {
  epl_results <- understat_league_match_results(league = "EPL", season_start_year = 2020)
  expect_type(epl_results, "list")
})


test_that("understat_team_season_shots() works", {
  team_shots <- understat_team_season_shots(team_url = "https://understat.com/team/Manchester_City/2020")
  expect_type(team_shots, "list")
  expect_equal(ncol(team_shots), 20)
})


test_that("understat_match_shots() works", {
  match_shots <- understat_match_shots(match_url = "https://understat.com/match/14789")
  expect_type(match_shots, "list")
  expect_equal(ncol(match_shots), 20)
})


test_that("understat_player_shots() works", {
  sterling <- understat_player_shots(player_url = "https://understat.com/player/618")
  expect_type(sterling, "list")
  expect_equal(ncol(sterling), 20)
})

test_that("understat_team_players_stats() works", {
  team_players_stats <- understat_team_players_stats(team_url = c("https://understat.com/team/Liverpool/2020", "https://understat.com/team/Manchester_City/2020"))
  expect_true(any("data.frame" == class(team_players_stats)))
  expect_equal(ncol(team_players_stats), 19)
  expect_gt(nrow(team_players_stats), 0)
})

