context("Testing FBref Load data functions")


test_that("load_match_results() works", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  test_df <- load_match_results(country = "ENG", gender = "M", season_end_year = 2022, tier = "1st")
  # test the functions returns the data
  expect_type(test_df, "list")

  # test that multiple countries can be passed to the function
  test_df <- load_match_results(country = c("ENG", "AUS"), gender = "F", season_end_year = 2021, tier = "1st")
  expect_type(test_df, "list")
  expect_false(nrow(test_df) == 0)

  bad_df <- load_match_results(country = "foo", gender = "M", season_end_year = 2022, tier = "1st")
  expect_true(nrow(bad_df) == 0)
})


test_that("load_fb_big5_advanced_season_stats() works", {
  testthat::skip_on_cran()
  big5_team_shooting_multiple <- load_fb_big5_advanced_season_stats(season_end_year = c(2019:2021), stat_type = "shooting", team_or_player = "team")
  expect_type(big5_team_shooting_multiple, "list")
  expect_false(nrow(big5_team_shooting_multiple) == 0)

  big5_team_shooting <- load_fb_big5_advanced_season_stats(season_end_year = c(2022), stat_type = "shooting", team_or_player = "team")
  expect_type(big5_team_shooting, "list")
  expect_false(nrow(big5_team_shooting) == 0)

  big5_player_shooting <- load_fb_big5_advanced_season_stats(season_end_year = c(2022), stat_type = "shooting", team_or_player = "player")
  expect_type(big5_player_shooting, "list")
  expect_false(nrow(big5_player_shooting) == 0)

  bad_df <- load_fb_big5_advanced_season_stats(season_end_year = c(2022), stat_type = "foo", team_or_player = "player")
  expect_true(nrow(bad_df) == 0)
})

test_that("load_fb_match_shooting() works", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  test_df <- load_fb_match_shooting(country = "ENG", gender = "M", tier = "1st")
  # test the functions returns the data
  expect_type(test_df, "list")

  # test that multiple countries can be passed to the function
  test_df <- load_fb_match_shooting(country = c("ESP", "USA"), gender = "M", season_end_year = 2021, tier = "1st")
  expect_type(test_df, "list")
  expect_false(nrow(test_df) == 0)

  bad_df <- load_fb_match_shooting(country = "foo", gender = "M", season_end_year = 2022, tier = "1st")
  expect_true(nrow(bad_df) == 0)
})


test_that("load_match_comp_results() works", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  test_df <- load_match_comp_results(comp_name = "Coppa Italia")
  # test the functions returns the data
  expect_type(test_df, "list")

  # test that multiple comps can be passed to the function
  cups <- c("FIFA Women's World Cup", "FIFA World Cup")
  test_df <- load_match_comp_results(comp_name = cups)
  expect_type(test_df, "list")
  expect_false(nrow(test_df) == 0)

  bad_df <- load_match_comp_results(comp_name = "foo")
  expect_true(nrow(bad_df) == 0)
})


test_that("load_understat_league_shots() works", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  test_df <- load_understat_league_shots(league = "Serie A")
  # test the functions returns the data
  expect_type(test_df, "list")
  expect_equal(ncol(test_df), 21)

  # test for error also:
  expect_error(load_understat_league_shots(league = "foo"))
})


test_that("check_league_name() works", {
  expect_error(check_league_name(league = "foo"))
  expect_silent(check_league_name(league = "Serie A"))
})


test_that("Change if statement by list", {
  expect_equal(LEAGUES[["La liga"]], "La_liga")
  expect_equal(LEAGUES[["Ligue 1"]], "Ligue_1")
  expect_equal(LEAGUES[["Serie A"]], "Serie_A")
})


test_that("load_fotmob_matches_by_date() works", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()

  expected_matches_by_date_cols <- c("away_id", "away_name", "away_score", "ccode", "home_id", "home_name", "home_score", "id", "match_id", "match_league_id")
  epl_matches <- load_fotmob_matches_by_date(league_id = 47)
  expect_type(epl_matches, "list")
  n_rows_epl_matches <- nrow(epl_matches)
  expect_gt(n_rows_epl_matches, 0)
  ## There have been issues where the columns are not in the same exact order depending on the day, so rely on sort
  expect_true(all(expected_matches_by_date_cols %in% colnames(epl_matches)))

  epl_ll_matches <- load_fotmob_matches_by_date(league_id = c(47, 87))
  expect_gt(nrow(epl_ll_matches), n_rows_epl_matches)
  expect_true(all(expected_matches_by_date_cols %in% colnames(epl_ll_matches)))

  expect_error(load_fotmob_matches_by_date(league_id = 0))
})

test_that("load_fotmob_match_details() works", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()

  ## "short_name" appears for a very small set of rows
  expected_match_detail_cols <- c("match_id", "match_round", "league_id", "league_name", "league_round_name", "parent_league_id", "parent_league_season", "match_time_utc", "home_team_id", "home_team", "home_team_color", "away_team_id", "away_team", "away_team_color", "id", "event_type", "team_id", "player_id", "player_name", "x", "y", "min", "min_added", "is_blocked", "is_on_target", "blocked_x", "blocked_y", "goal_crossed_y", "goal_crossed_z", "expected_goals", "expected_goals_on_target", "shot_type", "situation", "period", "is_own_goal", "on_goal_shot_x", "on_goal_shot_y", "on_goal_shot_zoom_ratio", "first_name", "last_name", "team_color", "short_name")
  epl_match_details <- load_fotmob_match_details(league_id = 47)
  expect_type(epl_match_details, "list")
  n_rows_epl_match_details <- nrow(epl_match_details)
  expect_gt(n_rows_epl_match_details, 0)
  expect_equal(sort(colnames(epl_match_details)), sort(expected_match_detail_cols))

  epl_ll_match_details <- load_fotmob_match_details(league_id = c(47, 87))
  expect_gt(nrow(epl_ll_match_details), n_rows_epl_match_details)
  expect_equal(sort(colnames(epl_ll_match_details)), sort(expected_match_detail_cols))

  expect_error(load_fotmob_match_details(league_id = 0))
})
