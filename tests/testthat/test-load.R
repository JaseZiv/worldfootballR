context("Testing FBref Load data functions")


test_that("load_match_results() works", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  test_df <- load_match_results(country = "ENG", gender = "M", season_end_year = 2022, tier="1st")
  # test the functions returns the data
  expect_type(test_df, "list")

  # test that multiple countries can be passed to the function
  test_df <- load_match_results(country = c("ENG", "AUS"), gender = "F", season_end_year = 2021, tier="1st")
  expect_type(test_df, "list")
  expect_false(nrow(test_df) == 0)

  bad_df <- load_match_results(country = "foo", gender = "M", season_end_year = 2022, tier="1st")
  expect_true(nrow(bad_df) == 0)

})


test_that("load_fb_big5_advanced_season_stats() works", {
  testthat::skip_on_cran()
  big5_team_shooting_multiple <- load_fb_big5_advanced_season_stats(season_end_year= c(2019:2021), stat_type= "shooting", team_or_player= "team")
  expect_type(big5_team_shooting_multiple, "list")
  expect_false(nrow(big5_team_shooting_multiple) == 0)

  big5_team_shooting <- load_fb_big5_advanced_season_stats(season_end_year= c(2022), stat_type= "shooting", team_or_player= "team")
  expect_type(big5_team_shooting, "list")
  expect_false(nrow(big5_team_shooting) == 0)

  big5_player_shooting <- load_fb_big5_advanced_season_stats(season_end_year= c(2022), stat_type= "shooting", team_or_player= "player")
  expect_type(big5_player_shooting, "list")
  expect_false(nrow(big5_player_shooting) == 0)

  bad_df <- load_fb_big5_advanced_season_stats(season_end_year= c(2022), stat_type= "foo", team_or_player= "player")
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
  expect_true(ncol(test_df) == 21)

  # test for error also:
  expect_error(load_understat_league_shots(league = "foo"))

})

test_that("load_fotmob_matches_by_date() works", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()

  n_cols_match_details <- 41
  epl_matches <- load_fotmob_matches_by_date(league_id = 47)
  expect_type(epl_matches, "list")
  n_rows_epl_matches <- nrow(epl_matches)
  expect_gt(n_rows_epl_matches, 0)
  expect_equal(ncol(epl_matches), n_cols_match_details)

  epl_ll_matches <- load_fotmob_matches_by_date(league_id = c(47, 87))
  expect_gt(nrow(epl_ll_matches), n_rows_epl_matches)
  expect_equal(ncol(epl_matches), n_cols_match_details)

  expect_error(load_fotmob_matches_by_date(league_id = 0))
})

test_that("load_fotmob_match_details() works", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()

  n_cols_match_details <- 42
  epl_match_details <- load_fotmob_match_details(league_id = 47)
  expect_type(epl_match_details, "list")
  n_rows_epl_match_details <- nrow(epl_match_details)
  expect_gt(n_rows_epl_match_details, 0)
  expect_equal(ncol(epl_match_details), n_cols_match_details)

  epl_ll_match_details <- load_fotmob_match_details(league_id = c(47, 87))
  expect_gt(nrow(epl_ll_match_details), n_rows_epl_match_details)
  expect_equal(ncol(epl_ll_match_details), n_cols_match_details)

  expect_error(load_fotmob_match_details(league_id = 0))
})


