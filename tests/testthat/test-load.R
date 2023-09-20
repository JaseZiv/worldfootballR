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
  test_df <- load_fb_match_shooting(
    country = "ENG",
    gender = "M",
    tier = "1st"
  )
  # test the functions returns the data
  expect_type(test_df, "list")
  expect_gt(nrow(test_df), 0)

  # test that multiple countries can be passed to the function
  test_df <- load_fb_match_shooting(
    country = c("ESP", "USA"),
    gender = "M",
    season_end_year = 2021,
    tier = "1st"
  )
  expect_type(test_df, "list")
  expect_gt(nrow(test_df), 0)
  expect_equal(length(unique(test_df$Country)), 2)

  expect_message(
    bad_df <- load_fb_match_shooting(
      country = "foo",
      gender = "M",
      season_end_year = 2022,
      tier = "1st"
    ),
    regexp = "Data not loaded[.] Please check parameters[.]"
  )
  expect_true(nrow(bad_df) == 0)

  ## won't error here. instead, will silently not load anything for "foo"
  test_df <- load_fb_match_shooting(
    country = c("ITA", "foo"),
    gender = "M",
    season_end_year = 2021,
    tier = "1st"
  )
  expect_type(test_df, "list")
  expect_gt(nrow(test_df), 0)
  expect_equal(length(unique(test_df$Country)), 1)
})

test_that("load_fb_match_summary() works", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  test_df <- load_fb_match_summary(
    country = "ENG",
    gender = "M",
    tier = "1st"
  )
  # test the functions returns the data
  expect_type(test_df, "list")
  expect_gt(nrow(test_df), 0)

  # test that multiple countries can be passed to the function
  test_df <- load_fb_match_summary(
    country = c("ESP", "USA"),
    gender = "M",
    season_end_year = 2021,
    tier = "1st"
  )
  expect_type(test_df, "list")
  expect_gt(nrow(test_df), 0)
  expect_equal(length(unique(test_df$Country)), 2)

  expect_message(
    bad_df <- load_fb_match_summary(
      country = "foo",
      gender = "M",
      season_end_year = 2022,
      tier = "1st"
    ),
    regexp = "Data not loaded[.] Please check parameters[.]"
  )
  expect_true(nrow(bad_df) == 0)

  ## won't error here. instead, will silently not load anything for "foo"
  test_df <- load_fb_match_summary(
    country = c("ITA", "foo"),
    gender = "M",
    season_end_year = 2021,
    tier = "1st"
  )
  expect_type(test_df, "list")
  expect_gt(nrow(test_df), 0)
  expect_equal(length(unique(test_df$Country)), 1)
})

test_that("load_fb_advanced_match_stats() works", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  test_df <- load_fb_advanced_match_stats(
    country = "ENG",
    gender = "M",
    tier = "1st",
    stat_type = "keeper",
    team_or_player = "player"
  )
  # test the functions returns the data
  expect_type(test_df, "list")
  expect_gt(nrow(test_df), 0)

  # test that multiple countries can be passed to the function
  test_df <- load_fb_advanced_match_stats(
    country = c("ESP", "USA"),
    gender = "M",
    season_end_year = 2021,
    tier = "1st",
    stat_type = "passing",
    team_or_player = "player"
  )
  expect_type(test_df, "list")
  expect_gt(nrow(test_df), 0)
  expect_equal(length(unique(test_df$Country)), 2)

  expect_message(
    bad_df <- load_fb_advanced_match_stats(
      country = "foo",
      gender = "M",
      season_end_year = 2022,
      tier = "1st",
      stat_type = "summary",
      team_or_player = "player"
    ),
    regexp = "Data not loaded[.] Please check parameters[.]"
  )
  expect_true(nrow(bad_df) == 0)

  ## won't error here. instead, will silently not load anything for "foo"
  test_df <- load_fb_advanced_match_stats(
    country = c("ITA", "foo"),
    gender = "M",
    season_end_year = 2021,
    tier = "1st",
    stat_type = "misc",
    team_or_player = "player"
  )
  expect_type(test_df, "list")
  expect_gt(nrow(test_df), 0)
  expect_equal(length(unique(test_df$Country)), 1)
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
