context("Extraction functions extracting data as expected")


test_that("get_advanced_match_stats() works", {
  testthat::skip_if_offline()
  # test the functions returns the data for each stat_type
  expect_type(get_advanced_match_stats(match_url = "https://fbref.com/en/matches/c0996cac/Bordeaux-Nantes-August-21-2020-Ligue-1",
                                       stat_type = "summary", team_or_player = "team"), "list")
  expect_type(get_advanced_match_stats(match_url = "https://fbref.com/en/matches/c0996cac/Bordeaux-Nantes-August-21-2020-Ligue-1",
                                       stat_type = "passing", team_or_player = "team"), "list")
  expect_type(get_advanced_match_stats(match_url = "https://fbref.com/en/matches/c0996cac/Bordeaux-Nantes-August-21-2020-Ligue-1",
                                       stat_type = "passing_types", team_or_player = "team"), "list")
  expect_type(get_advanced_match_stats(match_url = "https://fbref.com/en/matches/c0996cac/Bordeaux-Nantes-August-21-2020-Ligue-1",
                                       stat_type = "defense", team_or_player = "team"), "list")
  expect_type(get_advanced_match_stats(match_url = "https://fbref.com/en/matches/c0996cac/Bordeaux-Nantes-August-21-2020-Ligue-1",
                                       stat_type = "possession", team_or_player = "team"), "list")
  expect_type(get_advanced_match_stats(match_url = "https://fbref.com/en/matches/c0996cac/Bordeaux-Nantes-August-21-2020-Ligue-1",
                                       stat_type = "misc", team_or_player = "team"), "list")
  expect_type(get_advanced_match_stats(match_url = "https://fbref.com/en/matches/c0996cac/Bordeaux-Nantes-August-21-2020-Ligue-1",
                                       stat_type = "keeper", team_or_player = "team"), "list")
  expect_type(get_advanced_match_stats(match_url = "https://fbref.com/en/matches/c0996cac/Bordeaux-Nantes-August-21-2020-Ligue-1",
                                       stat_type = "keeper", team_or_player = "player"), "list")

  # test that multiple match_url can be passed to the function
  test_urls <- c("https://fbref.com/en/matches/c0996cac/Bordeaux-Nantes-August-21-2020-Ligue-1",
                 "https://fbref.com/en/matches/9cbccb37/Dijon-Angers-August-22-2020-Ligue-1")
  test_df <- get_advanced_match_stats(match_url = test_urls, stat_type = "possession", team_or_player = "team")
  expect_type(test_df, "list")
  expect_equal(nrow(test_df), 4)

  # test that incorrect URL will error
  expect_error(get_advanced_match_stats(match_url = "aaa.aaa"))

  # test that an invalid stat_type will error
  expect_error(get_advanced_match_stats(match_url = "https://fbref.com/en/matches/c0996cac/Bordeaux-Nantes-August-21-2020-Ligue-1",
                                        stat_type = "test", team_or_player = "team"))
  expect_error(get_advanced_match_stats(match_url = "https://fbref.com/en/matches/c0996cac/Bordeaux-Nantes-August-21-2020-Ligue-1",
                                        stat_type = "test", team_or_player = "player"))

})



test_that("get_match_lineups() works", {
  testthat::skip_if_offline()
  test_df <- get_match_lineups(match_url = "https://fbref.com/en/matches/c0996cac/Bordeaux-Nantes-August-21-2020-Ligue-1")
  # test the functions returns the data
  expect_type(test_df, "list")
  # test that the correct number of columns returned
  expect_equal(ncol(test_df), 6)
  # test that incorrect URL will return empty data frame
  error_lineups <- get_match_lineups("aaa.aaa")
  expect_equal(nrow(error_lineups), 0)


})


test_that("get_match_report() works", {
  testthat::skip_if_offline()
  test_df <- get_match_report(match_url = "https://fbref.com/en/matches/c0996cac/Bordeaux-Nantes-August-21-2020-Ligue-1")
  # test the functions returns the data
  expect_type(test_df, "list")

  # test that multiple match_url can be passed to the function
  test_urls <- c("https://fbref.com/en/matches/c0996cac/Bordeaux-Nantes-August-21-2020-Ligue-1",
                 "https://fbref.com/en/matches/9cbccb37/Dijon-Angers-August-22-2020-Ligue-1")
  test_df <- get_match_report(match_url = test_urls)
  expect_type(test_df, "list")

  # test that the correct number of columns returned
  expect_equal(ncol(test_df), 17)

  # test that incorrect URL will error
  expect_error(get_match_report("aaa.aaa"))


})


test_that("get_match_results() works", {
  testthat::skip_if_offline()
  test_df <- get_match_results(country = "ENG", gender = "F", season_end_year = 2021)
  # test the functions returns the data
  expect_type(test_df, "list")

  # test that multiple countries can be passed to the function
  test_df <- get_match_results(country = c("ENG", "AUS"), gender = "F", season_end_year = 2021)
  expect_type(test_df, "list")

  # test that incorrect Country will error
  expect_error(get_match_results(country = "BBB", gender = "F", season_end_year = 2021))


})


test_that("get_match_summary() works", {
  testthat::skip_if_offline()
  test_df <- get_match_summary(match_url = "https://fbref.com/en/matches/c0996cac/Bordeaux-Nantes-August-21-2020-Ligue-1")
  # test the functions returns the data
  expect_type(test_df, "list")

  # test that multiple match_url can be passed to the function
  test_urls <- c("https://fbref.com/en/matches/c0996cac/Bordeaux-Nantes-August-21-2020-Ligue-1",
                 "https://fbref.com/en/matches/9cbccb37/Dijon-Angers-August-22-2020-Ligue-1")
  test_df <- get_match_summary(match_url = test_urls)
  expect_type(test_df, "list")

  # test that incorrect url will error
  expect_error(get_match_summary(match_url = "aaa.aaa"))


})



test_that("get_match_url() works", {
  testthat::skip_if_offline()
  test_urls <- get_match_urls(country = "AUS", gender = "F", season_end_year = 2021)
  # test the functions returns the data
  expect_type(test_urls, "character")
  # also test if it's a correct url:
  expect_equal(grepl("https://", test_urls[1]), TRUE)

  # test that multiple match_url can be passed to the function
  test_urls <- c("https://fbref.com/en/matches/c0996cac/Bordeaux-Nantes-August-21-2020-Ligue-1",
                 "https://fbref.com/en/matches/9cbccb37/Dijon-Angers-August-22-2020-Ligue-1")
  test_df <- get_match_summary(match_url = test_urls)
  expect_type(get_match_urls(country = "AUS", gender = c("M", "F"), season_end_year = 2021), "character")

  # test that incorrect url will error
  expect_equal(length(get_match_urls(country = "BBB", gender = "M", season_end_year = 2021)), 0)


})




test_that("get_season_team_stats() works", {
  testthat::skip_if_offline()
  # test the functions returns the data
  expect_type(get_season_team_stats(country = "AUS", gender = "F", season_end_year = 2021, stat_type = "league_table"), "list")

  # test that multiple genders can be passed to the function
  expect_type(get_season_team_stats(country = "AUS", gender = c("M", "F"), season_end_year = 2021, stat_type = "league_table"), "list")

  # test that incorrect URL will error
  expect_error(get_advanced_match_stats(match_url = "aaa.aaa"))

  # test that an invalid stat_type will error
  expect_error(get_season_team_stats(country = "AUS", gender = "F", season_end_year = 2021, stat_type = "test"))

})

