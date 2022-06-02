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


})


test_that("load_fb_big5_advanced_season_stats() works", {
  testthat::skip_on_cran()
  big5_team_shooting_multiple <- load_fb_big5_advanced_season_stats(season_end_year= c(2019:2021), stat_type= "shooting", team_or_player= "team")
  expect_type(big5_team_shooting_multiple, "list")
  expect_false(nrow(big5_team_shooting_multiple) == 0)

  big5_team_shooting <- load_fb_big5_advanced_season_stats(season_end_year= c(2022), stat_type= "shooting", team_or_player= "team")
  expect_type(big5_team_shooting, "list")
  expect_false(nrow(big5_team_shooting) == 0)

})
