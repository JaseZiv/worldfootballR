context("Testing FBref functions")


test_that("player_dictionary_mapping() works", {
  testthat::skip_on_cran()
  mapped_players <- player_dictionary_mapping()
  expect_type(mapped_players, "list")
  expect_equal(ncol(mapped_players), 4)
})

test_that("fb_league_urls() works", {
  testthat::skip_on_cran()
  league_urls <- fb_league_urls(country = "ENG", gender = "M", season_end_year = 2021, tier = '2nd')
  expect_type(league_urls, "character")
  # test that there is more than just the baseline URL
  expect_false(grepl("https://fbref.com/en/$", league_urls[1]))
})


test_that("fb_team_urls() works", {
  testthat::skip_on_cran()
  team_urls <- fb_teams_urls("https://fbref.com/en/comps/9/Premier-League-Stats")
  expect_type(team_urls, "character")
  # test that there is more than just the baseline URL
  expect_false(grepl("https://fbref.com/en/$", team_urls[1]))
})


test_that("fb_player_urls() works", {
  testthat::skip_on_cran()
  player_urls <- fb_player_urls("https://fbref.com/en/squads/fd962109/Fulham-Stats")
  expect_type(player_urls, "character")
  # test that there is more than just the baseline URL
  expect_false(grepl("https://fbref.com/en/$", player_urls[1]))
})


test_that("get_advanced_match_stats() works", {
  testthat::skip_on_cran()
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
  test_urls <- c("https://fbref.com/en/matches/e0a84e7e/Southampton-Manchester-United-November-29-2020-Premier-League",
                 "https://fbref.com/en/matches/703db983/Paris-Saint-Germain-Toulouse-August-25-2019-Ligue-1")
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
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  test_df <- get_match_lineups(match_url = "https://fbref.com/en/matches/c0996cac/Bordeaux-Nantes-August-21-2020-Ligue-1")
  # test the functions returns the data
  expect_type(test_df, "list")
  # test that the correct number of columns returned
  expect_equal(ncol(test_df), 17)
  # test that incorrect URL will return empty data frame
  error_lineups <- get_match_lineups("aaa.aaa")
  expect_equal(nrow(error_lineups), 0)


})


test_that("get_match_report() works", {
  testthat::skip_on_cran()
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
  expect_equal(ncol(test_df), 21)

  # test that incorrect URL will error
  expect_error(get_match_report("aaa.aaa"))


})


test_that("get_match_results() works", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  test_df <- get_match_results(country = "ENG", gender = "M", season_end_year = 2022, tier="1st")
  # test the functions returns the data
  expect_type(test_df, "list")

  # test that multiple countries can be passed to the function
  test_df <- get_match_results(country = c("ENG", "AUS"), gender = "F", season_end_year = 2021, tier="1st")
  expect_type(test_df, "list")

  # test that incorrect Country will error
  expect_error(get_match_results(country = "BBB", gender = "F", season_end_year = 2021))


})


test_that("get_match_summary() works", {
  testthat::skip_on_cran()
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
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  test_urls <- get_match_urls(country = "AUS", gender = "F", season_end_year = 2020, tier="1st")
  # test the functions returns the data
  expect_type(test_urls, "character")
  # also test if it's a correct url:
  expect_equal(grepl("https://", test_urls[1]), TRUE)

  # test that incorrect url will error
  expect_equal(length(get_match_urls(country = "BBB", gender = "M", season_end_year = 2021, tier="1st")), 0)

  # friendly_int_2021_urls <- get_match_urls(country = "", gender = "M", season_end_year = 2021, tier = "", non_dom_league_url = "https://fbref.com/en/comps/218/history/Friendlies-M-Seasons")
  # expect_type(friendly_int_2021_urls, "character")

})




test_that("get_season_team_stats() works", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  # test the functions returns the data
  expect_type(get_season_team_stats(country = "AUS", gender = "F", season_end_year = 2021, tier="1st", stat_type = "league_table"), "list")
  expect_type(get_season_team_stats(country = "AUS", gender = "F", season_end_year = 2021, tier="1st", stat_type = "league_table_home_away"), "list")
  expect_type(get_season_team_stats(country = "AUS", gender = "F", season_end_year = 2021, tier="1st", stat_type = "standard"), "list")
  expect_type(get_season_team_stats(country = "ENG", gender = "M", season_end_year = 2021, tier="1st", stat_type = "keeper"), "list")
  expect_type(get_season_team_stats(country = "ENG", gender = "M", season_end_year = 2021, tier="1st", stat_type = "keeper_adv"), "list")
  expect_type(get_season_team_stats(country = "ENG", gender = "M", season_end_year = 2021, tier="1st", stat_type = "shooting"), "list")
  expect_type(get_season_team_stats(country = "ENG", gender = "M", season_end_year = 2021, tier="1st", stat_type = "passing"), "list")
  expect_type(get_season_team_stats(country = "ENG", gender = "M", season_end_year = 2021, tier="1st", stat_type = "passing_types"), "list")
  expect_type(get_season_team_stats(country = "ENG", gender = "M", season_end_year = 2021, tier="1st", stat_type = "goal_shot_creation"), "list")
  expect_type(get_season_team_stats(country = "ENG", gender = "M", season_end_year = 2021, tier="1st", stat_type = "defense"), "list")
  expect_type(get_season_team_stats(country = "ENG", gender = "M", season_end_year = 2021, tier="1st", stat_type = "possession"), "list")
  expect_type(get_season_team_stats(country = "ENG", gender = "M", season_end_year = 2021, tier="1st", stat_type = "playing_time"), "list")
  expect_type(get_season_team_stats(country = "ENG", gender = "M", season_end_year = 2021, tier="1st", stat_type = "misc"), "list")

  # test that multiple genders can be passed to the function
  expect_type(get_season_team_stats(country = "AUS", gender = c("M", "F"), season_end_year = 2021, tier="1st", stat_type = "league_table"), "list")

  # test that incorrect URL will error
  expect_equal(length(get_season_team_stats(country = "AUS", gender = "F", season_end_year = 2021, tier="1st", stat_type = "possession")), 0)

  # test that an invalid stat_type will error
  expect_error(get_season_team_stats(country = "AUS", gender = "F", season_end_year = 2021, tier="1st", stat_type = "test"))

  # test that MLS league table returns a new column for conference
  mls_table <- get_season_team_stats(country = "USA", gender = "M", season_end_year = 2021, tier="1st", stat_type = "league_table")
  expect_type(mls_table, "list")
  expect_true(any(grepl("Conference", names(mls_table))))

})


test_that("fb_big5_advanced_season_stats() works", {
  testthat::skip_on_cran()
  big5_team_shooting_multiple <- fb_big5_advanced_season_stats(season_end_year= c(2019:2021), stat_type= "shooting", team_or_player= "team")
  expect_type(big5_team_shooting_multiple, "list")
  expect_false(nrow(big5_team_shooting_multiple) == 0)

  big5_team_shooting <- fb_big5_advanced_season_stats(season_end_year= c(2022), stat_type= "shooting", team_or_player= "team")
  expect_type(big5_team_shooting, "list")
  expect_false(nrow(big5_team_shooting) == 0)

  big5_player_shooting <- fb_big5_advanced_season_stats(season_end_year= c(2022), stat_type= "shooting", team_or_player= "player")
  expect_type(big5_player_shooting, "list")
  expect_false(nrow(big5_player_shooting) == 0)
})


test_that("get_match_shooting() works", {
  testthat::skip_on_cran()
  shot_one_match <- get_match_shooting(match_url = "https://fbref.com/en/matches/a3eb7a37/Sheffield-United-Wolverhampton-Wanderers-September-14-2020-Premier-League")
  expect_type(shot_one_match, "list")
  expect_false(nrow(shot_one_match) == 0)

  test_urls_multiple <- c("https://fbref.com/en/matches/c0996cac/Bordeaux-Nantes-August-21-2020-Ligue-1",
                          "https://fbref.com/en/matches/9cbccb37/Dijon-Angers-August-22-2020-Ligue-1",
                          "https://fbref.com/en/matches/f96cd5a0/Lorient-Strasbourg-August-23-2020-Ligue-1")
  shot_multiple_matches <- get_match_shooting(test_urls_multiple)
  expect_type(shot_multiple_matches, "list")
  expect_false(nrow(shot_multiple_matches) == 0)
})


test_that("get_team_match_results() works", {
  testthat::skip_on_cran()
  # for single teams:
  man_city_2021_url <- "https://fbref.com/en/squads/b8fd03ef/Manchester-City-Stats"
  man_city_2021_results <- get_team_match_results(man_city_2021_url)
  expect_type(man_city_2021_results, "list")
  expect_false(nrow(man_city_2021_results) == 0)


  # get all team URLs for a league
  epl_2021_team_urls <- fb_teams_urls("https://fbref.com/en/comps/9/Premier-League-Stats")
  epl_2021_team_results <- get_team_match_results(team_url = epl_2021_team_urls)
  expect_type(epl_2021_team_results, "list")
  expect_false(nrow(epl_2021_team_results) == 0)
})


test_that("fb_player_scouting_report() works", {
  testthat::skip_on_cran()
  scout <- fb_player_scouting_report(player_url = "https://fbref.com/en/players/d70ce98e/Lionel-Messi",
                                     pos_versus = "primary")
  expect_type(scout, "list")
  expect_false(nrow(scout) == 0)

  scout_secondary <- fb_player_scouting_report(player_url = "https://fbref.com/en/players/d70ce98e/Lionel-Messi",
                                     pos_versus = "secondary")
  expect_type(scout, "list")
  expect_false(nrow(scout) == 0)
})


test_that("fb_player_season_stats() works", {
  testthat::skip_on_cran()
  mo_shooting <- fb_player_season_stats("https://fbref.com/en/players/e342ad68/Mohamed-Salah", stat_type = 'shooting')
  expect_type(mo_shooting, "list")
  expect_false(nrow(mo_shooting) == 0)

  multiple_playing_time <- fb_player_season_stats(player_url = c("https://fbref.com/en/players/d70ce98e/Lionel-Messi",
                                                                 "https://fbref.com/en/players/dea698d9/Cristiano-Ronaldo"),
                                                  stat_type = "playing_time")
  expect_type(multiple_playing_time, "list")
  expect_false(nrow(multiple_playing_time) == 0)
})


test_that("fb_player_match_logs() works", {
  testthat::skip_on_cran()
  ederson_summary <- fb_player_match_logs("https://fbref.com/en/players/3bb7b8b4/Ederson", season_end_year = 2021, stat_type = 'summary')
  expect_type(ederson_summary, "list")
  expect_false(nrow(ederson_summary) == 0)
})


test_that("fb_team_player_stats() works", {
  testthat::skip_on_cran()
  fleetwood_standard_stats <- fb_team_player_stats(team_urls= "https://fbref.com/en/squads/d6a369a2/Fleetwood-Town-Stats", stat_type= 'standard')
  expect_type(fleetwood_standard_stats, "list")
  expect_false(nrow(fleetwood_standard_stats) == 0)

  # this should error because the stat type isn't available
  expect_error(fb_team_player_stats(team_urls= "https://fbref.com/en/squads/d6a369a2/Fleetwood-Town-Stats", stat_type= 'keeper_adv'))
  # this should error because the spelling of the stat type is incorrect
  expect_error(fb_team_player_stats(team_urls= "https://fbref.com/en/squads/d6a369a2/Fleetwood-Town-Stats", stat_type= 'stangard'))

})


test_that("fb_team_match_log_stats() works", {
  testthat::skip_on_cran()
  liv_passing_log <- fb_team_match_log_stats(team_url= "https://fbref.com/en/squads/822bd0ba/Liverpool-Stats", stat_type= 'passing')
  expect_type(liv_passing_log, "list")
  expect_false(nrow(liv_passing_log) == 0)

  # this should error because the stat type isn't available
  expect_error(fb_team_match_log_stats(team_url = "https://fbref.com/en/squads/d6a369a2/Fleetwood-Town-Stats", stat_type= 'keeper_adv'))
  # this should error because the spelling of the stat type is incorrect
  expect_error(fb_team_player_stats(team_urls= "https://fbref.com/en/squads/d6a369a2/Fleetwood-Town-Stats", stat_type= 'shooooooting'))

})
