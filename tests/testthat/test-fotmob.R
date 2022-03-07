context("Testing fotmob functions")


test_that("fotmob_get_matches_by_date() works", {
  testthat::skip_on_cran()
  results <- fotmob_get_matches_by_date(date = c("20210925", "20210926"))
  expect_true(nrow(results) > 0)
  expect_equal(ncol(results), 12)
})

test_that("fotmob_get_league_matches() works", {
  testthat::skip_on_cran()

  league_matches <- fotmob_get_league_matches(
    country = "ENG",
    league_name = "Premier League"
  )

  expect_true(nrow(league_matches) > 0)
  expect_equal(ncol(league_matches), 11)

  league_matches <- fotmob_get_league_matches(
    league_id = 47
  )

  expect_true(nrow(league_matches) > 0)
  expect_equal(ncol(league_matches), 11)

  league_matches <- fotmob_get_league_matches(
    country =     c("ENG",            "ESP"   ),
    league_name = c("Premier League", "LaLiga")
  )

  expect_true(nrow(league_matches) > 0)
  expect_equal(ncol(league_matches), 11)


  league_matches_unnested <- league_matches %>%
    dplyr::select(match_id = id, home, away) %>%
    tidyr::unnest_wider(c(home, away), names_sep = "_")

  expect_true(nrow(league_matches_unnested) > 0)
  expect_equal(ncol(league_matches_unnested), 7)

  # doesn't exist
  expect_error(
    fotmob_get_league_matches(
      league_id = 1
    )
  )

  # must also provide league_name
  expect_error(
    fotmob_get_league_matches(
      country = "ENG"
    )
  )

  # must also provide country
  expect_error(
    fotmob_get_league_matches(
      league_name = "Premier League"
    )
  )

  # mis-specified league_name
  expect_error(
    fotmob_get_league_matches(
      country = "ESP",
      league_name = "La Liga"
    )
  )

  # "La Liga" should be "LaLiga". the function will work just for the Premier League
  expect_warning(
    fotmob_get_league_matches(
      country =     c("ENG",            "ESP"   ),
      league_name = c("Premier League", "La Liga")
    )
  )

  # "SPA" should be "ESP". the function will work just for the Premier League
  expect_warning(
    fotmob_get_league_matches(
      country =     c("ENG",            "SPA"   ),
      league_name = c("Premier League", "LaLiga")
    )
  )
})


test_that("fotmob_get_league_tables() works", {
  testthat::skip_on_cran()

  league_table <- fotmob_get_league_tables(
    country = "ENG",
    league_name = "Premier League"
  )

  expect_true(nrow(league_table) > 0)
  expect_equal(ncol(league_table), 14)

  league_table <- fotmob_get_league_tables(
    league_id = 47
  )

  expect_true(nrow(league_table) > 0)
  expect_equal(ncol(league_table), 14)

  league_table <- fotmob_get_league_tables(
    country =     c("ENG",            "ESP"   ),
    league_name = c("Premier League", "LaLiga")
  )

  expect_true(nrow(league_table) > 0)
  expect_equal(ncol(league_table), 14)

  table_types <- dplyr::distinct(league_table, table_type)
  expect_equal(
    table_types$table_type,
    c("all", "home", "away")
  )

})

test_that("fotmob_get_league_season_stats() works", {
  testthat::skip_on_cran()

  epl_team_xg_2021_a <- fotmob_get_season_stats(
    league_id = 47,
    season = "2020/2021",
    stat_type = "xg",
    team_or_player = "team"
  )
  expect_true(nrow(epl_team_xg_2021_a) > 0)
  expect_equal(ncol(epl_team_xg_2021_a), 18)

  get_epl_season_stats <- function(
    season = "2020/2021",
    team_or_player = "team",
    stat_type = "xg"
  ) {
    fotmob_get_season_stats(
      country = "ENG",
      league_name = "Premier League",
      season = season,
      team_or_player = team_or_player,
      stat_type = stat_type
    )
  }

  epl_team_xg_2021_b <- get_epl_season_stats()

  expect_identical(epl_team_xg_2021_a, epl_team_xg_2021_b)

  ## fotmob doesn't has data for 2016/2017 for some leagues, but not all
  expect_warning(
    get_epl_season_stats(
      season = "2016/2017"
    ),
    regexp = "Issue with data"
  )

  ## fotmob doesn't have data this far back for any stat or league
  expect_error(
    get_epl_season_stats(
      season = "2010/2011"
    ),
    regexp = "matching parameters"
  )

  epl_player_xg_2021 <- get_epl_season_stats(
    team_or_player = "player"
  )
  expect_true(nrow(epl_player_xg_2021) > 0)
  expect_equal(ncol(epl_player_xg_2021), 18)

  ## similar to team test
  expect_warning(
    get_epl_season_stats(
      season = "2016/2017",
      team_or_player = "player"
    ),
    regexp = "Issue with data"
  )

  ## similar to team test
  expect_error(
    get_epl_season_stats(
      season = "2010/2011",
      team_or_player = "player"
    ),
    regexp = "matching parameters"
  )

  ## more than one `stat_type` is not allowed
  expect_error(
    get_epl_season_stats(
      stat_type = c("xg", "xg_conceded")
    )
  )

  ## more than one `team_or_player` is not allowed
  expect_error(
    get_epl_season_stats(
      team_or_player = c("team", "player")
    )
  )

  ## invalid `stat_type`
  expect_error(
    get_epl_season_stats(
      stat_type = "foo"
    )
  )

  ## invalid `team_or_player`
  expect_error(
    get_epl_season_stats(
      team_or_player = "foo"
    )
  )
})

test_that("fotmob_get_match_details() works", {
  testthat::skip_on_cran()
  details <- fotmob_get_match_details(c(3609987, 3609979))
  expect_true(nrow(details) > 0)
  expect_equal(ncol(details), 15)
})


test_that("fotmob_get_match_players() works", {
  testthat::skip_on_cran()
  players <- fotmob_get_match_players(c(3609987, 3609979))
  expect_true(nrow(players) > 0)
  expect_equal(ncol(players), 29)
})

