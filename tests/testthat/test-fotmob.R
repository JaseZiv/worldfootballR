context("Testing fotmob functions")


test_that("fotmob_get_matches_by_date() works", {
  testthat::skip_on_cran()

  results <- fotmob_get_matches_by_date(date = c("20210925", "20210926"))
  expect_gt(nrow(results), 0)
  expect_equal(ncol(results), 12)
})

test_that("fotmob_get_league_matches() works", {
  testthat::skip_on_cran()

  league_matches <- fotmob_get_league_matches(
    country = "ENG",
    league_name = "Premier League"
  )

  expect_gt(nrow(league_matches), 0)
  expect_equal(ncol(league_matches), 11)

  league_matches <- fotmob_get_league_matches(
    league_id = 47
  )

  expect_gt(nrow(league_matches), 0)
  expect_equal(ncol(league_matches), 11)

  ## test cached
  league_matches <- fotmob_get_league_matches(
    league_id = 47,
    cached = FALSE
  )

  expect_gt(nrow(league_matches), 0)
  expect_equal(ncol(league_matches), 11)

  league_matches <- fotmob_get_league_matches(
    country =     c("ENG",            "ESP"   ),
    league_name = c("Premier League", "LaLiga")
  )

  expect_gt(nrow(league_matches), 0)
  expect_equal(ncol(league_matches), 11)


  league_matches_unnested <- league_matches %>%
    dplyr::select(match_id = id, home, away) %>%
    tidyr::unnest_wider(c(home, away), names_sep = "_")

  expect_gt(nrow(league_matches_unnested), 0)
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

  n_expected_domestic_league_table_cols <- 16
  epl_league_table <- fotmob_get_league_tables(
    country = "ENG",
    league_name = "Premier League"
  )

  ## should be 20 teams x 3 table types = 60
  expect_gt(nrow(epl_league_table), 0)
  expect_equal(ncol(epl_league_table), n_expected_domestic_league_table_cols)

  epl_league_table <- fotmob_get_league_tables(
    league_id = 47
  )

  expect_gt(nrow(epl_league_table), 0)
  expect_equal(ncol(epl_league_table), n_expected_domestic_league_table_cols)

  epl_ll_league_tables <- fotmob_get_league_tables(
    country =     c("ENG",            "ESP"   ),
    league_name = c("Premier League", "LaLiga")
  )

  ## should be 2 leagues x 20 teams x 3 table types = 120
  expect_gt(nrow(epl_ll_league_tables), 0)
  expect_equal(ncol(epl_ll_league_tables), n_expected_domestic_league_table_cols)

  table_types <- dplyr::distinct(epl_ll_league_tables, table_type)
  expect_equal(
    table_types$table_type,
    c("all", "home", "away")
  )

  ## non-domestic league
  n_expected_int_league_table_cols <- 20
  cl_league_table <- fotmob_get_league_tables(
    country =     "INT",
    league_name = "Champions League",
    cached = FALSE
  )

  ## should be 32 teams x 3 table types = 96
  expect_gt(nrow(cl_league_table), 0)
  expect_equal(ncol(cl_league_table), n_expected_int_league_table_cols)

})

test_that("fotmob_get_season_stats() works", {
  testthat::skip_on_cran()

  n_expected_stat_cols <- 20
  epl_team_xg_21_a <- fotmob_get_season_stats(
    league_id = 47,
    season_name = "2020/2021",
    stat_name = "Expected goals",
    team_or_player = "team"
  )
  expect_gt(nrow(epl_team_xg_21_a), 0)
  expect_equal(ncol(epl_team_xg_21_a), n_expected_stat_cols)

  get_epl_season_stats <- function(
    season_name = "2020/2021",
    team_or_player = "team",
    stat_name = "Expected goals"
  ) {
    fotmob_get_season_stats(
      country = "ENG",
      league_name = "Premier League",
      season_name = season_name,
      team_or_player = team_or_player,
      stat_name = stat_name
    )
  }

  epl_team_xg_21_b <- get_epl_season_stats()

  expect_identical(epl_team_xg_21_a, epl_team_xg_21_b)

  ## fotmob has data for 2016/2017 for some leagues and stats, but not all
  expect_warning(
    get_epl_season_stats(
      season_name = "2016/2017"
    ),
    regexp = "Issue with data"
  )

  ## fotmob doesn't have data this far back for any stat or league
  expect_message(
    get_epl_season_stats(
      season_name = "2010/2011"
    ),
    regexp = "not found"
  )

  epl_player_xg_21 <- get_epl_season_stats(
    team_or_player = "player"
  )
  expect_gt(nrow(epl_player_xg_21), 0)
  expect_equal(ncol(epl_player_xg_21), n_expected_stat_cols)

  ## similar to team test
  expect_warning(
    get_epl_season_stats(
      season = "2016/2017",
      team_or_player = "player"
    ),
    regexp = "Issue with data"
  )

  ## similar to team test
  expect_message(
    get_epl_season_stats(
      season = "2010/2011",
      team_or_player = "player"
    ),
    regexp = "not found"
  )

  ## more than one `team_or_player` is not allowed
  expect_error(
    get_epl_season_stats(
      team_or_player = c("team", "player")
    )
  )

  ## invalid `stat_name`
  expect_error(
    get_epl_season_stats(
      stat_name = "foo"
    )
  )

  ## invalid `team_or_player`
  expect_error(
    get_epl_season_stats(
      team_or_player = "foo"
    )
  )

  ## Does this work for an international tournament?
  cl_team_xg_21 <- fotmob_get_season_stats(
    league_id = 42,
    season_name = "2020/2021",
    stat_name = "Expected goals",
    team_or_player = "team"
  )
  expect_gt(nrow(cl_team_xg_21), 0)
  expect_equal(ncol(cl_team_xg_21), n_expected_stat_cols)

  ## multiple leagues
  epl_ll_team_xg_21 <- fotmob_get_season_stats(
    league_id = c(47, 87),
    season_name = "2020/2021",
    stat_name = "Expected goals",
    team_or_player = "team"
  )

  expect_gt(nrow(epl_ll_team_xg_21), nrow(epl_team_xg_21_a))
  expect_equal(ncol(epl_ll_team_xg_21), n_expected_stat_cols)

  ## multiple seasons
  epl_team_xg_2122 <- get_epl_season_stats(
    season_name = c("2020/2021", "2021/2022")
  )

  expect_gt(nrow(epl_team_xg_2122), nrow(epl_team_xg_21_a))
  expect_equal(ncol(epl_team_xg_2122), n_expected_stat_cols)

  ## more than one stat
  epl_team_xgs_21 <- get_epl_season_stats(
    stat_name = c("Expected goals", "xG conceded")
  )
  expect_gt(nrow(epl_team_xgs_21), nrow(epl_team_xg_21_a))
  expect_equal(ncol(epl_team_xgs_21), n_expected_stat_cols)

  ## multiple leagues, seasons, and stats
  epl_ll_team_xgs_2122 <- fotmob_get_season_stats(
    league_id = c(47, 87),
    season_name = c("2020/2021", "2021/2022"),
    stat_name = c("Expected goals", "xG conceded"),
    team_or_player = "team"
  )

  expect_gt(nrow(epl_ll_team_xgs_2122), nrow(epl_ll_team_xg_21))
  expect_gt(nrow(epl_ll_team_xgs_2122), nrow(epl_team_xg_2122))
  expect_gt(nrow(epl_ll_team_xgs_2122), nrow(epl_team_xgs_21))
  expect_equal(ncol(epl_ll_team_xgs_2122), n_expected_stat_cols)
})

test_that("fotmob_get_match_details() works", {
  testthat::skip_on_cran()

  n_expected_match_detail_cols <- 15
  details <- fotmob_get_match_details(c(3609987, 3609979))

  ## 1 row per match
  expect_gt(nrow(details), 0)
  expect_equal(ncol(details), n_expected_match_detail_cols)

  ## non-domestic match
  details <- fotmob_get_match_details(3846342)
  expect_gt(nrow(details), 0)
  expect_equal(ncol(details), n_expected_match_detail_cols)
})


test_that("fotmob_get_match_players() works", {
  testthat::skip_on_cran()

  n_expected_match_player_cols <- 32
  players <- fotmob_get_match_players(c(3609987, 3609979))
  expect_gt(nrow(players), 0)
  expect_equal(ncol(players), n_expected_match_player_cols)

  ## non-domestic league
  players <- fotmob_get_match_players(3846347)
  expect_gt(nrow(players), 0)
  expect_equal(ncol(players), n_expected_match_player_cols)
})

