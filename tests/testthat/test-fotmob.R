context("Testing fotmob functions")


test_that("fotmob_get_matches_by_date() works", {
  testthat::skip_on_cran()

  td <- Sys.Date()
  results <- fotmob_get_matches_by_date(date = as.character(c(td, td-1)))
  expect_gt(nrow(results), 0)
  expect_equal(
    colnames(results),
    c("ccode", "id", "primary_id", "name", "match_id", "match_league_id", "match_time", "home_id", "home_score", "home_name", "home_long_name", "home_pen_score", "away_id", "away_score", "away_name", "away_long_name", "away_pen_score", "match_eliminated_team_id", "match_status_id", "match_tournament_stage", "match_status_finished", "match_status_started", "match_status_cancelled", "match_status_score_str", "match_status_start_date_str", "match_status_start_date_str_short", "short", "long", "match_status_ongoing", "match_status_start_time_str", "match_status_awarded", "match_status_aggregated_str", "match_time_ts", "parent_league_id", "internal_rank", "live_rank", "simple_league", "is_group", "group_name", "parent_league_name")

  )
})

test_that("fotmob_get_league_matches() works", {
  testthat::skip_on_cran()

  expected_league_matches_cols <- c("id", "page_url", "opponent", "home", "away", "display_tournament", "lname_arr", "color", "not_started", "tournament", "status")
  epl_league_matches <- fotmob_get_league_matches(
    country = "ENG",
    league_name = "Premier League"
  )

  expect_gt(nrow(epl_league_matches), 0)
  expect_equal(colnames(epl_league_matches), expected_league_matches_cols)

  epl_league_matches <- fotmob_get_league_matches(
    league_id = 47
  )

  expect_gt(nrow(epl_league_matches), 0)
  expect_equal(colnames(epl_league_matches), expected_league_matches_cols)

  ## test cached
  epl_league_matches <- fotmob_get_league_matches(
    league_id = 47,
    cached = FALSE
  )

  expect_gt(nrow(epl_league_matches), 0)
  expect_equal(colnames(epl_league_matches), expected_league_matches_cols)

  ## MLS is usually in-season when European leagues are out-of-season, so it's useful
  ##   for checking that stats work in the off-season
  mls_league_matches <- fotmob_get_league_matches(
    league_id = 130
  )

  expect_gt(nrow(mls_league_matches), 0)
  expect_equal(colnames(mls_league_matches), expected_league_matches_cols)

  epl_ll_league_matches <- fotmob_get_league_matches(
    country =     c("ENG",            "ESP"   ),
    league_name = c("Premier League", "LaLiga")
  )

  expect_gt(nrow(epl_ll_league_matches), 0)
  expect_equal(colnames(epl_ll_league_matches), expected_league_matches_cols)


  epl_ll_league_matches_unnested <- epl_ll_league_matches %>%
    dplyr::select(match_id = id, home, away) %>%
    tidyr::unnest_wider(c(home, away), names_sep = "_")

  expect_gt(nrow(epl_ll_league_matches_unnested), 0)
  expect_equal(
    colnames(epl_ll_league_matches_unnested),
    c("match_id", "home_id", "home_name", "home_score", "away_id", "away_name", "away_score")
  )

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

  expected_domestic_league_table_cols <- c("league_id", "page_url", "table_type", "table_name", "table_short_name", "table_id", "table_page_url", "table_deduction", "table_ongoing", "table_played", "table_wins", "table_draws", "table_losses", "table_scores_str", "table_goal_con_diff", "table_pts", "table_idx", "table_qual_color")
  epl_league_table <- fotmob_get_league_tables(
    country = "ENG",
    league_name = "Premier League"
  )

  ## should be 20 teams x 3 table types = 60
  expect_gt(nrow(epl_league_table), 0)
  expect_equal(colnames(epl_league_table), expected_domestic_league_table_cols)

  epl_league_table <- fotmob_get_league_tables(
    league_id = 47
  )

  expect_gt(nrow(epl_league_table), 0)
  expect_equal(colnames(epl_league_table), expected_domestic_league_table_cols)

  ## see not about MLS from before
  mls_league_table <- fotmob_get_league_tables(
    league_id = 130
  )

  expect_gt(nrow(mls_league_table), 0)
  ## MLS typically has 4 extra columns, sometimes 5. Don't check for the fifth, "ongoing", since it depends on the time of year.
  expect_equal(
    sort(setdiff(colnames(mls_league_table), "ongoing")),
    sort(c(expected_domestic_league_table_cols, "ccode", "group_id", "group_page_url", "group_name"))
  )

  epl_ll_league_tables <- fotmob_get_league_tables(
    country =     c("ENG",            "ESP"   ),
    league_name = c("Premier League", "LaLiga")
  )

  ## should be 2 leagues x 20 teams x 3 table types = 120
  expect_gt(nrow(epl_ll_league_tables), 0)
  expect_equal(colnames(epl_ll_league_tables), expected_domestic_league_table_cols)

  table_types <- dplyr::distinct(epl_ll_league_tables, table_type)
  expect_equal(
    table_types$table_type,
    c("all", "home", "away")
  )

  ## non-domestic league
  ## Can only check on CL after group stages and briefly after the final.
  m <- lubridate::month(Sys.Date())
  if(m >= 1 && m <= 5) {
    cl_league_table <- fotmob_get_league_tables(
      country =     "INT",
      league_name = "Champions League"
    )

    ## should be 32 teams x 3 table types = 96
    expect_gt(nrow(cl_league_table), 0)
    expect_equal(colnames(cl_league_table), expected_league_matches_cols)
  }
})

test_that("fotmob_get_season_stats() works", {
  testthat::skip_on_cran()

  expected_stat_cols <- c("country", "league_name", "league_id", "season_name", "season_id", "stat_league_name", "stat_name", "stat", "participant_name", "particiant_id", "team_id", "team_color", "stat_value", "sub_stat_value", "minutes_played", "matches_played", "stat_value_count", "rank", "participant_country_code", "team_name")
  epl_team_xg_21_a <- fotmob_get_season_stats(
    league_id = 47,
    season_name = "2020/2021",
    stat_name = "Expected goals",
    team_or_player = "team"
  )
  expect_gt(nrow(epl_team_xg_21_a), 0)
  expect_equal(colnames(epl_team_xg_21_a), expected_stat_cols)

  get_epl_season_stats <- function(
    season_name = "2020/2021",
    team_or_player = "team",
    stat_name = ifelse(team_or_player == "team", "Expected goals", "Expected goals (xG)")
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

  ## Liga MX has season ids with hyphens
  liga_mx_team_xg_21 <- fotmob_get_season_stats(
    country = "MEX",
    league_name = "Liga MX",
    season = "2021/2022-Clausura",
    stat_name = "Expected goals",
    team_or_player = "team"
  )
  expect_gt(nrow(liga_mx_team_xg_21), 0)
  expect_equal(colnames(liga_mx_team_xg_21), expected_stat_cols)

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
  expect_equal(colnames(epl_player_xg_21), expected_stat_cols)

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
  m <- lubridate::month(Sys.Date())
  if(m >= 1 && m <= 5) {
    cl_team_xg_21 <- fotmob_get_season_stats(
      league_id = 42,
      season_name = "2020/2021",
      stat_name = "Expected goals",
      team_or_player = "team"
    )
    expect_gt(nrow(cl_team_xg_21), 0)
    expect_equal(colnames(cl_team_xg_21), expected_stat_cols)
  }

  ## see not about MLS from before
  mls_team_xg_21 <- fotmob_get_season_stats(
    league_id = 130,
    season_name = "2021",
    stat_name = "Expected goals",
    team_or_player = "team"
  )
  expect_gt(nrow(mls_team_xg_21), 0)
  expect_equal(colnames(mls_team_xg_21), expected_stat_cols)

  ## multiple leagues
  epl_ll_team_xg_21 <- fotmob_get_season_stats(
    league_id = c(47, 87),
    season_name = "2020/2021",
    stat_name = "Expected goals",
    team_or_player = "team"
  )

  expect_gt(nrow(epl_ll_team_xg_21), nrow(epl_team_xg_21_a))
  expect_equal(colnames(epl_ll_team_xg_21), expected_stat_cols)

  ## multiple seasons
  epl_team_xg_2122 <- get_epl_season_stats(
    season_name = c("2020/2021", "2021/2022")
  )

  expect_gt(nrow(epl_team_xg_2122), nrow(epl_team_xg_21_a))
  expect_equal(colnames(epl_team_xg_2122), expected_stat_cols)

  ## more than one stat
  epl_team_xgs_21 <- get_epl_season_stats(
    stat_name = c("Expected goals", "xG conceded")
  )
  expect_gt(nrow(epl_team_xgs_21), nrow(epl_team_xg_21_a))
  expect_equal(colnames(epl_team_xgs_21), expected_stat_cols)

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
  expect_equal(colnames(epl_ll_team_xgs_2122), expected_stat_cols)
})

test_that("fotmob_get_match_info() works", {
  testthat::skip_on_cran()

  expected_match_info_cols <- c("match_id", "match_round", "league_id", "league_name", "league_round_name", "parent_league_id", "parent_league_season", "match_time_utc", "home_team_id", "home_team", "home_team_color", "away_team_id", "away_team", "away_team_color", "match_date_date_formatted", "match_date_time_formatted", "tournament_id", "tournament_link", "tournament_league_name", "tournament_round", "stadium_name", "stadium_city", "stadium_country", "stadium_lat", "stadium_long", "referee_img_url", "referee_text", "referee_country", "attendance")
  match_info <- fotmob_get_match_info(c(3609987, 3609979))

  expect_gt(nrow(match_info), 0)
  expect_equal(colnames(match_info), expected_match_info_cols)

  ## non-domestic match
  match_info <- fotmob_get_match_info(3846342)
  expect_gt(nrow(match_info), 0)
  expect_equal(colnames(match_info), expected_match_info_cols)
})

test_that("fotmob_get_match_team_stats() works", {
  testthat::skip_on_cran()

  expected_match_team_stats_cols <- c("match_id", "match_round", "league_id", "league_name", "league_round_name", "parent_league_id", "parent_league_season", "match_time_utc", "home_team_id", "home_team", "home_team_color", "away_team_id", "away_team", "away_team_color", "title", "stats_title", "home_value", "away_value", "stats_type", "stats_highlighted")
  match_team_stats <- fotmob_get_match_team_stats(c(3609987, 3609979))

  expect_gt(nrow(match_team_stats), 0)
  expect_equal(colnames(match_team_stats), expected_match_team_stats_cols)

  ## non-domestic match
  match_team_stats <- fotmob_get_match_team_stats(3846342)
  expect_gt(nrow(match_team_stats), 0)
  expect_equal(colnames(match_team_stats), expected_match_team_stats_cols)
})

test_that("fotmob_get_match_details() works", {
  testthat::skip_on_cran()

  expected_match_detail_cols <- c("match_id", "match_round", "league_id", "league_name", "league_round_name", "parent_league_id", "parent_league_season", "match_time_utc", "home_team_id", "home_team", "home_team_color", "away_team_id", "away_team", "away_team_color", "id", "event_type", "team_id", "player_id", "player_name", "x", "y", "min", "min_added", "is_blocked", "is_on_target", "blocked_x", "blocked_y", "goal_crossed_y", "goal_crossed_z", "expected_goals", "expected_goals_on_target", "shot_type", "situation", "period", "is_own_goal", "on_goal_shot_x", "on_goal_shot_y", "on_goal_shot_zoom_ratio", "first_name", "last_name", "team_color")
  details <- fotmob_get_match_details(c(3609987, 3609979))

  expect_gt(nrow(details), 0)
  expect_equal(colnames(details), expected_match_detail_cols)

  ## non-domestic match
  details <- fotmob_get_match_details(3846342)
  expect_gt(nrow(details), 0)
  expect_equal(colnames(details), expected_match_detail_cols)
})

test_that("fotmob_get_match_players() works", {
  testthat::skip_on_cran()

  ## shouldn't test for exact equality since not all stats may appear for a game (if a no player in the game registers a value for a given stat)
  expected_match_player_cols <- c("match_id", "team_id", "team_name", "id", "using_opta_id", "first_name", "last_name", "image_url", "page_url", "shirt", "is_home_team", "time_subbed_on", "time_subbed_off", "usual_position", "position_row", "role", "is_captain", "subbed_out", "g", "rating_num", "rating_bgcolor", "is_top_rating", "is_match_finished", "fantasy_score_num", "fantasy_score_bgcolor", "home_team_id", "home_team_color", "away_team_id", "away_team_color", "stats_fot_mob_rating", "stats_minutes_played", "stats_saves", "stats_goals", "stats_goals_conceded", "stats_x_got_faced", "stats_accurate_passes", "stats_accurate_long_balls", "stats_diving_save", "stats_saves_inside_box", "stats_acted_as_sweeper", "stats_punches", "stats_throws", "stats_high_claim", "stats_recoveries", "stats_fantasy_points", "stats_touches", "stats_assists", "stats_total_shots", "stats_shotmap", "stats_chances_created", "stats_expected_goals_x_g", "stats_expected_goals_on_target_x_got", "stats_expected_assists_x_a", "stats_shot_accuracy", "stats_successful_dribbles", "stats_accurate_crosses", "stats_dispossessed", "stats_tackles_won", "stats_clearances", "stats_dribbled_past", "stats_ground_duels_won", "stats_aerial_duels_won", "stats_was_fouled", "stats_fouls_committed", "stats_blocks", "stats_interceptions", "stats_blocked_shots", "stats_corners", "stats_offsides", "stats_headed_clearance", "stats_big_chance_missed", "shotmap", "is_starter", "stats_error_led_to_goal")

  players <- fotmob_get_match_players(c(3609987, 3609979))
  expect_gt(nrow(players), 0)
  expect_true(all(colnames(players) %in% expected_match_player_cols))

  ## non-domestic league
  players <- fotmob_get_match_players(3846347)
  expect_gt(nrow(players), 0)
  expect_true(all(colnames(players) %in% expected_match_player_cols))
})

