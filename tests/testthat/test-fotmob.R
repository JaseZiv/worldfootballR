context("Testing fotmob functions")


test_that("fotmob_get_matches_by_date() works", {
  testthat::skip_on_cran()
  results <- fotmob_get_matches_by_date(date = c("20210925", "20210926"))
  expect_equal(nrow(results), 268)
  expect_equal(ncol(results), 12)
})

test_that("fotmob_get_league_matches() works", {
  testthat::skip_on_cran()

  league_matches <- fotmob_get_league_matches(
    country = "ENG",
    league_name = "Premier League"
  )

  expect_equal(nrow(league_matches), 380)
  expect_equal(ncol(league_matches), 11)

  league_matches <- fotmob_get_league_matches(
    league_id = 47
  )

  expect_equal(nrow(league_matches), 380)
  expect_equal(ncol(league_matches), 11)

  league_matches <- fotmob_get_league_matches(
    country =     c("ENG",            "ESP"   ),
    league_name = c("Premier League", "LaLiga")
  )

  expect_equal(nrow(league_matches), 760)
  expect_equal(ncol(league_matches), 11)


  league_matches_unnested <- league_matches %>%
    dplyr::select(match_id = id, home, away) %>%
    tidyr::unnest_wider(c(home, away), names_sep = "_")

  expect_equal(nrow(league_matches_unnested), 760)
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
      country = "Premier League"
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

  expect_equal(nrow(league_table), 60)
  expect_equal(ncol(league_table), 14)

  league_table <- fotmob_get_league_tables(
    league_id = 47
  )

  expect_equal(nrow(league_table), 60)
  expect_equal(ncol(league_table), 14)

  league_table <- fotmob_get_league_tables(
    country =     c("ENG",            "ESP"   ),
    league_name = c("Premier League", "LaLiga")
  )

  expect_equal(nrow(league_table), 120)
  expect_equal(ncol(league_table), 14)

  table_types <- dplyr::distinct(league_table, table_type)
  expect_equal(
    table_types$table_type,
    c("all", "home", "away")
  )

})

test_that("fotmob_get_match_details() works", {
  testthat::skip_on_cran()
  details <- fotmob_get_match_details(c(3609987, 3609979))
  expect_equal(nrow(details), 2)
  expect_equal(ncol(details), 15)
})


test_that("fotmob_get_match_players() works", {
  testthat::skip_on_cran()
  players <- fotmob_get_match_players(c(3609987, 3609979))
  expect_equal(nrow(players), 80)
  expect_equal(ncol(players), 29)
})

