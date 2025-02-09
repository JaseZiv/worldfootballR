context("Testing Transfermarkt functions")

test_that("tm_player_market_values() works", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  # test that multiple countries can be passed to the function
  expect_type(tm_player_market_values(country_name = c("Australia", "Croatia"), start_year = 2020), "list")

  # test that an invalid country will error
  expect_error(tm_player_market_values(country_name = "Fake Country", start_year = 2020))

})

test_that("tm_player_transfer_history() works", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  ## multiple URLs
  transfer_data <- tm_player_transfer_history(
    c(
      "https://www.transfermarkt.com/cristiano-ronaldo/profil/spieler/8198",
      "https://www.transfermarkt.com/jack-rodwell/profil/spieler/57079"
    )
  )

  expected_base_transfer_data_cols <- c(
    "player_name",
    "season",
    "transfer_date",
    "team_from",
    "team_to",
    "market_value",
    "transfer_value",
    "transfer_type"
  )
  expected_extra_transfer_data_cols <- c(
    "country_from",
    "country_to",
    "contract_expiry",
    "days_remaining"
  )

  # test the functions returns the data
  expect_type(transfer_data, "list")
  expect_true(nrow(transfer_data) > 0)
  expect_setequal(
    colnames(transfer_data), c(expected_base_transfer_data_cols, expected_extra_transfer_data_cols)
  )
  expect_true(length(unique(transfer_data$player_name)) == 2L)

  ## non default params
  transfer_data_no_extra_info <- tm_player_transfer_history(
    "https://www.transfermarkt.com/jack-rodwell/profil/spieler/57079",
    get_extra_info = FALSE
  )

  expect_true(nrow(transfer_data_no_extra_info) > 0)
  expect_setequal(
    colnames(transfer_data_no_extra_info), expected_base_transfer_data_cols
  )

  # test that an invalid country will error
  expect_error(tm_player_transfer_history("aaa.com.au"))

})


test_that("tm_team_transfers() works", {
  testthat::skip_on_cran()
  bayern_summer <- tm_team_transfers(team_url = "https://www.transfermarkt.com/fc-bayern-munchen/startseite/verein/27/saison_id/2020", transfer_window = "summer")
  expect_type(bayern_summer, "list")
  expect_false(nrow(bayern_summer) == 0)
  expect_false(any(is.na(bayern_summer$player_name)))

  # test multiple urls:
  urls <- c("https://www.transfermarkt.com/fc-burnley/startseite/verein/1132/saison_id/2020",
            "https://www.transfermarkt.com/fc-bayern-munchen/startseite/verein/27/saison_id/2020")
  multi <- tm_team_transfers(team_url = urls, transfer_window = "all")
  expect_false(nrow(multi) == 0)
  expect_false(any(is.na(multi$player_name)))

})


test_that("tm_matchday_table() works", {
  testthat::skip_on_cran()
  expect_type(tm_matchday_table(country_name="England", start_year="2021", matchday=1), "list")
  expect_type(tm_matchday_table(country_name="England", start_year="2021", matchday=c(1:5)), "list")
})


test_that("tm_league_team_urls() works", {
  testthat::skip_on_cran()
  team_urls <- tm_league_team_urls(country_name = "England", start_year = 2021)
  expect_type(team_urls, "character")
  expect_true(length(team_urls) == 20)
})


test_that("tm_team_player_urls() works", {
  testthat::skip_on_cran()
  player_urls <- tm_team_player_urls(team_url = "https://www.transfermarkt.com/fc-burnley/startseite/verein/1132/saison_id/2020")
  expect_type(player_urls, "character")
})


test_that("tm_squad_stats() works", {
  testthat::skip_on_cran()
  bayern <- tm_squad_stats(team_url = "https://www.transfermarkt.com/fc-bayern-munchen/startseite/verein/27/saison_id/2020")
  expect_type(bayern, "list")
})


test_that("tm_player_bio() works", {
  testthat::skip_on_cran()
  burnley_player_urls <- tm_team_player_urls(team_url = "https://www.transfermarkt.com/fc-burnley/startseite/verein/1132/saison_id/2020")
  # then pass all those URLs to the tm_player_bio
  burnley_bios <- tm_player_bio(player_urls = burnley_player_urls[1:3])
  expect_type(burnley_bios, "list")
  expect_false(nrow(burnley_bios)==0)

})



test_that("tm_team_staff_urls() works", {
  testthat::skip_on_cran()

  # get a list of team URLs for the EPL 2021/22 season
  teams <- c("https://www.transfermarkt.com/manchester-city/startseite/verein/281/saison_id/2021",
             "https://www.transfermarkt.com/fc-chelsea/startseite/verein/631/saison_id/2021")
  # get all EPL managers for the 2021/22 season
  epl_managers <- tm_team_staff_urls(team_urls = teams, staff_role = "Manager")
  expect_type(epl_managers, "character")
  # test that there is more than just the baseline URL
  expect_false(all(grepl("https://www.transfermarkt.com$", epl_managers)))

  # get all EPL goal keeping coaches for the 2021/22 season
  epl_gk_coaches <- tm_team_staff_urls(team_urls = teams, staff_role = "Goalkeeping Coach")
  expect_type(epl_gk_coaches, "character")
  # test that there is more than just the baseline URL
  expect_false(all(grepl("https://www.transfermarkt.com$", epl_gk_coaches)))

})


test_that("tm_team_staff_history() works", {
  testthat::skip_on_cran()

  # get a list of team URLs for the EPL 2021/22 season
  teams <- c("https://www.transfermarkt.com/manchester-city/startseite/verein/281/saison_id/2021",
            "https://www.transfermarkt.com/fc-chelsea/startseite/verein/631/saison_id/2021")
  # get all EPL managers for the 2021/22 season
  epl_club_managers <- tm_team_staff_history(team_urls = teams, staff_role = "Manager")
  expect_type(epl_club_managers, "list")
  expect_false(nrow(epl_club_managers) == 0)
})


test_that("tm_team_staff_history() works", {
  testthat::skip_on_cran()

  managers <- c("https://www.transfermarkt.com/pep-guardiola/profil/trainer/5672",
                "https://www.transfermarkt.com/thomas-tuchel/profil/trainer/7471")
  epl_manager_job_histories <- tm_staff_job_history(staff_urls = managers)
  expect_type(epl_manager_job_histories, "list")
  expect_false(nrow(epl_manager_job_histories) == 0)

  gk_coaches <- c("https://www.transfermarkt.com/xabier-mancisidor/profil/trainer/17486",
                  "https://www.transfermarkt.com/richard-hartis/profil/trainer/19348")

  epl_gk_coach_job_histories <- tm_staff_job_history(staff_urls = gk_coaches)
  expect_type(epl_gk_coach_job_histories, "list")
  expect_false(nrow(epl_gk_coach_job_histories) == 0)
})


test_that("tm_league_debutants() works", {
  testthat::skip_on_cran()

  laliga_debutants <- tm_league_debutants(country_name = "Spain", debut_type = "league", debut_start_year = 2021, debut_end_year = 2021)
  expect_type(laliga_debutants, "list")
  expect_equal(ncol(laliga_debutants), 20)
  expect_false(nrow(laliga_debutants) == 0)

  league_one_PRO_debutants <- tm_league_debutants(country_name = "",
                                                  league_url = "https://www.transfermarkt.com/league-one/startseite/wettbewerb/GB3",
                                                  debut_type = "pro", debut_start_year = 2021, debut_end_year = 2021)
  expect_type(league_one_PRO_debutants, "list")
  expect_equal(ncol(league_one_PRO_debutants), 20)
  expect_false(nrow(league_one_PRO_debutants) == 0)
})


test_that("tm_expiring_contracts() works", {
  testthat::skip_on_cran()

  laliga_expiring <- tm_expiring_contracts(country_name = "Spain", contract_end_year = 2024)
  expect_type(laliga_expiring, "list")
  expect_equal(ncol(laliga_expiring), 14)
  expect_false(nrow(laliga_expiring) == 0)


  league_one_expiring <- tm_expiring_contracts(country_name = "",
                                               contract_end_year = 2024,
                                               league_url = "https://www.transfermarkt.com/league-one/startseite/wettbewerb/GB3")
  expect_type(league_one_expiring, "list")
  expect_equal(ncol(league_one_expiring), 14)
  expect_false(nrow(league_one_expiring) == 0)

})


test_that("tm_league_injuries() works", {
  testthat::skip_on_cran()

  laliga_injuries <- tm_league_injuries(country_name = "Spain")
  expect_type(laliga_injuries, "list")
  expect_equal(ncol(laliga_injuries), 14)
  expect_false(nrow(laliga_injuries) == 0)


  league_one_injuries <- tm_league_injuries(country_name = "",
                                               league_url = "https://www.transfermarkt.com/league-one/startseite/wettbewerb/GB3")
  expect_type(league_one_injuries, "list")
  expect_equal(ncol(league_one_injuries), 14)
  expect_false(nrow(league_one_injuries) == 0)

})


test_that("tm_player_injury_history() works", {
  testthat::skip_on_cran()

  hazard_injuries <- tm_player_injury_history(player_urls = "https://www.transfermarkt.com/eden-hazard/profil/spieler/50202")
  expect_type(hazard_injuries, "list")
  expect_equal(ncol(hazard_injuries), 9)
  expect_false(nrow(hazard_injuries) == 0)

})


test_that("tm_get_player_absence() works", {
  testthat::skip_on_cran()

  player_absence <- tm_get_player_absence(player_urls = c("https://www.transfermarkt.com/cristian-romero/profil/spieler/355915",
                                                          "https://www.transfermarkt.com/micky-van-de-ven/profil/spieler/557459"))
  expect_type(player_absence, "list")
  expect_equal(ncol(player_absence), 10)
  expect_false(nrow(player_absence) == 0)

})


test_that("tm_get_suspensions() works", {
  testthat::skip_on_cran()

  player_suspensions <- tm_get_suspensions(league_url = "https://www.transfermarkt.com/jupiler-pro-league/sperrenausfaelle/wettbewerb/BE1")
  expect_type(player_suspensions, "list")
  expect_equal(ncol(player_suspensions), 10)
  expect_false(nrow(player_suspensions) == 0)
})


test_that("tm_get_risk_of_suspensions() works", {
  testthat::skip_on_cran()

  player_stats <- tm_get_risk_of_suspension(league_url = "https://www.transfermarkt.com/jupiler-pro-league/sperrenausfaelle/wettbewerb/BE1")
  expect_type(player_stats, "list")
  expect_equal(ncol(player_stats), 7)
  expect_false(nrow(player_stats) == 0)

})
