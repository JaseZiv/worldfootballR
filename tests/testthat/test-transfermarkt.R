context("Testing Transfermarkt functions")

test_that("get_player_market_values() works", {
  testthat::skip_if_offline()
  # testthat::skip_on_cran()
  # test the functions returns the data
  expect_type(get_player_market_values(country_name = "Australia", start_year = 2021), "list")

  # test that multiple countries can be passed to the function
  expect_type(get_player_market_values(country_name = c("Australia", "Croatia"), start_year = 2020), "list")

  # test that an invalid country will error
  expect_error(get_player_market_values(country_name = "Fake Country", start_year = 2020))

})



test_that("player_transfer_history() works", {
  testthat::skip_if_offline()
  # testthat::skip_on_cran()

  transfer_data <- player_transfer_history(c("https://www.transfermarkt.com/cristiano-ronaldo/profil/spieler/8198"))
  # test the functions returns the data
  expect_type(transfer_data, "list")
  expect_true(ncol(transfer_data) == 11)

  # test that an invalid country will error
  expect_error(player_transfer_history("aaa.com.au"))

})


test_that("tm_team_transfers() works", {
  bayern_summer <- tm_team_transfers(team_url = "https://www.transfermarkt.com/fc-bayern-munchen/startseite/verein/27/saison_id/2020", transfer_window = "summer")
  expect_type(bayern_summer, "list")
  bayern_all <- tm_team_transfers(team_url = "https://www.transfermarkt.com/fc-bayern-munchen/startseite/verein/27/saison_id/2020", transfer_window = "summer")
  expect_type(bayern_all, "list")
  # test multiple urls:
  urls <- c("https://www.transfermarkt.com/fc-burnley/startseite/verein/1132/saison_id/2020",
            "https://www.transfermarkt.com/fc-bayern-munchen/startseite/verein/27/saison_id/2020")
  multi <- tm_team_transfers(team_url = urls, transfer_window = "all")

})


test_that("tm_matchday_table() works", {
  expect_type(tm_matchday_table(country_name="England", start_year="2021", matchday=1), "list")
  expect_type(tm_matchday_table(country_name="England", start_year="2021", matchday=c(1:5)), "list")
})


test_that("tm_league_team_urls() works", {
  team_urls <- tm_league_team_urls(country_name = "England", start_year = 2021)
  expect_type(team_urls, "character")
  expect_true(length(team_urls) == 20)
})


test_that("tm_team_player_urls() works", {
  player_urls <- tm_team_player_urls(team_url = "https://www.transfermarkt.com/fc-burnley/startseite/verein/1132/saison_id/2020")
  expect_type(player_urls, "character")
})


test_that("tm_squad_stats() works", {
  bayern <- tm_squad_stats(team_url = "https://www.transfermarkt.com/fc-bayern-munchen/startseite/verein/27/saison_id/2020")
  expect_type(bayern, "list")
})


test_that("tm_player_bio() works", {
  hazard_bio <- tm_player_bio(player_url = "https://www.transfermarkt.com/eden-hazard/profil/spieler/50202")
  expect_type(hazard_bio, "list")
  expect_equal(ncol(hazard_bio), 19)

  burnley_player_urls <- tm_team_player_urls(team_url = "https://www.transfermarkt.com/fc-burnley/startseite/verein/1132/saison_id/2020")
  # then pass all those URLs to the tm_player_bio
  burnley_bios <- tm_player_bio(player_urls = burnley_player_urls[1:3])
  expect_type(burnley_bios, "list")
  expect_equal(ncol(burnley_bios), 21)

})
