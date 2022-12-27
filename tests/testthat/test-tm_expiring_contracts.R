test_that("clean_up_values_columns() works", {
  testthat::skip_on_cran()
  data <- tibble::tibble(
    player_name = c(1, 2),
    player_url = c(2, 3),
    position = c(TRUE, FALSE),
    nationality = c(2.1, 3.3),
    second_nationality = c(5, 6),
    current_club = c(6, 7),
    contract_expiry = c("2021-12-20", "2022/12/26"),
    contract_option = c(7, 8),
    player_market_value = c("3m", "4th."),
    transfer_fee = c("3m", "4th."),
    agent = c(123, 345)
  )
  cleaned_data <- clean_up_values_columns(data)
  expect_equal(typeof(cleaned_data$player_name), "character")
  expect_equal(typeof(cleaned_data$player_url), "character")
  expect_equal(typeof(cleaned_data$position), "character")
  expect_equal(typeof(cleaned_data$nationality), "character")
  expect_equal(typeof(cleaned_data$second_nationality), "character")
  expect_equal(typeof(cleaned_data$current_club), "character")
  expect_equal(cleaned_data$contract_expiry[1], lubridate::ymd(20211220))
  expect_equal(typeof(cleaned_data$contract_option), "character")
  expect_equal(typeof(cleaned_data$player_market_value), "double")
  expect_equal(typeof(cleaned_data$transfer_fee), "double")
  expect_equal(typeof(cleaned_data$agent), "character")
})
