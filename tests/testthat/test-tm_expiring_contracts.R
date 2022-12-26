test_that("clean_up_values_columns() works", {
  testthat::skip_on_cran()
  data <- tibble::tibble(
    player_name = c(1,2),
    player_url = c(2,3),
      position = c(TRUE, FALSE),
      nationality = c(2.1, 3.3),
      second_nationality = c(5,6),
      current_club = c(6, 7),
      contract_expiry = c("2021-12-20", "2022/12/26"),
      contract_option = c(7, 8),
      player_market_value = c("3m", "4th."),
      transfer_fee = c("3m", "4th."),
      agent = c(123, 345))
  cleaned_data <- clean_up_values_columns(data)
  expect_equal(typeof(cleaned_data$player_name), "character")
})
