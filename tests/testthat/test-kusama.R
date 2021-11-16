test_that("kusama API scraping works", {
  res_de <- parse_kusama("E4ZoURFwN9eHPxwwDaijY6zXnTvZ3AwFFQFsnSekRTW2sPQ",
    pp_lang = "DE",
    dec = "."
  )
  checkmate::check_data_frame(res_de, any.missing = FALSE)
  expect_named(res_de, c(
    "Datum", "Stueck", "Typ", "Buchungswaehrung", "Wertpapiername", "Wechselkurs", "Wert"
  ))
  expect_equal(unique(res_de$Wertpapiername), "Kusama")

  res_en <- parse_kusama("E4ZoURFwN9eHPxwwDaijY6zXnTvZ3AwFFQFsnSekRTW2sPQ",
    dec = "."
  )
  checkmate::check_data_frame(res_en, any.missing = FALSE)
  testthat::expect_named(res_en, c(
    "Date", "Shares", "Type", "Transaction Currency", "Security Name", "Exchange Rate", "Value"
  ))
  expect_equal(unique(res_en[["Security Name"]]), "Kusama")
})

test_that("kusama grouping by day works", {
  res_de <- parse_kusama("E4ZoURFwN9eHPxwwDaijY6zXnTvZ3AwFFQFsnSekRTW2sPQ",
    pp_lang = "DE", by_day = FALSE, dec = "."
  )
  expect_false(length(unique(unique(res_de$Datum))) == nrow(res_de))
})
