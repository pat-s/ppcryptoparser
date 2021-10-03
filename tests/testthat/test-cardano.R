test_that("parse_cardano works", {
  data("cardano")

  res_de <- parse_cardano(cardano, pp_lang = "DE")
  checkmate::check_data_frame(res, any.missing = FALSE)
  testthat::expect_named(res_de, c(
    "Datum", "Stück", "Wechselkurs", "Buchungswährung", "Wert",
    "Wertpapiername", "Typ"
  ))

  res_en <- parse_cardano(cardano, pp_lang = "EN")
  checkmate::check_data_frame(res_en, any.missing = FALSE)
  testthat::expect_named(res_en, c(
    "Date", "Shares", "Exchange Rate", "Transaction Currency",
    "Value", "Security Name", "Type"
  ))
})
