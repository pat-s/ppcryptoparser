test_that("Solana API scraping works", {
  res_de <- parse_solana("AsbXE1vn9Z2uZftkQKDMg9cx3ZaKGExxzfwe2u8uj9ZZ",
    pp_lang = "DE",
    api_key = Sys.getenv("solanabeach_api_key"), dec = ","
  )
  checkmate::check_data_frame(res_de, any.missing = FALSE)
  expect_named(res_de, c(
    "Datum", "Stueck", "Typ", "Buchungswaehrung", "Wertpapiername",
    "Wechselkurs", "Wert"
  ))
  expect_equal(unique(res_de$Wertpapiername), "Solana")

  res_en <- parse_solana("AsbXE1vn9Z2uZftkQKDMg9cx3ZaKGExxzfwe2u8uj9ZZ",
    api_key = Sys.getenv("solanabeach_api_key"), dec = "."
  )
  checkmate::check_data_frame(res_en, any.missing = FALSE)
  testthat::expect_named(res_en, c(
    "Date", "Shares", "Type", "Transaction Currency", "Security Name",
    "Exchange Rate", "Value"
  ))
  expect_equal(unique(res_en[["Security Name"]]), "Solana")
})

test_that("Solana multiple addresses", {
  res_en <- parse_solana(c(
    "AsbXE1vn9Z2uZftkQKDMg9cx3ZaKGExxzfwe2u8uj9ZZ",
    "HUKZz7MK9dMGis2AC8trhSME3WFRSivVMfVDypkkNWJR"
  ),
  pp_lang = "EN", dec = ".", securities_account = "test",
  api_key = Sys.getenv("solanabeach_api_key")
  )

  checkmate::check_data_frame(res_en, any.missing = FALSE)
  testthat::expect_gt(nrow(res_en), 19)
  testthat::expect_named(res_en, c(
    "Date", "Shares", "Type", "Transaction Currency", "Security Name",
    "Exchange Rate", "Value", "Securities Account"
  ))
  expect_equal(unique(res_en[["Security Name"]]), "Solana")
  checkmate::expect_character(as.character(res_en$Date), unique = TRUE)
})
