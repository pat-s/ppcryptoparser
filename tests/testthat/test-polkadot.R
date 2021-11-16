test_that("polkadot API scraping works", {
  res_de <- parse_polkadot("1qEk2g6N1uugFgyvmnsL6P9Conh5nTwL5mj85bm3XHtjc73",
    pp_lang = "DE",
    dec = "."
  )
  checkmate::check_data_frame(res_de, any.missing = FALSE)
  testthat::expect_named(res_de, c(
    "Datum", "Stueck", "Typ", "Buchungswaehrung", "Wertpapiername", "Wechselkurs", "Wert"
  ))

  res_en <- parse_polkadot("1qEk2g6N1uugFgyvmnsL6P9Conh5nTwL5mj85bm3XHtjc73",
    dec = "."
  )
  checkmate::check_data_frame(res_en, any.missing = FALSE)
  testthat::expect_named(res_de, c(
    "Datum", "Stueck", "Typ", "Buchungswaehrung", "Wertpapiername", "Wechselkurs", "Wert"
  ))
})
