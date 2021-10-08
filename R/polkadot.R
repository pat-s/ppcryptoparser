#' Parse Cardano Staking Rewards
#'
#' @description
#' Retrieves staking rewards for a specific account from \url{https://subscan.io}.
#'
#' @return
#' @export
#' @importFrom mlr3misc map_dtr
#' @importFrom tibblify tibblify
#' @importFrom dplyr relocate arrange pull between
#' @importFrom data.table rbindlist
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#'
#' @examples
#' parse_polkadot("1qEk2g6N1uugFgyvmnsL6P9Conh5nTwL5mj85bm3XHtjc73")
parse_polkadot <- function(address, currency = "EUR", pp_lang = "EN", filename = "polkadot.csv") {
  resp <- request_fun(address, page = 1)
  n_pages <- ceiling(resp$data$count / 30)

  resp_list <- mlr3misc::map_dtr(seq_len(n_pages), function(x) {
    tibblify::tibblify(request_fun(address, page = x)$data$list)
  })

  timestamp_date_unix <- resp_list %>%
    pull(block_timestamp)
  timestamp_date <- as.Date(as.POSIXct(as.numeric(timestamp_date_unix),
    origin = "1970-01-01", tz = "GMT"
  ))

  price_tbl <- binance_klines(min(timestamp_date_unix))

  price_tbl_close <- price_tbl %>%
    mutate(Timestamp = as.Date(as.POSIXct(Timestamp / 1e3,
      origin = "1970-01-01", tz = "GMT"
    ))) %>%
    select(Timestamp, Close) %>%
    tibble::as_tibble() %>%
    filter(between(Timestamp, tail(timestamp_date, 1), timestamp_date[1]))


  if (pp_lang == "EN") {
    resp_tibble <- resp_list %>%
      mutate(Date = as.Date(as.POSIXct(as.numeric(block_timestamp),
        origin = "1970-01-01", tz = "GMT"
      ))) %>%
      arrange(Date) %>%
      # DOT: 1/10000000000
      # KSM: 1/1000000000000
      mutate(Shares = as.numeric(amount) / 10000000000) %>%
      relocate(Date, .before = account) %>%
      mutate(Type = "Delivery (Inbound)") %>%
      select(Date, Shares, Type) %>%
      mutate(`Transaction Currency` = currency) %>%
      mutate(`Security Name` = "Polkadot")

    resp_tbl_prices <- dplyr::left_join(resp_tibble, price_tbl_close, by = c("Date" = "Timestamp")) %>%
      mutate(Value = Close)

    readr::write_csv(resp_tbl_prices, filename)
  } else if (pp_lang == "DE") {
    resp_tibble <- resp_list %>%
      mutate(Date = as.POSIXct(as.numeric(block_timestamp),
        origin = "1970-01-01", tz = "GMT"
      )) %>%
      arrange(Date) %>%
      # DOT: 1/10000000000
      # KSM: 1/1000000000000
      mutate(Stück = as.numeric(amount) / 10000000000) %>%
      relocate(Datum, .before = account) %>%
      mutate(Typ = "Einlieferung") %>%
      select(Datum, Stück, Typ) %>%
      mutate(Buchungswährung = currency) %>%
      mutate(Wertpapiername = "Polkadot")

    resp_tbl_prices <- dplyr::left_join(resp_tibble, price_tbl_close, by = c("Datum" = "Timestamp")) %>%
      mutate(Wert = Close)

    readr::write_csv2(resp_tbl_prices, filename)
  }

  return(invisible(resp_tbl_prices))
}

#' Title
#'
#' @param address
#' @param page
#'
#' @return
#' @export
#'
#' @examples
request_fun <- function(address, page) {
  req <- request("https://polkadot.api.subscan.io/api/scan/account/reward_slash") %>%
    req_body_json(list(
      address = address,
      page = page,
      row = 30
    )) %>%
    req_perform() %>%
    resp_body_json()
}


#' Get Binance Klines API data
#'
#' @description
#' The time is returned in UNIX time format (milliseconds). To convert it to
#' a date format, do
#'
#' ```r
#' as.Date(as.POSIXct(<time> / 1e3 ,origin = "1970-01-01", tz = "GMT"))
#' ```
#'
#' @param start_time `[character]`\cr
#'   Start time in UNIX date format with `origin = "1970-01-01"`.
#' @param interval `[character]`\cr
#'   The interval to get the data for. Defaults to one day.
#'   Other possible values are '1m', '3m', '5m', '15m', '30m', '1h', '2h',
#'    '4h', '6h', '8h', '12h', '1d', '3d', '1w', '1M'.
#' @param symbol `[character]`\cr
#'   The currency pair.
#'
#' @return [tibble::tibble]
#' @keywords internal
#' @importFrom httr2 request req_body_json req_perform resp_body_json req_url_query req_headers
#' @export
#'
#' @examples
#' # 2021-06-24
#' binance_klines("1630808472")
binance_klines <- function(start_time, interval = "1d", symbol = "DOTEUR") {
  price_tbl <- request("https://api.binance.com/api/v3/klines") %>%
    req_headers(accept = "application/json") %>%
    req_url_query(
      interval = interval,
      symbol = symbol,
      startTime = start_time
    ) %>%
    req_perform() %>%
    resp_body_json() %>%
    data.table::rbindlist() %>%
    tibble::as_tibble()

  colnames(price_tbl) <- c(
    "Timestamp", "Open", "High", "Low", "Close", "Volume",
    "Close Time", "Quote asset volume", "Number of trades", "Taker buy base asset volume",
    "Taker buy quote asset volume", "Ignore"
  )

  return(price_tbl)
}
