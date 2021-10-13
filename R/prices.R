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
#' @importFrom httr2 request req_body_json req_perform resp_body_json
#'   req_url_query req_headers resp_header
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

kraken_klines <- function(start_time, interval = "1440", symbol = "KSMEUR") {
  price_tbl <- request("https://api.kraken.com/0/public/OHLC") %>%
    req_headers(accept = "application/json") %>%
    req_url_query(
      interval = interval,
      pair = symbol,
      # we deduct one day in unix time format, otherwise we miss the first day
      since = start_time - 86400
    ) %>%
    req_perform() %>%
    resp_body_json()

  price_tbl <- price_tbl[["result"]][[symbol]] %>%
    data.table::rbindlist() %>%
    tibble::as_tibble()

  colnames(price_tbl) <- c(
    "Timestamp", "Open", "High", "Low", "Close", "vwap",
    "Volume", "Count"
  )

  return(price_tbl)
}
