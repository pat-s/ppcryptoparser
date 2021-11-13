#' Parse Polkadot Staking Rewards
#'
#' @description
#' Retrieves staking rewards for a specific account from \url{https://subscan.io}.
#'
#' @template param_address
#' @template param_pp_security_name
#' @param currency [character]\cr
#'   Currency in which to calculate the staking rewards.
#'   Must be part of a valid coin pair on Binance, e.g. "DOTEUR".
#' @template param_pp_lang
#' @template param_securities_account
#' @template param_filename
#' @template param_dec
#' @template param_sep
#' @template param_api_key
#'
#' @return [tibble::tibble] (invisibly)
#' @export
#' @importFrom mlr3misc map_dtr
#' @importFrom tibblify tibblify
#' @importFrom dplyr relocate arrange pull between ungroup summarise first
#'   group_by
#' @importFrom data.table rbindlist
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @import cli
#'
#' @examples
#' parse_polkadot("1qEk2g6N1uugFgyvmnsL6P9Conh5nTwL5mj85bm3XHtjc73", dec = ".")
#' parse_polkadot("1qEk2g6N1uugFgyvmnsL6P9Conh5nTwL5mj85bm3XHtjc73",
#'   api_key = Sys.getenv("subscan_api_key"), dec = "."
#' )
parse_polkadot <- function(address, pp_security_name = "Polkadot",
                           currency = "EUR", pp_lang = "EN",
                           securities_account = NULL,
                           dec = NULL, sep = ";",
                           filename = NULL, api_key = NULL) {
  dec <- helper_dec(dec, pp_lang)

  chain <- "polkadot"
  resp_tbl_prices <- workhorse(
    address, pp_security_name, currency,
    pp_lang, securities_account, dec, sep,
    filename, api_key, chain
  )
  return(invisible(resp_tbl_prices))
}

workhorse <- function(address, pp_security_name, currency = "EUR", pp_lang = "EN",
                      securities_account = NULL,
                      dec = NULL, sep = ";", filename = NULL, api_key = NULL,
                      chain = "polkadot", by_day = FALSE) {
  resp <- request_fun(address, page = 1, api_key = api_key, chain = chain)
  resp_body <- resp %>%
    resp_body_json()

  # calculate how many rows per page we need to request
  n_rows <- 100
  count <- resp_body$data$count
  n_pages <- ceiling(count / n_rows)

  resp_all <- mlr3misc::map_dtr(0:n_pages, function(x) {
    resp <- request_fun(address, page = x, row = n_rows, chain = chain)
    resp_body <- resp %>%
      resp_body_json()
    tibblify::tibblify(resp_body$data$list)
  })

  timestamp_date_unix <- resp_all %>%
    pull(.data$block_timestamp)
  timestamp_date <- as.Date(as.POSIXct(as.numeric(timestamp_date_unix),
    origin = "1970-01-01", tz = "GMT"
  ))

  if (chain == "polkadot") {
    symb <- "DOT"
  } else if (chain == "kusama") {
    symb <- "KSM"
  }

  if (chain == "polkadot") {
    price_tbl <- binance_klines(min(timestamp_date_unix),
      symbol = sprintf("%s%s", symb, currency)
    )
    price_tbl_close <- price_tbl %>%
      mutate(Timestamp = as.Date(as.POSIXct(.data$Timestamp / 1e3,
        origin = "1970-01-01", tz = "GMT"
      ))) %>%
      select(.data$Timestamp, .data$Close) %>%
      tibble::as_tibble() %>%
      filter(between(.data$Timestamp, tail(timestamp_date, 1), timestamp_date[1]))
  } else if (chain == "kusama") {
    price_tbl <- kraken_klines(min(timestamp_date_unix),
      symbol = sprintf("%s%s", symb, currency)
    )
    price_tbl_close <- price_tbl %>%
      mutate(Timestamp = as.Date(as.POSIXct(.data$Timestamp,
        origin = "1970-01-01", tz = "GMT"
      ))) %>%
      select(.data$Timestamp, .data$Close) %>%
      tibble::as_tibble() %>%
      filter(between(.data$Timestamp, tail(timestamp_date, 1), timestamp_date[1]))
  }

  if (pp_lang == "EN") {
    resp_tibble <- resp_all %>%
      mutate(Date = as.Date(as.POSIXct(as.numeric(.data$block_timestamp),
        origin = "1970-01-01", tz = "GMT"
      ))) %>%
      arrange(.data$Date) %>%
      relocate(.data$Date, .before = .data$account) %>%
      mutate(Type = "Delivery (Inbound)") %>%
      mutate(`Transaction Currency` = currency) %>%
      mutate(`Security Name` = pp_security_name)

    if (chain == "polkadot") {
      resp_tibble$Shares <- as.numeric(resp_tibble$amount) / 10000000000
    } else if (chain == "kusama") {
      resp_tibble$Shares <- as.numeric(resp_tibble$amount) / 1000000000000
    }
    resp_tibble <- resp_tibble %>%
      select(
        .data$Date, .data$Shares, .data$Type, -.data$amount,
        .data$`Transaction Currency`, .data$`Security Name`
      )

    resp_tbl_prices <- dplyr::left_join(resp_tibble, price_tbl_close,
      by = c("Date" = "Timestamp")
    ) %>%
      mutate(`Exchange Rate` = as.numeric(.data$Close)) %>%
      select(-.data$Close) %>%
      mutate(Value = .data$Shares * .data$`Exchange Rate`)

    if (!is.null(securities_account)) {
      resp_tbl_prices$`Securities Account` <- securities_account
    }

    if (by_day) {
      resp_tbl_prices <- resp_tbl_prices %>%
        group_by(.data$Date) %>%
        summarise(
          Shares = sum(.data$Shares), Type = first(.data$Type),
          `Transaction Currency` = first(.data$`Transaction Currency`),
          `Security Name` = first(.data$`Security Name`),
          `Exchange Rate` = first(.data$`Exchange Rate`),
          Value = sum(.data$Value)
        ) %>%
        ungroup()
    }

    if (!is.null(filename)) {
      write_csv_helper(resp_tbl_prices, filename, dec, sep)
    }
  } else if (pp_lang == "DE") {
    resp_tibble <- resp_all %>%
      mutate(Datum = as.Date(as.POSIXct(as.numeric(.data$block_timestamp),
        origin = "1970-01-01", tz = "GMT"
      ))) %>%
      arrange(.data$Datum) %>%
      relocate(.data$Datum, .before = .data$account) %>%
      mutate(Typ = "Einlieferung") %>%
      mutate(Buchungswaehrung = currency) %>%
      mutate(Wertpapiername = pp_security_name)

    if (chain == "polkadot") {
      resp_tibble$Stueck <- as.numeric(resp_tibble$amount) / 10000000000
    } else if (chain == "kusama") {
      resp_tibble$Stueck <- as.numeric(resp_tibble$amount) / 1000000000000
    }

    resp_tibble <- resp_tibble %>%
      select(
        .data$Datum, .data$Stueck, .data$Typ, -.data$amount,
        .data$Buchungswaehrung, .data$Wertpapiername
      )

    resp_tbl_prices <- dplyr::left_join(resp_tibble, price_tbl_close,
      by = c("Datum" = "Timestamp")
    ) %>%
      mutate(Wechselkurs = as.numeric(.data$Close)) %>%
      select(-.data$Close) %>%
      mutate(Wert = .data$Stueck * .data$Wechselkurs)

    if (!is.null(securities_account)) {
      resp_tbl_prices$`Depot` <- securities_account
    }

    if (by_day) {
      resp_tbl_prices <- resp_tbl_prices %>%
        group_by(.data$Datum) %>%
        summarise(
          Stueck = sum(.data$Stueck), Typ = first(.data$Typ),
          Buchungswaehrung = first(.data$Buchungswaehrung),
          Wertpapiername = first(.data$Wertpapiername),
          Wechselkurs = first(.data$Wechselkurs),
          Wert = sum(.data$Wert)
        ) %>%
        ungroup()
    }

    if (!is.null(filename)) {
      write_csv_helper(resp_tbl_prices, filename, dec, sep)
    }
  }
  return(invisible(resp_tbl_prices))
}

# helper -----------------------------------------------------------------------

#' Make API requests to Polkadot network
#'
#' @template param_address
#' @param page `[integer]`\cr
#'   The page to query.
#' @param row `[integer]`\cr
#'   How many rows to query per page.
#' @template param_api_key
#'
#' @importFrom httr2 req_headers req_user_agent req_retry resp_status resp_header
#' @return httr2 response
#' @keywords internal
#' @export
request_fun <- function(address, page, row = 1, api_key = NULL, chain = "polkadot") {
  resp <- request(sprintf("https://%s.api.subscan.io/api/scan/account/reward_slash", chain)) %>%
    { # nolint
      if (!is.null(api_key)) {
        req_headers(.,
          "X-API-KEY" = api_key
        )
      } else {
        . # nolint
      }
    } %>%
    req_user_agent("ppcryptoparser (http://github.com/pat-s/ppcryptoparser)") %>%
    req_body_json(list(
      address = address,
      page = page,
      row = row
    )) %>%
    req_retry(
      after = polka_after,
      is_transient = is_transient_polkadot,
      max_seconds = 70
    ) %>%
    req_perform()

  return(resp)
}

#' @importFrom httr2 resp_header
is_transient_polkadot <- function(resp) {
  resp_status(resp) == 429 &&
    resp_header(resp, "ratelimit-remaining") == "0"
}

polka_after <- function(resp) {
  time <- as.numeric(resp_header(resp, "ratelimit-reset")) # nolint
  time - unclass(Sys.time())
}
