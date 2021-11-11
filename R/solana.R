#' Parse Solana Staking Rewards
#'
#' @description
#' Retrieves staking rewards for a specific account from \url{https://solanabeach.io}.
#'
#' @template param_address
#' @template param_pp_security_name
#' @param currency `[character]`\cr
#'   Currency in which to calculate the staking rewards.
#'   Must be part of a valid coin pair on Binance, e.g. "DOTEUR".
#' @template param_pp_lang
#' @template param_securities_account
#' @template param_filename
#' @template param_dec
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
#' @importFrom purrr flatten
#' @import cli
#'
#' @examples
#' parse_solana("AsbXE1vn9Z2uZftkQKDMg9cx3ZaKGExxzfwe2u8uj9ZZ", filename = "~/Downloads/solana.csv", pp_lang = "EN", securities_account = "Ledger", api_key = Sys.getenv("solanabeach_api_key"))
parse_solana <- function(address, pp_security_name = "Solana",
                         currency = "EUR", pp_lang = "EN",
                         securities_account = NULL, api_key,
                         dec = NULL, filename = NULL) {
  dec <- helper_dec(dec, pp_lang)

  resp_tbl_prices <- workhorse_sol(
    address, pp_security_name, currency,
    pp_lang, securities_account, api_key, dec,
    filename
  )
  return(invisible(resp_tbl_prices))
}

workhorse_sol <- function(address, pp_security_name, currency = "EUR", pp_lang = "EN",
                          securities_account, api_key,
                          dec = NULL, filename = NULL) {
  resp_list <- list()

  resp <- request_fun_sol(address, api_key = api_key)
  resp_body <- resp %>%
    resp_body_json()
  resp_list[[1]] <- resp_body
  while_n <- 2

  while (length(resp_body) == 5) {
    # get lowest epoch value
    new_epoch_cursor <- resp_body[[length(resp_body)]]$epoch - 1
    resp <- request_fun_sol(address, cursor = new_epoch_cursor, api_key)
    resp_body <- resp %>%
      resp_body_json()
    resp_list[[while_n]] <- resp_body
    while_n <- while_n + 1
  }

  data_mod <- data.table::rbindlist(purrr::flatten(resp_list)) %>%
    select(.data$amount, .data$timestamp) %>%
    mutate(amount = .data$amount / 1000000000) %>%
    mutate(Date = as.Date(as.POSIXct(.data$timestamp,
      origin = "1970-01-01", tz = "GMT"
    )))


  timestamp_date_unix <- data_mod %>%
    pull(.data$timestamp)
  timestamp_date <- as.Date(as.POSIXct(as.numeric(timestamp_date_unix),
    origin = "1970-01-01", tz = "GMT"
  ))
  price_tbl <- binance_klines(min(timestamp_date_unix),
    symbol = sprintf("SOL%s", currency)
  )
  price_tbl_close <- price_tbl %>%
    mutate(Timestamp = as.Date(as.POSIXct(.data$Timestamp / 1e3,
      origin = "1970-01-01", tz = "GMT"
    ))) %>%
    select(.data$Timestamp, .data$Close) %>%
    tibble::as_tibble() %>%
    filter(between(.data$Timestamp, tail(timestamp_date, 1), timestamp_date[1]))


  if (pp_lang == "EN") {
    resp_tibble <- data_mod %>%
      arrange(.data$Date) %>%
      mutate(Type = "Delivery (Inbound)") %>%
      mutate(`Transaction Currency` = currency) %>%
      mutate(`Security Name` = pp_security_name) %>%
      rename(Shares = .data$amount) %>%
      select(-.data$timestamp) %>%
      relocate(.data$Date, .before = .data$Shares)

    resp_tbl_prices <- dplyr::left_join(resp_tibble, price_tbl_close,
      by = c("Date" = "Timestamp")
    ) %>%
      mutate(`Exchange Rate` = as.numeric(.data$Close)) %>%
      select(-.data$Close) %>%
      mutate(Value = .data$Shares * .data$`Exchange Rate`)

    if (!is.null(securities_account)) {
      resp_tbl_prices$`Securities Account` <- securities_account
    }

    if (!is.null(filename)) {
      write_csv_helper(resp_tbl_prices, filename, dec)
    }
  } else if (pp_lang == "DE") {
    resp_tibble <- data_mod %>%
      mutate(Datum = as.Date(as.POSIXct(as.numeric(timestamp_date_unix),
        origin = "1970-01-01", tz = "GMT"
      ))) %>%
      # rename(Datum = .data$Date) %>%
      arrange(.data$Datum) %>%
      mutate(Typ = "Einlieferung") %>%
      mutate(Buchungswaehrung = currency) %>%
      rename(Stueck = .data$amount) %>%
      mutate(Wertpapiername = pp_security_name) %>%
      select(-.data$timestamp, -.data$Date) %>%
      relocate(.data$Datum, .before = .data$Stueck)

    resp_tbl_prices <- dplyr::left_join(resp_tibble, price_tbl_close,
      by = c("Datum" = "Timestamp")
    ) %>%
      mutate(Wechselkurs = as.numeric(.data$Close)) %>%
      select(-.data$Close) %>%
      mutate(Wert = .data$Stueck * .data$Wechselkurs)

    if (!is.null(securities_account)) {
      resp_tbl_prices$`Konto` <- securities_account
    }

    if (!is.null(filename)) {
      write_csv_helper(resp_tbl_prices, filename, dec)
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
request_fun_sol <- function(address, cursor = NULL, api_key) {
  req <- request(sprintf("https://api.solanabeach.io/v1/account/%s/stake-rewards", address)) %>%
    { # nolint
      if (!is.null(api_key)) {
        req_headers(.,
          "Authorization: Bearer" = api_key,
        )
      } else {
        . # nolint
      }
    } %>%
    req_user_agent("ppcryptoparser (http://github.com/pat-s/ppcryptoparser)") %>%
    req_url_query(
      cursor = cursor
    ) %>%
    # req_retry(
    #   after = polka_after,
    #   is_transient = is_transient_polkadot,
    #   max_seconds = 70
    # ) %>%
    req_perform()

  return(req)
}

#' #' @importFrom httr2 resp_header
#' is_transient_solana <- function(resp) {
#'   resp_status(resp) == 429 &&
#'     resp_header(resp, "ratelimit-remaining") == "0"
#' }

# solana_after <- function(resp) {
#   time <- as.numeric(resp_header(resp, "ratelimit-reset")) # nolint
#   time - unclass(Sys.time())
# }
