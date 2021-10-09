#' Parse Cardano Staking Rewards
#'
#' @description
#' Retrieves staking rewards for a specific account from
#' \url{https://subscan.io}.
#'
#' @return
#' @export
#' @importFrom httr2 request req_body_json req_perform resp_body_json
#' @importFrom mlr3misc map_dtr
#' @importFrom tibblify tibblify
#' @importFrom dplyr relocate arrange
#' @importFrom magrittr %>%
#'
#' @examples
#' parse_kusama("E4ZoURFwN9eHPxwwDaijY6zXnTvZ3AwFFQFsnSekRTW2sPQ")
parse_kusama <- function(address, currency = "EUR", pp_lang = "EN",
                         filename = "kusama.csv") {
  resp <- request_fun(address, page = 1)
  n_pages <- ceiling(resp$data$count / 30)

  resp_list <- mlr3misc::map_dtr(seq_len(n_pages), function(x) {
    tibblify::tibblify(request_fun(address, page = x)$data$list)
  })

  if (pp_lang == "EN") {
    resp_tibble <- resp_list %>%
      mutate(Date = as.POSIXct(as.numeric(block_timestamp),
        origin = "1970-01-01", tz = "GMT"
      )) %>%
      arrange(Date) %>%
      # DOT: 1/10000000000 # nolint
      # KSM: 1/1000000000000 # nolint
      mutate(Shares = as.numeric(amount) / 1000000000000) %>%
      relocate(Date, .before = account) %>%
      mutate(Type = "Delivery (Inbound)") %>%
      select(Date, Shares, Type) %>%
      mutate(`Transaction Currency` = currency) %>%
      mutate(`Security Name` = "kusama")
    readr::write_csv(resp_tibble, filename)
  } else if (pp_lang == "DE") {
    resp_tibble <- resp_list %>%
      mutate(Date = as.POSIXct(as.numeric(block_timestamp),
        origin = "1970-01-01", tz = "GMT"
      )) %>%
      arrange(Date) %>%
      # DOT: 1/10000000000 # nolint
      # KSM: 1/1000000000000 # nolint
      mutate(Stück = as.numeric(amount) / 1000000000000) %>%
      relocate(Datum, .before = account) %>%
      mutate(Typ = "Einlieferung") %>%
      select(Datum, Stück, Typ) %>%
      mutate(Buchungswährung = currency) %>%
      mutate(Wertpapiername = "kusama")

    readr::write_csv2(resp_tibble, filename)
  }

  # join coingecko price info

  return(invisible(resp_tibble))
}
# TODO: add price converter (from coingecko because subscan only has DOT/USD)

# req_price = request("https://kusama.api.subscan.io/api/open/price") %>%
#   req_body_json(list(time = 6769307))
#
# resp_price = req_perform(req_price)
#
# resp_price_tbl = resp_body_json(resp_price)

request_fun <- function(address, page) {
  req <- request("https://kusama.api.subscan.io/api/scan/account/reward_slash") %>%
    req_body_json(list(
      address = address,
      page = page,
      row = 30
    )) %>%
    req_perform() %>%
    resp_body_json()
}
