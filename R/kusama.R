#' Parse Kusama Staking Rewards
#'
#' @description
#' Retrieves staking rewards for a specific account from \url{https://subscan.io}.
#'
#' Note that by default staking rewards are aggregated by day. Usually, Kusama
#' pays out rewards up to four times per day. If you want to receive all
#' individual staking rewards, set `by_day = FALSE`.
#'
#' A more coarse aggregation (weekly, monthly) would result in a biased "Value"
#' column as one would need to take the mean of the selected period whereas the
#' reward payouts actually happened on a daily base with a specific coin rate,
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
#' @param by_day [logical]\cr
#'   Whether to aggregate by day. Kusama staking rewards are paid up to four
#'   times per day.
#'   If set to `FALSE`, all individual rewards are returned.
#' @return [tibble::tibble] (invisibly)
#' @export
#' @importFrom httr2 request req_body_json req_perform resp_body_json
#' @importFrom mlr3misc map_dtr
#' @importFrom tibblify tibblify
#' @importFrom dplyr relocate arrange filter
#' @importFrom magrittr %>%
#' @importFrom utils tail
#' @importFrom rlang .data
#'
#' @examples
#' parse_kusama("E4ZoURFwN9eHPxwwDaijY6zXnTvZ3AwFFQFsnSekRTW2sPQ", dec = ".")
parse_kusama <- function(address, pp_security_name = "Kusama", currency = "EUR",
                         pp_lang = "EN", securities_account = NULL, dec = NULL,
                         sep = ";", filename = NULL,
                         api_key = NULL, by_day = TRUE) {
  dec <- helper_dec(dec, pp_lang)

  chain <- "kusama"
  resp_tbl_prices <- workhorse(
    address, pp_security_name, currency,
    pp_lang, securities_account, dec, sep,
    filename, api_key, chain, by_day
  )
  return(invisible(resp_tbl_prices))
}
