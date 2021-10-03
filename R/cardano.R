#' Parse Cardano Staking Rewards
#'
#' @description
#' Reads generic CSV outputs from pooltool.io and transposes these into a format
#' which can be read by Portfolio Performance.
#'
#' Rewards are classified as "Delivery (Inbound)" (German: "Einlieferung") which
#' is a category that is different from "buy" and "interest".
#'
#' @param data [character]\cr
#'   The (generic) input CSV from pooltool.io.
#' @param pp_lang [character]\cr
#'   The currency of Portfolio Performance. Only German ("DE") and English
#'   ("EN") are supported.
#' @param filename [character]\cr
#'   The output filename.
#' @param currency
#'
#' @importFrom readr read_csv write_csv write_csv2 col_double col_datetime
#'   col_character cols
#' @importFrom dplyr rename mutate select
#'
#' @return [data.frame] (Invisible)
#' @export
#'
#' @examples
#' data("cardano")
#' parse_cardano(cardano, pp_lang = "DE")
#' parse_cardano(cardano, pp_lang = "EN")
parse_cardano <- function(data, pp_lang = "DE", filename = "cardano.csv") {

  if (!inherits(data, "data.frame")) {
    data <- readr::read_csv(data, col_types = cols(
      date = col_datetime(format = ""),
      epoch = col_double(),
      stake = col_double(),
      pool = col_character(),
      operator_rewards = col_double(),
      stake_rewards = col_double(),
      total_rewards = col_double(),
      rate = col_double(),
      currency = col_character(),
      operator_rewards_value = col_double(),
      stake_rewards_value = col_double(),
      value = col_double()
    ), progress = FALSE)
  }

  if (pp_lang == "DE") {

    # TODO: split time into its own col
    data_mod <- data %>%
      rename(
        Datum = date, Stück = stake_rewards, Wert = stake_rewards_value, Wechselkurs = rate,
        Buchungswährung = currency
      ) %>%
      mutate(Wertpapiername = "Cardano") %>%
      mutate(Typ = "Einlieferung") %>%
      select(-operator_rewards, -pool, -epoch, -stake, -operator_rewards_value, -value, -total_rewards)

    readr::write_csv2(data_mod, filename)
  } else if (pp_lang == "EN") {
    data_mod <- data %>%
      rename(Date = date, Shares = stake_rewards, Value = stake_rewards_value, `Exchange Rate` = rate, `Transaction Currency` = currency) %>%
      mutate(`Security Name` = "Cardano") %>%
      mutate(Type = "Delivery (Inbound)") %>%
      select(-operator_rewards, -pool, -epoch, -stake, -operator_rewards_value, -value, -total_rewards)

    readr::write_csv(data_mod, filename)
  }
  return(invisible(data_mod))
}
