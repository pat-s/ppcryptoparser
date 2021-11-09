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
#'   The (generic) input CSV from \url{https://pooltool.io}.
#' @template param_pp_security_name
#' @template param_pp_lang
#' @template param_dec
#' @template param_filename
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
parse_cardano <- function(data, pp_security_name = "Cardano", pp_lang = "DE",
                          filename = NULL, dec = NULL) {
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

  dec <- helper_dec(dec, pp_lang)

  if (pp_lang == "DE") {

    # TODO: split time into its own col
    data_mod <- data %>%
      rename(
        Datum = .data$date, Stueck = .data$stake_rewards,
        Wert = .data$stake_rewards_value,
        Wechselkurs = .data$rate,
        Buchungswaehrung = .data$currency
      ) %>%
      mutate(Wertpapiername = pp_security_name) %>%
      mutate(Typ = "Einlieferung") %>%
      select(
        -.data$operator_rewards, -.data$pool, -.data$epoch, -.data$stake,
        -.data$operator_rewards_value, -.data$value, -.data$total_rewards
      )
    if (!is.null(filename)) {
      readr::write_csv2(data_mod, filename)
    }
  } else if (pp_lang == "EN") {
    data_mod <- data %>%
      rename(
        Date = .data$date, Shares = .data$stake_rewards, Value = .data$stake_rewards_value,
        `Exchange Rate` = .data$rate, `Transaction Currency` = .data$currency
      ) %>%
      mutate(`Security Name` = pp_security_name) %>%
      mutate(Type = "Delivery (Inbound)") %>%
      select(
        -.data$operator_rewards, -.data$pool, -.data$epoch, -.data$stake,
        -.data$operator_rewards_value, -.data$value, -.data$total_rewards
      )
    if (!is.null(filename)) {
      write_csv_helper(data_mod, filename, dec)
    }
  }
  return(invisible(data_mod))
}
