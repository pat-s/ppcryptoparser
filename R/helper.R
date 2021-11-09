#' @keywords internal
#' @export
helper_dec <- function(dec, pp_lang) {
  if (is.null(dec) && pp_lang == "DE") {
    cli::cli_alert_info("Setting decimal separator to {.code ,} because
      {.code pp_lang = 'DE'} is set.
      If this is undesired and/or you want to silence this warning, please
      explicitly set argument {.code dec}.", wrap = TRUE)
    dec <- ","
  } else if (is.null(dec) && pp_lang == "EN") {
    cli::cli_alert_info("Setting decimal separator to {.code .} because
      {.code pp_lang = 'EN'} is set.
      If this is undesired and/or you want to silence this warning, please
      explicitly set argument {.code dec}.", wrap = TRUE)
    dec <- "."
  }
  return(dec)
}

#' @keywords internal
#' @export
write_csv_helper <- function(data, filename, dec) {
  if (dec == ".") {
    write.table(data, filename, dec = dec, row.names = FALSE, sep = ";")
  } else if (dec == ",") {
    write.table(data, filename, dec = dec, row.names = FALSE, sep = ";")
  } else {
    write.table(data, filename, row.names = FALSE, sep = ";")
  }
}
