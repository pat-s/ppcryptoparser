% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/solana.R
\name{parse_solana}
\alias{parse_solana}
\title{Parse Solana Staking Rewards}
\usage{
parse_solana(
  address,
  pp_security_name = "Solana",
  currency = "EUR",
  pp_lang = "EN",
  securities_account = NULL,
  api_key,
  dec = NULL,
  sep = ";",
  filename = NULL,
  by_day = TRUE
)
}
\arguments{
\item{address}{\verb{[character]}\cr
Wallet address.}

\item{pp_security_name}{\link{character}\cr
The name of the security in Portfolio Performance (e.g. "Kusama").}

\item{currency}{\link{character}\cr
Currency in which to calculate the staking rewards.
Must be part of a valid coin pair on Binance, e.g. "DOTEUR".}

\item{pp_lang}{\link{character}\cr
The currency of Portfolio Performance. Only German ("DE") and English
("EN") are supported.}

\item{securities_account}{\link{character}\cr
The name of the account to which to associate the deliveries.
In German called "Depot".}

\item{api_key}{\link{character}\cr
Optional personal API key.}

\item{dec}{\link{character} \cr
Decimal separator. If not set, inferred by the value of argument \code{pp_lang}:
\itemize{
\item If \code{pp_lang = "DE"} -> \code{dec = ","}
\item If \code{pp_lang = "EN"} -> \code{dec = "."}
}}

\item{sep}{\link{character} \cr
Columns separator. Defaults to \verb{;}.}

\item{filename}{\link{character}\cr
The output filename.}

\item{by_day}{\link{logical}\cr
Whether to aggregate staking rewards from different accounts by day.
If set to \code{FALSE}, all individual rewards are returned.}
}
\value{
\link[tibble:tibble]{tibble::tibble} (invisibly)
}
\description{
Retrieves staking rewards for a specific account from \url{https://solanabeach.io}.
}
\examples{
# single address
parse_solana("AsbXE1vn9Z2uZftkQKDMg9cx3ZaKGExxzfwe2u8uj9ZZ",
  api_key = Sys.getenv("solanabeach_api_key"), dec = "."
)
# multiple addresses
parse_solana(c(
  "AsbXE1vn9Z2uZftkQKDMg9cx3ZaKGExxzfwe2u8uj9ZZ",
  "HUKZz7MK9dMGis2AC8trhSME3WFRSivVMfVDypkkNWJR"
),
pp_lang = "EN", dec = ".",
api_key = Sys.getenv("solanabeach_api_key")
)
}
