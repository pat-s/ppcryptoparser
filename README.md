
# ppcryptoparser

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/ppcryptoparser)](https://CRAN.R-project.org/package=ppcryptoparser)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/pat-s/ppcryptoparser/workflows/R-CMD-check/badge.svg)](https://github.com/pat-s/ppcryptoparser/actions)
<a href="https://www.buymeacoffee.com/patrickschratz" target="_blank"><img src="https://cdn.buymeacoffee.com/buttons/default-orange.png" alt="Buy Me A Coffee" height="20" width="85"></a>
<!-- badges: end -->

The goal of {ppcryptoparser} is to simplify the import of staking reward from various altcoins into [Portfolio Performance](https://www.portfolio-performance.info/).

## Installation

You need a working installation of [R](https://www.r-project.org/).

Windows: `scoop install r` or [download from CRAN](https://cran.r-project.org/).

macOS: `brew install --cask r` or [download from CRAN](https://cran.r-project.org/).

Ubuntu: `apt install r-base`

You can then install `ppcryptoparser` from GitHub with:

``` r
install.packages("remotes")
remotes::install_github("pat-s/ppcryptoparser")
```

## Example

Spin up an R session by calling "R" in a terminal and then run

```r
library("ppcryptoparser")

parse_polkadot(<address>, <csv output file>)
```

Please also consult the help files for each function, either on the command line via `?<function name>` or by looking at the [pkgdown](https://pat-s.github.io/ppcryptoparser) page of this package.

## Supported Coins

- Cardano (ADA)
- Polkadot (DOT)
- Kusama (KSM)
- Solana (SOL) - [API Key needed!](#solana)

## Planned Support

- Terra (LUNA)
- Solana (SOL)
- Polygon (MATIC)

## Language & Currency

The default language is set to (US) English (`"EN"`).
The language setting should match the language used in Portfolio Performance.

## Encoding & Windows

I've seen that on Windows machines, the encoding might be set to something else than "UTF-8", causing issues in the processing.

Also Windows seems to set the decimal separator to `.` instead of `,`, which causes a wrong import of the data.
In this case, edit the resulting `.csv` file and change the decimal separators from `,` to `.` and check whether the import is working as intended.
I might add an argument to the functions to account for this within the R package.

## CSV Import

When importing, ensure to choose the type "Depotumsätze" / "Portfolio Transactions":

![Screenshot showing how to import CSV](man/figures/readme-1.png)

## Coin-specific Infos

### Kusama

Kusama pays out rewards every six hours.
`parse_kusama()` comes with an argument `"by_day" which aggregates rewards by day.

### Solana

Solana data is queried from https://solanabeach.io which requires an API key.
Instructions how to ask for an API key can be found [on their GitHub README](https://github.com/solana-beach/api).

Solana staking account cannot be topped up, hence often more than one staking account exists.
`parse_solana()` is able to account for this by merging the rewards from multiple addresses.
To do so, one needs to pass the addresses as a vector like this 

```r
parse_solana(c("<address1>", "<address2>"), by_day = TRUE)
```
