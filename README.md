
# ppcryptoparser

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/ppcryptoparser)](https://CRAN.R-project.org/package=ppcryptoparser)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/pat-s/ppcryptoparser/workflows/R-CMD-check/badge.svg)](https://github.com/pat-s/ppcryptoparser/actions)
<!-- badges: end -->

The goal of {ppcryptoparser} is to simplify the import of staking reward CSV files into [Portfolio Performance](https://www.portfolio-performance.info/).

## Installation

You need a working installation of [R](https://www.r-project.org/).

Windows: `scoop install r` or [download from CRAN](https://cran.r-project.org/).

macOS: `brew install --cask r` or [download from CRAN](https://cran.r-project.org/).

Ubuntu: `apt install r-base`

You can then install `ppcryptoparser` from GitHub with:

``` r
remotes::install_github("pat-s/ppcryptoparser")
```

## Example

```r
library(ppcryptoparser)

parse_cardano(<address>, <csv output file>)
```

Please also consult the help files for each function, either on the command line via `?<function name>` or by looking at the [pkgdown](https://pat-s.github.io/ppcryptoparser) page of this package.

## Supported Coins

- Cardano (ADA)
- Polkadot (DOT)
- Kusama (KSM)

## Planned Support

- Terra (LUNA)
- Solana (SOL)
- Polygon (MATIC)

## Language & Currency

The default language is set to (US) English (`"EN"`).
The language setting should match the language used in Portfolio Performance.

## CSV Import

When importing, ensure to choose the type "Depotumsätze" / "Portfolio Transactions":

![Screenshot showing how to import CSV](man/figures/readme-1.png)
