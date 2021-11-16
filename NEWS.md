<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

# ppcryptoparser 0.2.1

- Fixed Solana and Cardano functions


# ppcryptoparser 0.2.0

- Support for Solana via `parse_solana()`. Note: API key required, see README for instructions.
- New argument `sep` to all functions allowing to set any column separator for the CSV output.
- New argument `securities_account` to specify the name of the securities account in Portfolio Performance to which the rewards should be mapped to.


# ppcryptoparser 0.1.0

- The separator for CSV outputs is now fixed to "semicolon" as this seems to be the global PP default across all languages.
- New argument `dec` to allow setting a custom decimal separator. This is helpful if the decimal separator does not follow the PP language setting.
- Allow setting `pp_security_name` for `parse_cardano()`.


# ppcryptoparser 0.0.1

- Initial release
