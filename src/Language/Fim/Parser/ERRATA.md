## Errata

For the most part I have tried to stay faithful to the [FiM++ 1.0 (Sparkle) spec][spec].
However, in some cases the spec as written was ambiguous, impossible to implement on down right contradictory. Divergences are as follows:

* The syntax for Prompts was `I asked <variable
  name><whitespace><value><punctuation>`. However, because values can simply be
  variables, there is no way to unambiguously seperate `I asked Fluttershy her
  age` into `I asked Fluttershy (her age)` vs `I asked (Fluttershy her) age`. To
  prevent this, I have changed the syntax to
  `I asked <variable name><colon><whitespace><value><punctuation>`

[spec]: https://docs.google.com/document/d/1gU-ZROmZu0Xitw_pfC1ktCDvJH5rM85TxxQf5pg_xmg/edit?pli=1#
