## Errata

For the most part I have tried to stay faithful to the [FiM++ 1.0 (Sparkle) spec][spec].
However, in some cases the spec as written was ambiguous, impossible to implement on down right contradictory. Divergences are as follows:

* The syntax for Prompts was `I asked <variable
  name><whitespace><value><punctuation>`. However, because values can simply be
  variables, there is no way to unambiguously seperate `I asked Fluttershy her
  age` into `I asked Fluttershy (her age)` vs `I asked (Fluttershy her) age`. To
  prevent this, I have changed the syntax to `I asked <variable
  name><colon><whitespace><value><punctuation>`
* The spec says variable declarations can either be typed literals or untyped
  variables. I extended the latter case to also include method calls, which
  function very similar to variables in the trivial case.
* The spec says variables cannot be declared with the same names as methods, but
  I have instead opted for a Ruby-esque semi-shared namespace (Methods take
  precedence over variables), but do not overwrite them
* To disambiguate arrays, numbers are not allowed at the _end_ of variable
  names.

[spec]: https://docs.google.com/document/d/1gU-ZROmZu0Xitw_pfC1ktCDvJH5rM85TxxQf5pg_xmg/edit?pli=1#
