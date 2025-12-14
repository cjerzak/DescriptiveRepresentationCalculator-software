# DescriptiveRepresentationCalculator 1.1.1

## Bug Fixes

* Fixed undocumented parameter `b` in `RelativeRepresentation()` documentation that caused R CMD check warnings.

* Fixed division by zero error in `RelativeRepresentation()` when `standardize = TRUE` and the standard deviation equals zero. Now returns `NA` with a warning.

* Fixed README example that used invalid `PopShares` values summing to 4/3 instead of 1.

* Fixed vignette formula notation to correctly show `R = b + a * sum(...)` matching the actual implementation.

## New Features
* Added input validation across all core functions:
 - `PopShares` must sum to 1 (within tolerance)
  - `PopShares` values must be non-negative
  - Empty body handling with informative warning
  - Warning when body members don't match any population group

## Documentation

* Added comprehensive documentation for `RelativeRepresentation()` to the package vignette, including examples and interpretation guidance.

* Updated vignette to document all four main package functions consistently.

# DescriptiveRepresentationCalculator 1.1.0

* Initial CRAN release.
