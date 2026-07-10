# DescriptiveRepresentationCalculator 1.2.0

## New Features

* Added `ExpectedDistrictRepresentation()`, which computes the expected squared-deviation
  representation index for a body elected from single-member districts whose group
  compositions follow a Dirichlet distribution. Supports two seat-allocation rules
  (`"random"` sampling and `"affinity"` largest-group-wins voting) and the large-district
  limit via `nDistricts = Inf`. Two-group affinity results use exact Beta-distribution
  closed forms; more than two groups are handled by Monte Carlo simulation. Based on
  Propositions 1-4 of Gerring, Hicken, Jerzak, Moser, and Oncel (book manuscript,
  "Electoral Rules and Descriptive Representation").

* Added `GroupRepresentation()`, which returns per-group representation diagnostics:
  each group's population share, body share, shortfall (population share minus body
  share), and shortfall as a fraction of the group's population share.

* Added `CompareRepresentation()`, which computes the difference in observed
  representation between two bodies (e.g., the multi-member and single-member tiers
  of a mixed electoral system) measured against the same population shares.

* Added a `metric` argument to `ExpectedRepresentation()`, `ObservedRepresentation()`,
  `SDRepresentation()`, and `RelativeRepresentation()`. The default `"L1"` reproduces
  the previous behavior (Rose Index of Proportionality); `"L2"` uses squared instead of
  absolute deviations, yielding the squared-deviation representation index. For
  `ExpectedRepresentation()`, the `"L2"` expectation is computed with an exact closed
  form.

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
