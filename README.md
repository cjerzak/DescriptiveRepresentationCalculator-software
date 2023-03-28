# Introduction
How much representation should we expected in a given political body, given the composition of the population and the size of that body? 

**Characterizing Expected Representation:** This package provides one function, `ExpectedRepresentation`, that calculates the degree of representation under random sampling, where, by default, representation is calculated using the Rose Index of Proportionality. 

**Characterizing Unexplained Representation:** The package contains another function, `ResidualRepresentation`, that calculates the degree of representation we would expect not to be explained by the average discrepancy value; this quantity is the variance of the Rose Index of Proportionality under the random sampling model.

# Downloading 
You may download via the `devtools` package. In particular, use 

```
devtools::install_github(repo = "cjerzak/ExpectedRepresentationCalculator-software/ExpectedRepresentationCalculator")
```

Then, to load the software in, use 
```
library(   ExpectedRepresentationCalculator  ) 
```

# Example Use
```
##################
# Compute the expected amount of representation
# (i.e. the expected Rose Index of Proportionality:)
# - for a population of group proportions (1/3, 2/3, 1/3) 
# - in a political body of size 50
ExpectedRep <- ExpectedRepresentation(PopShares = c(1/3, 2/3, 1/3),
                                      BodyN = 50)
print( ExpectedRep )

##################
# Compute the amount of representation left on explained under 
# the random sampling model for the same body and population
ResidualRep <- ResidualRepresentation(PopShares = c(1/3, 2/3, 1/3),
                                      BodyN = 50)
print( ResidualRep )
```

# Suggested Applications
There are several ways in which this package could be helpful: 
- *Quantifying descriptive representation:* If you want to explain what determines descriptive representation outcomes, you can use the package to compute relative representation---i.e., the level of representation relative to what we would expect based on random sampling alone. We'll add a function which quantifies this in a future release. 
- *Control variable:* In other situations, researchers may want to use the expected degree of representation under the random sampling model in order to control for compositional factors such as body size and population composition. This expected representation measure is a direct quantification of how those factors should, under the random sampling model, affect representation and therefore is a useful control variable. 

# Feature Requests & Future Development Plan
Don't hesitate to reach out to Connor Jerzak at `connor.jerzak@gmail.com` for package support or feature requests.

In future releases, we will allow users to compute expected and residual representation under different institutional arrangements. We will also release country-level descriptive representation data. 

# References
John Gerring, Connor T. Jerzak, Erzen Öncel. "The Composition of Descriptive Representation." *SocArXiv Preprint*, 2023. [`osf.io/preprints/socarxiv/9hqnp`](https://osf.io/preprints/socarxiv/9hqnp)
