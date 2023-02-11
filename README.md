# Introduction
How much representation should we expected in a given political body, given the composition of the population and the size of that body? 

**Characterizing Expected Representation:** This package provides one function, `ExpectedRepresentation`, that calculates the degree of representation under random sampling, where representation is calculated using the Rose Index of Proportionality. 

**Characterizing Unexplained Representation:** The package contains another function, `ResidualRepresentation`, that calculates the degree of representation we would expect not to be explained by the average discrepancy value; this quantity is the variance of the Rose Index of Proportionality under the random sampling model.

# Downloading 
You may download via the `devtools` package. In particular, use 

```
devtools::install_github(repo = "cjerzak/ExpectedRepresentationCalculator/ExpectedRepresentation")
```

Then, to load the software in, use 
```
library(   ExpectedRepresentation  ) 
```

# Example Use
```
##################
# Compute the expected amount of representation: 
# - for a population of group proportions  (1/3, 2/3, 1/3) 
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

# Getting Support or Feature Requests
Don't hesitate to reach out to Connor Jerzak at `connor.jerzak@gmail.com` for package support or feature requests.

# References
John Gerring, Connor T. Jerzak, Erzen Öncel. "The Composition of Descriptive Representation." *SocArXiv Preprint*, 2023. [`osf.io/preprints/socarxiv/9hqnp`](https://osf.io/preprints/socarxiv/9hqnp)
