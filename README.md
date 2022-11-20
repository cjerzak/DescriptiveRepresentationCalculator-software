# Introduction
How much representation should we expected in a given political body, given the composition of the population and the size of that body? 

**Expected Representation:** This package provides one function, `ExpectedRepresentation`, that calculates the degree of representation under random sampling, where representation is calculated using the Rose Index of Proportionality. 

**Characterizing Unexplained Representation:** It contains another function `ResidualRepresentation`, that calculates the degree of representation we would expect not to be explained by the average discrepancy value; this quantity is the variance of the Rose Index of Proportionality under the random sampling model.

# Downloading 
You may download via the `devtools` package. In particular, use `devtools::install_github("ExpectedRepresentationCalculator/ExpectedRepresentation")`

# Example Use
```
library(ExpectedRepresentation)
# Compute the expected amount of representation: 
# - for a population of group proportions  (1/3, 2/3,1/3) 
# - in a political body of size 50
ExpectedRep <- ExpectedRepresentation(PopShares = c(1/3, 2/3,1/3),
                                       BodyN = 50)
print( ExpectedRep )

# Compute the amount of representation left on explained under 
# the random sampling model for the same body and population
ResidualRep <- ResidualRepresentation(PopShares = c(1/3, 2/3,1/3),
                                       BodyN = 50)
print( ResidualRep )
```

# Citation
Gerring, Jerzak and Oncel, 2022+. 


