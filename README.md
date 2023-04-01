# Introduction
How much representation should we expected in a given political body, given the composition of the population and the size of that body? 

**Characterizing Expected Representation:** This package provides one function, `ExpectedRepresentation`, that calculates the degree of representation under random sampling, where, by default, representation is calculated using the Rose Index of Proportionality. 

**Characterizing Unexplained Representation:** The package contains another function, `ResidualRepresentation`, that calculates the degree of representation we would expect not to be explained by the average discrepancy value; this quantity is the variance of the Rose Index of Proportionality under the random sampling model.

# Downloading 
You may download via the `devtools` package. In particular, use 

```
devtools::install_github(repo = "cjerzak/DescriptiveRepresentationCalculator-software/DescriptiveRepresentationCalculator")
```

Then, to load the software in, use 
```
library(   DescriptiveRepresentationCalculator  ) 
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

# An Example Application 
Now, let's do a more complex application. Let's compare the observed representation in a hypothetical supreme court with the expected representation index we would see under random sampling. 
```
####################
# Generate synthetic body with group types A and B
####################
MemberIdentitiesInSupremeCourt <- c("A","B","A","A","A","A","A","A","A")
bodyN <- length( MemberIdentitiesInSupremeCourt ) 

####################
# Population proportions of A and B (e.g., from Census data)
####################
PopulationProportions <- c("A"=0.8,
                           "B"=0.2)

####################
# Compute the observed representation index
####################
BodyProportions <- prop.table(table( MemberIdentitiesInSupremeCourt) )
ObservedIndex <- 1-0.5*sum(abs(PopulationProportions-BodyProportions))
print(ObservedIndex) # 0.91

####################
# Compute expected representation index
####################
ExpectedRep <- ExpectedRepresentation(PopShares = PopulationProportions,
                                      BodyN = bodyN)
                                      ExpectedRep
print( ExpectedRep ) # 0.89

# The hypothetical body is actually somewhat *more* representative
# than would be expected under random sampling

####################
# variance analysis
####################
SDRep <- ResidualRepresentation(PopShares = PopulationProportions,
                                      BodyN = bodyN)
ExpectedRep_CI <- c(ExpectedRep-1.96*SDRep, 
                    ExpectedRep + 1.96*SDRep)
# print(ExpectedRep_CI) -> 0.736855 1.048397

# Conclusion: 
# Observed representation is well-within 
# confidence intervals under the random sampling model
# (i.e., the representation index so observed could have
# plausibly been generated under random sampling)

```

# Suggested Applications
There are several ways in which this package could be helpful: 
- *Quantifying descriptive representation:* If you want to explain what determines descriptive representation outcomes, you can use the package to compute relative representation---i.e., the level of representation relative to what we would expect based on random sampling alone. We'll add a function which quantifies this in a future release, but can be calculated using the quantities output from this package now. 
- *Control variable:* In other situations, researchers may want to use the expected degree of representation under the random sampling model in order to control for compositional factors such as body size and population composition. This expected representation measure is a direct quantification of how those factors should, under the random sampling model, affect representation and therefore is a useful control variable. 

# Feature Requests & Future Development Plan
Don't hesitate to reach out to Connor Jerzak at `connor.jerzak@gmail.com` for package support or feature requests.

In future releases, we will allow users to compute expected and residual representation under different institutional arrangements. We will also release country-level descriptive representation data. 

# References
John Gerring, Connor T. Jerzak, Erzen Öncel. "The Composition of Descriptive Representation." *SocArXiv Preprint*, 2023. [`osf.io/preprints/socarxiv/9hqnp`](https://osf.io/preprints/socarxiv/9hqnp)
