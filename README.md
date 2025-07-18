# `DescriptiveRepresentationCalculator`: An R package for quantifying observed and expected descriptive representation 

[**What is the DescriptiveRepresentationCalculator?**](#description)
| [**Key Definitions**](#definitions)
| [**Main Package Functions**](#functions)
| [**Installation**](#installation)
| [**Example Use**](#example)
| [**Tutorial**](#tutorial)
| [**Other Package Uses**](#uses)
| [**Data**](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/BIQZNT)
| [**CRAN**](https://cran.r-project.org/package=DescriptiveRepresentationCalculator)
| [**References**](#references)
| [**Documentation**](https://github.com/cjerzak/DescriptiveRepresentationCalculator-software/blob/main/DescriptiveRepresentationCalculator.pdf)

[<img src="https://img.shields.io/badge/Demo-View%20Demo-blue"
alt="Demo
Button">](https://cran.r-project.org/web/packages/DescriptiveRepresentationCalculator/vignettes/MainVignette.html)

[<img src="https://img.shields.io/badge/CRAN-View%20on%20CRAN-green" alt="CRAN Button">](https://cran.r-project.org/package=DescriptiveRepresentationCalculator) [<img src="https://github.com/cjerzak/DescriptiveRepresentationCalculator-software/actions/workflows/tests.yml/badge.svg" alt="Unit tests status">](https://github.com/cjerzak/DescriptiveRepresentationCalculator-software/actions/workflows/tests.yml)

[<img src="https://img.shields.io/badge/Results-from%20Calculator%20for%20156%20countries-blue" alt="Results from the Calculator for 156 countries">](https://globalleadershipproject.net/country-rankings/)

[![Hugging Face Dataset](https://img.shields.io/badge/Hugging%20Face-View%20Dataset-orange?style=flat-square&logo=huggingface&logoColor=white)](https://huggingface.co/datasets/cjerzak/GlobalLeadershipProject_v1)

## What is the DescriptiveRepresentationCalculator?<a id="description"></a>
How much representation *should* we expect in a political body, given the composition of the population and the size of that body? How much variability should we expect to see around mean descriptive representation? How can we quantify observed representation in a political body and compare it to the expected value? This package helps provide answers to these questions based on [Gerring, Jerzak, and Öncel (2023)](https://www.cambridge.org/core/journals/american-political-science-review/article/composition-of-descriptive-representation/7EAEA1CA4C553AB9D76054D1FA9C0840). 

## Key Definitions<a id="definitions"></a>
### Descriptive Representation

*Definition:* The degree to which the identity characteristics present in an organization or political body  align with those of the population they serve.

*Details:* Descriptive representation is often contrasted against substantive representation, which concerns the degree to which the preferences of those in the body align with those of the population. Descriptive representation is thought to be connected with substantive representation in that individuals with a given set of identity characteristics may be better positioned to understand the needs of that population compared to others. Descriptive representation would be low, for example, if a political body contained few woman (since the population contains around 50% women). Representation would be high if the political body contained 50% women. 

### Rose Index of Proportionality

*Definition:* A quantitative measure of descriptive representation.

*Details:* The quantity is calculated by taking the body shares of each social group, finding the absolute difference compared with the population shares, summing, and re-scaling so that the measure is between 0 and 1, with 1 indicating perfect descriptive representation. Perfect descriptive representation implies that the population and body shares are perfectly balanced. 

## Main Package Functions<a id="functions"></a>
**Characterizing Expected Representation:** Among other things, this package provides a function, `ExpectedRepresentation`, that calculates the degree of representation under random sampling, where, by default, representation is calculated using the Rose Index of Proportionality. This quantifies the value of descriptive representation we should expect in a political body, given population and body characteristics. 

**Characterizing Representation Variability:** The package contains another function, `SDRepresentation`, that calculates the degree of representation we would expect not to be explained by the average discrepancy value; this quantity is the standard deviation of the Rose Index of Proportionality under the random sampling model. It captures how much uncertainty we would expect over the expected representation value under the random sampling model.

**Quantifying Relative Representation:** `RelativeRepresentation` compares the observed representation in a body to its expected value under random sampling. It optionally standardizes this difference by the variability captured through `SDRepresentation`.

**Characterizing Observed Representation:** The package contains third function, `ObservedRepresentation`, that computes the Rose Index of Proportionality using observed data. Observed representation index scores can be compared against the expected value of the index under random sampling and also against the variability of observed represenation under that model. 

## Installation<a id="installation"></a>
You may download via the `devtools` package. In particular, use 

```
# install stable package version from CRAN
install.packages("DescriptiveRepresentationCalculator")

# install development version 
#devtools::install_github(repo = "cjerzak/DescriptiveRepresentationCalculator-software/DescriptiveRepresentationCalculator")
```

Then, to load the software in, use 
```
library(   DescriptiveRepresentationCalculator  ) 
```

[<img src="https://github.com/cjerzak/DescriptiveRepresentationCalculator-software/blob/main/misc/figures/PackageViz.png?raw=true" width="450" height="400">](https://www.cambridge.org/core/journals/american-political-science-review/article/composition-of-descriptive-representation/7EAEA1CA4C553AB9D76054D1FA9C0840)

## Example Use<a id="example"></a>
```
##################
# Compute the expected amount of representation
# (i.e. the expected Rose Index of Proportionality:)
# - for a population of group proportions (1/3, 2/3, 1/3) 
# - in a political body of size 50
ExpectedRep <- ExpectedRepresentation(
  PopShares = c(1/3, 2/3, 1/3),
  BodyN = 50)
print( ExpectedRep )

##################
# Compute the amount of representation left unexplained under
# the random sampling model for the same body and population
ResidualRep <- SDRepresentation(
  PopShares = c(1/3, 2/3, 1/3),
  BodyN = 50)
print( ResidualRep )
```

## Tutorial<a id="tutorial"></a>
Now, let's do a more complex application. Let's compare the observed representation in a hypothetical supreme court with the expected representation index we would see under random sampling. 
```
####################
# Generate synthetic body with group types A and B
####################
MemberIdentitiesInSupremeCourt <- c("A","B","A","A",
                    "A","A","A","A","A")
bodyN <- length( MemberIdentitiesInSupremeCourt ) 

####################
# Population proportions of A and B (e.g., from census data)
####################
PopulationProportions <- c("A"=0.8,
                           "B"=0.2)

####################
# Compute the observed representation index
####################
ObservedIndex <- ObservedRepresentation(
  BodyMemberCharacteristics = MemberIdentitiesInSupremeCourt,
  PopShares = PopulationProportions)
print(ObservedIndex) # 0.91

####################
# Compute expected representation index
####################
ExpectedRep <- ExpectedRepresentation(
  PopShares = PopulationProportions,
  BodyN = bodyN)
print( ExpectedRep ) # 0.89

# The hypothetical body is actually somewhat *more* representative
# than would be expected under random sampling

####################
# variability analysis
####################
SDRep <- SDRepresentation(PopShares = 
  PopulationProportions,
  BodyN = bodyN)
ExpectedRep_CI <- c(ExpectedRep-1.96*SDRep, 
                    ExpectedRep + 1.96*SDRep)
print(ExpectedRep_CI) # -> 0.736855 1.048397

# Conclusion: 
# Observed representation is well-within 
# confidence intervals under the random sampling model
# (i.e., the representation index so observed could have
# plausibly been generated under random sampling)
```

## Other Package Uses<a id="uses"></a>
There are several other ways in which this package could be helpful in practice: 
- *Quantifying expected and observed descriptive representation:* If you want to explain what determines descriptive representation outcomes, you can use the package to compute relative representation---i.e., the level of representation relative to what we would expect based on random sampling alone. The `RelativeRepresentation` function computes this quantity directly.
- *Control variable creation:* In other situations, researchers may want to use the expected degree of representation under the random sampling model in order to control for compositional factors such as body size and population composition. This expected representation measure is a direct quantification of how those factors should, under the random sampling model, affect representation and therefore is a useful control variable. 

## Feature Requests & Development Plan
Don't hesitate to reach out to Connor Jerzak at [`connor.jerzak@gmail.com`](mailto:connor.jerzak@gmail.com)
for package support or feature requests.

In future releases, we will allow users to compute expected and residual representation under different institutional arrangements. We will also release country-level descriptive representation data. 

## References<a id="references"></a>
John Gerring, Connor T. Jerzak, Erzen Öncel. "The Composition of Descriptive Representation."  *American Political Science Review*, 118(2): 784-801, 2024. [\[PDF\]](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/7EAEA1CA4C553AB9D76054D1FA9C0840/S0003055423000680a.pdf/the-composition-of-descriptive-representation.pdf) [\[Dataverse\]](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/BIQZNT)
```
@article{GJO-CompDR,
        title={The Composition of Descriptive Representation},
        author={Gerring, John and Connor T. Jerzak and Erzen Öncel},
        journal={American Political Science Review},
        year={2024},
        volume={118},
        number={2},
        pages={784-801}
}
```

## Related Work<a id="related"></a>
 John Gerring, Allen Hicken, Connor T. Jerzak, Robert G. Moser, and Erzen Öncel. "Where Minorities are the Majority: Electoral Rules and Ethnic Representation."  *OSF Preprint*, 2024. [\[PDF\]](https://osf.io/preprints/osf/ums8y)

<!-- 
<table style="width:100%;">
  <tr>
    <td style="width:50%; text-align:center;"><img src="https://i0.wp.com/connorjerzak.com/wp-content/uploads/2023/07/dr.png?w=926&ssl=1" /></td>
    <td style="width:50%; text-align:center;">Figure: <i>Observed vs. expected representation globally. Findings: <br> [a.] Political bodies in every country are less representative than expected under a simple random sampling model. <br> [b.] But the model still explains much variability in representation.</i></td>
  </tr>
</table>

[<img src="https://i0.wp.com/connorjerzak.com/wp-content/uploads/2023/07/dr.png?w=926&ssl=1" width="450" height="400">](https://www.cambridge.org/core/journals/american-political-science-review/article/composition-of-descriptive-representation/7EAEA1CA4C553AB9D76054D1FA9C0840)
-->

[<img src="https://i0.wp.com/connorjerzak.com/wp-content/uploads/2023/07/dr.png?w=926&ssl=1" width="400">](https://www.cambridge.org/core/journals/american-political-science-review/article/composition-of-descriptive-representation/7EAEA1CA4C553AB9D76054D1FA9C0840)

[<img src="https://globalleadershipproject.net/wp-content/uploads/2025/04/cropped-GLPLogo.png" width="400">](https://globalleadershipproject.net)
