# setup environment
rm(  list = ls()  )
setwd("~/Documents/DescriptiveRepresentationCalculator-software")
package_path <- "~/Documents/DescriptiveRepresentationCalculator-software/DescriptiveRepresentationCalculator"
versionNumber <- "1.0.0"


# build documentation
library(devtools); library(utils)
devtools::document(package_path)
try(file.remove("./DescriptiveRepresentationCalculator.pdf"),T)
devtools::document(package_path)
#devtools::build_manual(package_path)
system(paste(shQuote(file.path(R.home("bin"), "R")),
             "CMD", "Rd2pdf", shQuote(package_path)))

# build tar
system( paste(shQuote(file.path(R.home("bin"), "R")),
              "R CMD build", shQuote(package_path)) )

# check package to ensure it meets CRAN standards.
devtools::check( package_path, cran = T )

# check as cran
system( paste(shQuote(file.path(R.home("bin"), "R")),
              "R CMD check --as-cran",
              shQuote(
                paste(package_path, "_", versionNumber, ".tar.gz", sep = "")
              )) )

