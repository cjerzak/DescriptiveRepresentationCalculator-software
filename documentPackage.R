setwd("~/Documents/DescriptiveRepresentationCalculator-software")
package_path <- "~/Documents/DescriptiveRepresentationCalculator-software/DescriptiveRepresentationCalculator"

# build documentation
devtools::document(package_path)
try(file.remove("./DescriptiveRepresentationCalculator.pdf"),T)
system(paste(shQuote(file.path(R.home("bin"), "R")),
             "CMD", "Rd2pdf", shQuote(package_path)))

# Check package to ensure it meets CRAN standards.
devtools::check( package_path )

# build tar
system( paste(shQuote(file.path(R.home("bin"), "R")),
              "R CMD build", shQuote(package_path)) )
