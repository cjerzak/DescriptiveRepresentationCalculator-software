setwd("~/Documents/ExpectedRepresentationCalculator-software")

package_path <- "~/Documents/ExpectedRepresentationCalculator-software/DescriptiveRepresentationCalculator"

devtools::document(package_path)
try(file.remove("./DescriptiveRepresentationCalculator.pdf"),T)
system(paste(shQuote(file.path(R.home("bin"), "R")),
             "CMD", "Rd2pdf", shQuote(package_path)))

#install.packages(package_path)
