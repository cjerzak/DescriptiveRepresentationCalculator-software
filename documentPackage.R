setwd("~/Documents/ExpectedRepresentationCalculator")

package_path <- "~/Documents/ExpectedRepresentationCalculator/ExpectedRepresentationCalculator"

devtools::document(package_path)
try(file.remove(sprintf("./ExpectedRepresentationCalculator.pdf")),T)
system(paste(shQuote(file.path(R.home("bin"), "R")),
             "CMD", "Rd2pdf", shQuote(package_path)))

#install.packages(package_path)
