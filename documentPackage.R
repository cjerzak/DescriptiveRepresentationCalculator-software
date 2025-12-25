{
  # setup environment
  rm(  list = ls()  )
  setwd("~/Documents/DescriptiveRepresentationCalculator-software")
  PackageName <- "DescriptiveRepresentationCalculator"
  package_path <- sprintf("~/Documents/%s-software/%s",PackageName,PackageName)
  versionNumber <- "1.1.1"
  
  # build documentation
  library(devtools); library(utils)
  try(file.remove("./DescriptiveRepresentationCalculator.pdf"),T)
  devtools::document(package_path)
  system(paste(shQuote(file.path(R.home("bin"), "R")),
               "CMD", "Rd2pdf", shQuote(package_path)))
  
  setwd("~/Documents/")
  
  # build tar
  system( paste(shQuote(file.path(R.home("bin"), "R")),
                "R CMD build --resave-data", shQuote(package_path)) )
  
  # check package to ensure it meets CRAN standards.
  devtools::check( package_path, cran = T )
  
  # check as cran
  system( paste(shQuote(file.path(R.home("bin"), "R")),
                "R CMD check --as-cran",
                shQuote(
                  paste(PackageName, "_", versionNumber, ".tar.gz", sep = "")
                ))  )
}
