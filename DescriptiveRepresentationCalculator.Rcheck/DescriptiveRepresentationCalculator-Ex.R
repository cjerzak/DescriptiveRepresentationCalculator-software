pkgname <- "DescriptiveRepresentationCalculator"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "DescriptiveRepresentationCalculator-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('DescriptiveRepresentationCalculator')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("ExpectedRepresentation")
### * ExpectedRepresentation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ExpectedRepresentation
### Title: Compute the expected degree of representation for any group in a
###   political body
### Aliases: ExpectedRepresentation

### ** Examples


ExpectedRep <- ExpectedRepresentation(PopShares = c(1/3, 2/3, 1/3),
                                      BodyN = 50)

print( ExpectedRep )




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ExpectedRepresentation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ObservedRepresentation")
### * ObservedRepresentation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ObservedRepresentation
### Title: Compute the observed degree of representation for any group in a
###   political body
### Aliases: ObservedRepresentation

### ** Examples


ObsRep <- ObservedRepresentation(
                        BodyMemberCharacteristics = c("A","A","C","A","C","A"),
                        PopShares = c("A"=1/3,"B"=2/3, "C"=1/3))

print( ObsRep )




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ObservedRepresentation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("SDRepresentation")
### * SDRepresentation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: SDRepresentation
### Title: Compute the amount of representation left unexplained by a
###   random sampling model.
### Aliases: SDRepresentation

### ** Examples


SDRep <- SDRepresentation(PopShares = c(1/3, 2/3, 1/3),
                                BodyN = 50)

print( SDRep )




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("SDRepresentation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
