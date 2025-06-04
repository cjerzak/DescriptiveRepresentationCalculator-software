#' Compute relative representation compared to random sampling
#'
#' Calculates the difference between observed and expected representation.
#' Optionally standardizes this difference using the standard deviation of
#' representation under the random sampling model.
#'
#' @usage
#' RelativeRepresentation(BodyMemberCharacteristics,
#'                        PopShares,
#'                        a = -0.5, b = 1,
#'                        standardize = FALSE,
#'                        nMonte = 10000)
#'
#' @param BodyMemberCharacteristics A vector specifying characteristics for each
#' member of a political body.
#' @param PopShares A numeric vector of population group proportions. Names must
#' correspond to identities in `BodyMemberCharacteristics`.
#' @param a, b Parameters controlling the affine transformation of the
#' representation index, passed to `ObservedRepresentation` and
#' `ExpectedRepresentation`.
#' @param standardize Logical. If `TRUE`, the difference between observed and
#' expected representation is divided by the standard deviation of representation
#' under random sampling.
#' @param nMonte A positive integer denoting number of Monte Carlo iterations used
#' for estimating the standard deviation when `standardize = TRUE`.
#'
#' @return A scalar giving the difference between observed and expected
#' representation. If `standardize = TRUE`, the difference is divided by the
#' standard deviation under the random sampling model.
#' @seealso \code{\link{ObservedRepresentation}},
#'          \code{\link{ExpectedRepresentation}},
#'          \code{\link{SDRepresentation}}
#' @export
#' @md
RelativeRepresentation <- function(BodyMemberCharacteristics,
                                   PopShares,
                                   a = -0.5, b = 1,
                                   standardize = FALSE,
                                   nMonte = 10000){
  ObsRep <- ObservedRepresentation(BodyMemberCharacteristics = BodyMemberCharacteristics,
                                   PopShares = PopShares,
                                   a = a, b = b)
  BodyN <- length(BodyMemberCharacteristics)
  ExpRep <- ExpectedRepresentation(PopShares = PopShares,
                                   BodyN = BodyN,
                                   a = a, b = b)
  RelRep <- ObsRep - ExpRep
  if(standardize){
    SDRep <- SDRepresentation(PopShares = PopShares,
                              BodyN = BodyN,
                              a = a, b = b,
                              nMonte = nMonte)
    RelRep <- RelRep / SDRep
  }
  return(RelRep)
}
