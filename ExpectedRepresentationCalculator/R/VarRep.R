#' Compute the amount of representation left unexplained by a random sampling model.
#'
#' Finds the residual standard deviation when using the expected representation for any group
#' in a political body to predict observed representation as described in Gerring, Jerzak and Oncel, 2023+.
#'
#' @usage
#'
#' ResidualRepresentation(PopShares, BodyN)
#'
#' @param PopShares A numeric vector containing the group-level population proportions.
#'
#' @param BodyN A positive integer denoting the size of the political body in question.
#'
#' @param a=-0.5,b=1 Parameters controlling the affine transformation for how the representation measure is summarized.
#' That is, `a` and `b` control how the expected L1 deviation of the population shares from the body shares
#' is re-weighted. The expected L1 deviation is the average value of the absolute deviation of the population from body shares under
#' a random sampling model. This expected L1 deviation is multiplied by `a`; `b` is as an additive re-scaling term: `a*E[L1]+b`.
#' By default, `a=-0.5` and `b=1` so that the expected Rose Index of Proportionality is used in the calculation.
#'
#' @return A scalar summary of the amount of representation not explained by a random sampling model.
#' More precisely, this function returns the
#' the residual standard deviation when using the expected degree of representation to predict observed
#' representation under a random sampling model.
#'
#' @export
#'
#' @examples
#'
#' ResidualRep <- ResidualRepresentation(PopShares = c(1/3, 2/3, 1/3),
#'                                       BodyN = 50)
#'
#' print( ResidualRep )
#'
#' @export
#'
#' @md

ResidualRepresentation <- function(PopShares, BodyN, a = -1/2, b = 1,nMonte = 10000){
  MeanTrue <- theoretical_L1deviance_affine(PopShares = PopShares,
                                            BodyN = BodyN,
                                            a = a,
                                            b = b)
  SampleBodies <- rmultinom(n=nMonte,size = BodyN,prob = PopShares) / BodyN
  ObsDescrep <- b + a * colSums( abs(SampleBodies - PopShares ) )
  SDEst <- sqrt( mean( (ObsDescrep - MeanTrue)^2 ) )
}
