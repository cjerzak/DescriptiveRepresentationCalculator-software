#' Compute the amount of representation not explained by a random sampling model.
#'
#' Finds the residual standard deviation when using the expected representation for any group
#' in a political body to predict observed representation (Gerring, Jerzak and Oncel, 2022+).
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
#' More precisely, `a` and `b` control how the L1 deviation of the population shares from the body shares
#' are re-weighted (the expected L1 deviation is multiplied by `a`; `b` is as an additive re-scaling term). By default,
#' `a=-0.5` and `b=1` so that the expected Rose Index of Proportionality is returned.
#'
#' @return A summary of the amount of representation not explained by a random sampling model.
#' More precisely, this function returns the
#' the residual standard deviation when using the expected degree of representation to predict observed
#' representatoin under a random sampling model.
#'
#' @export
#'
#' @examples
#'
#' ResidualRep <- ResidualRepresentation(PopShares = c(1/3, 2/3,1/3),
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
