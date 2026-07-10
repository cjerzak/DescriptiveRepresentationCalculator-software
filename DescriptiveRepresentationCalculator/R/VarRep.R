#' Compute the amount of representation left unexplained by a random sampling model.
#'
#' Finds the residual standard deviation when using the expected representation for any group
#' in a political body to predict observed representation as described in Gerring, Jerzak and Oncel (2024). 
#'
#' @usage
#'
#' SDRepresentation(PopShares, BodyN, a = -0.5, b = 1, nMonte = 10000,
#'                  metric = c("L1", "L2"))
#'
#' @param PopShares A numeric vector containing the group-level population proportions.
#'
#' @param BodyN A positive integer denoting the size of the political body in question.
#'
#' @param a,b Parameters controlling the affine transformation for how the representation measure is summarized.
#' That is, `a` and `b` control how the expected L1 deviation of the population shares from the body shares
#' is re-weighted. The expected L1 deviation is the average value of the absolute deviation of the population from body shares under
#' a random sampling model. This expected L1 deviation is multiplied by `a`; `b` is as an additive re-scaling term: `a*E[L1]+b`.
#' By default, `a=-0.5` and `b=1` so that the expected Rose Index of Proportionality is used in the calculation.
#'
#' @param nMonte A positive integer denoting number of Monte Carlo iterations used to approximate the variance of representation under a random sampling model.
#'
#' @param metric A character string selecting the deviation metric underlying the representation index.
#' `"L1"` (the default) uses the sum of absolute deviations between population and body shares,
#' yielding the Rose Index of Proportionality under the default `a` and `b`.
#' `"L2"` uses the sum of squared deviations, yielding the squared-deviation representation
#' index analyzed in Gerring, Hicken, Jerzak, Moser, and Oncel (book manuscript).
#'
#' @return A scalar summary of the amount of representation not explained by a random sampling model.
#' More precisely, this function returns the
#' the residual standard deviation when using the expected degree of representation to predict observed
#' representation under a random sampling model.
#'
#' @export
#'
#' @section References:
#' \itemize{
#' \item John Gerring, Connor T. Jerzak, Erzen Oncel. (2024),
#' The Composition of Descriptive Representation,
#' \emph{American Political Science Review}, 118(2): 784-801.
#' \doi{10.1017/S0003055423000680}
#' }
#'
#' @examples
#'
#' SDRep <- SDRepresentation(PopShares = c(1/4, 2/4, 1/4),
#'                                 BodyN = 50)
#'
#' print( SDRep )
#' 
#' @seealso
#' \itemize{
#' \item \code{\link{ExpectedRepresentation}} for calculating expected representation scores under random sampling. 
#' \item \code{\link{ObservedRepresentation}} for calculating representation scores from observed data.
#' }
#'
#' @importFrom stats rmultinom
#' @export
#' @md

SDRepresentation <- function(PopShares, BodyN, a = -0.5, b = 1, nMonte = 10000,
                             metric = c("L1", "L2")){
  metric <- match.arg(metric)

  # return NA if any NA
  if(any(is.na(PopShares))){return( NA )}

  # validate PopShares (non-negative, sum to 1)
  validatePopShares(PopShares)

  # otherwise, compute SD
  MeanTrue <- ExpectedRepresentation(PopShares = PopShares,
                                     BodyN = BodyN,
                                     a = a,
                                     b = b,
                                     metric = metric)
  SampleBodies <- rmultinom(n=nMonte,size = BodyN,prob = PopShares) / BodyN
  ObsDescrep <- b + a * colSums( shareDeviations(PopShares, SampleBodies, metric) )
  return( SDEst <- sqrt( mean( (ObsDescrep - MeanTrue)^2 ) ) )
}
