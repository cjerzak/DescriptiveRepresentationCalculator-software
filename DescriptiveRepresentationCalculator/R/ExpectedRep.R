#' Compute the expected degree of representation for any group in a political body
#'
#' Finds the degree of expected representation for any group in a political body under a random sampling model as described in Gerring, Jerzak and Oncel (2024).
#'
#' @usage
#'
#' ExpectedRepresentation(PopShares, BodyN, a = -0.5, b = 1)
#'
#' @param PopShares A numeric vector containing the group-level population proportions.
#'
#' @param BodyN A positive integer denoting the size of the political body in question.
#'
#' @param a,b The `a` and `b` parameters control the affine transformation for how the representation measure is summarized.
#' That is, `a` and `b` control how the expected L1 deviation of the population shares from the body shares
#' is re-weighted. The expected L1 deviation is the average value of the absolute deviation of the population from body shares under
#' a random sampling model. This expected L1 deviation is multiplied by `a`; `b` is as an additive re-scaling term: `a*E[L1]+b`.
#' By default, `a=-0.5` and `b=1` so that the expected Rose Index of Proportionality is returned.
#'
#' @return The expected degree of representation (a scalar).
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
#' ExpectedRep <- ExpectedRepresentation(PopShares = c(1/4, 2/4, 1/4),
#'                                       BodyN = 50)
#'
#' print( ExpectedRep )
#' 
#' @seealso
#' \itemize{
#' \item \code{\link{ObservedRepresentation}} for calculating representation scores from observed data. 
#' \item \code{\link{SDRepresentation}} for calculating representation unexplained under the random sampling model. 
#' }
#' 
#' @export
#' @md

ExpectedRepresentation <- function(PopShares, BodyN, a = -0.5, b = 1){
  # if any pop shares are NA, return NA
  if(any(is.na(PopShares))){return( NA) }

  # validate PopShares (non-negative, sum to 1)
  validatePopShares(PopShares) 

  if(length(PopShares) > 1){
    theoretical_means_log <- log(2) +
      (BodyN - floor(BodyN*PopShares))*log(1 - PopShares) +
      (floor(BodyN*PopShares)+1)*log(PopShares) +
      log(floor(BodyN*PopShares)+1) +
      lchoose(BodyN,floor(BodyN*PopShares)+1)
    theoretical_means <- exp(  theoretical_means_log )
    theoretical_mean <- sum( theoretical_means / BodyN )
  }
  if(length(PopShares) == 1){theoretical_mean <- 0}
  if(abs(max(PopShares) - 1) < 10^(-10)){
    theoretical_means <- 2 * (1 - PopShares)^(BodyN - floor(BodyN*PopShares)) *
      PopShares^(floor(BodyN*PopShares)+1)*
      (floor(BodyN*PopShares)+1)*
      choose(BodyN,floor(BodyN*PopShares)+1)
    theoretical_mean <- sum( theoretical_means / BodyN )
  }
  return( a * theoretical_mean + b )
}
