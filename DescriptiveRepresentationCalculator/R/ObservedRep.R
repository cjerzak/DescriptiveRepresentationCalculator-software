#' Compute the observed degree of representation for any group in a political body
#'
#' Finds the degree of observed representation for any group in a political body.
#'
#' @usage
#'
#' ObservedRepresentation(BodyMemberCharacteristics, PopShares, BodyShares,
#'                        a = -0.5, b = 1, metric = c("L1", "L2"))
#'
#' @param BodyMemberCharacteristics A vector specifying the characteristics for members of a political body.
#'
#' @param PopShares A numeric vector specifying population shares of identities specified in the body-member characteristics input. The names of the entries in `PopShares` should correspond to identities in that body-member characteristics input (see Example).
#'
#' @param BodyShares (optional) A numeric vector with same structure as `PopShares` specifying group population shares of a given body. If supplied with names, they are matched to `PopShares`; otherwise, the order is assumed to correspond to that of `PopShares`.
#'
#' @param a,b Parameters controlling the affine transformation for how the representation measure is summarized.
#' That is, `a` and `b` control how the deviation of the population shares from the body shares
#' is re-weighted. This deviation is multiplied by `a`; `b` is as an additive re-scaling term: `a*deviation+b`.
#' By default, `a=-0.5` and `b=1` so that the Rose Index of Proportionality is returned when `metric = "L1"`.
#'
#' @param metric A character string selecting the deviation metric underlying the representation index.
#' `"L1"` (the default) uses the sum of absolute deviations between population and body shares,
#' yielding the Rose Index of Proportionality under the default `a` and `b`.
#' `"L2"` uses the sum of squared deviations, yielding the squared-deviation representation
#' index analyzed in Gerring, Hicken, Jerzak, Moser, and Oncel (book manuscript). Because squared deviations of
#' proportions are no larger than absolute deviations, the `"L2"` index is always greater than
#' or equal to the `"L1"` index under the default `a` and `b`.
#'
#' @return The observed degree of representation (a scalar). By default, this quantity is the Rose Index of Proportionality.
#' @export
#'
#' @examples
#'
#' ObsRep <- ObservedRepresentation(
#'                         BodyMemberCharacteristics = c("A","A","C","A","C","A"),
#'                         PopShares = c("A"=1/4,"B"=2/4, "C"=1/4))
#'
#' print( ObsRep )
#' 
#' @seealso
#' \itemize{
#' \item \code{\link{ExpectedRepresentation}} for calculating expected representation scores under random sampling. 
#' \item \code{\link{SDRepresentation}} for calculating representation unexplained under the random sampling model. 
#' } 
#'
#' @export
#' @md

ObservedRepresentation <- function( BodyMemberCharacteristics = NULL,
                                    PopShares,
                                    BodyShares = NULL,
                                    a = -0.5, b = 1,
                                    metric = c("L1", "L2")){
  metric <- match.arg(metric)

  # validate PopShares (non-negative, sum to 1)
  if(!any(is.na(PopShares))){
    validatePopShares(PopShares)
  }

  # check for empty body

  if(is.null(BodyShares) && (is.null(BodyMemberCharacteristics) || length(BodyMemberCharacteristics) == 0)){
    warning("BodyMemberCharacteristics is empty. Returning NA.")
    return(NA)
  }

  # derive body shares aligned with PopShares
  BodyShares <- deriveBodyShares(BodyMemberCharacteristics, PopShares, BodyShares)

  # if any body or pop shares are NA, return NA
  if(any(is.na(BodyShares <- f2n(BodyShares)))){ return( ObservedIndex <- NA )  }
  if(any(is.na(PopShares <- f2n(PopShares)))){ return( ObservedIndex <- NA )  }

  # compute observed representation index
  return( ObservedIndex <- a*sum(shareDeviations(PopShares, BodyShares, metric),na.rm=T) + b )
}
