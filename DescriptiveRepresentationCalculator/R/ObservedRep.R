#' Compute the observed degree of representation for any group in a political body
#'
#' Finds the degree of observed representation for any group in a political body.
#'
#' @usage
#'
#' ObservedRepresentation(BodyMemberCharacteristics, PopShares, BodyShares, a = -0.5, b = 1)
#'
#' @param BodyMemberCharacteristics A vector specifying the characteristics for members of a political body.
#'
#' @param PopShares A numeric vector specifying population shares of identities specified in the body-member characteristics input. The names of the entries in `PopShares` should correspond to identities in that body-member characteristics input (see Example).
#'
#' @param BodyShares (optional) A numeric vector with same structure as `PopShares` specifying group population shares of a given body. If specified, used by default instead of `BodyMemberCharacteristics`.
#'
#' @param a,b Parameters controlling the affine transformation for how the representation measure is summarized.
#' That is, `a` and `b` control how the L1 deviation of the population shares from the body shares
#' is re-weighted. This expected L1 deviation is multiplied by `a`; `b` is as an additive re-scaling term: `a*L1+b`.
#' By default, `a=-0.5` and `b=1` so that the Rose Index of Proportionality is returned.
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
                                    a = -0.5, b = 1){
  # if BodyShares 
  if(is.null(BodyShares)){
    BodyShares <- prop.table(table( BodyMemberCharacteristics) )
    BodyShares <- BodyShares[names(PopShares)]
    BodyShares[is.na(BodyShares)] <- 0 
  }
  
  # if any body or pop shares are NA, return NA
  if(any(is.na(BodyShares <- f2n(BodyShares)))){ return( ObservedIndex <- NA )  }
  if(any(is.na(PopShares <- f2n(PopShares)))){ return( ObservedIndex <- NA )  }

  # compute observed representation index 
  return( ObservedIndex <- a*sum(abs(PopShares-BodyShares),na.rm=T) + b )
}
