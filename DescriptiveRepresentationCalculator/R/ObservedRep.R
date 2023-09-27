#' Compute the observed degree of representation for any group in a political body
#'
#' Finds the degree of observed representation for any group in a political body.
#'
#' @usage
#'
#' ObservedRepresentation(BodyMemberCharacteristics, PopShares)
#'
#' @param BodyMemberCharacteristics A vector specifying the characteristics for members of a political body.
#'
#' @param PopShares A numeric vector specifying population shares of identities specified in `BodyMemberCharacteristics`. The names of the entries in `PopShares` should correspond to identities in `BodyMemberCharacteristics` (see Example).
#'
#' @param BodyShares (optional) A numeric vector with same structure as `PopShares` specifying group population shares of a given body. If specified, used by default instead of `BodyMemberCharacteristics`.
#'
#' @param a=-0.5,b=1 Parameters controlling the affine transformation for how the representation measure is summarized.
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
#'                         BodyMemberCharacteristics = c("A","A","B","C","A","C","A"),
#'                         PopShares = c("A"=1/3,"B"=2/3, "C"=1/3))
#'
#' print( ObsRep )
#'
#' @export
#' @md

ObservedRepresentation <- function( BodyMemberCharacteristics = NULL, PopShares, BodyShares = NULL, a = -1/2, b = 1){
  if(is.null(BodyShares)){
    BodyShares <- prop.table(table( BodyMemberCharacteristics) )
    ObservedIndex <- a*sum(abs(PopShares-BodyShares[names(PopShares)]),na.rm=T) + b
  }
  if(!is.null(BodyShares)){
    ObservedIndex <- a*sum(abs(PopShares-BodyShares),na.rm=T) + b
  }
  if(all(is.na(f2n(BodyShares)))){ ObservedIndex <- NA }

  return( ObservedIndex )
}
