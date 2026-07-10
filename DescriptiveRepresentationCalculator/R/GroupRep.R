#' Compute group-level representation diagnostics for a political body
#'
#' Computes, for each group, the gap between its population share and its share of a
#' political body, both as a raw difference ("shortfall") and as a fraction of the
#' group's population share. These are the group-level "Shortfall" and "Share"
#' measures used in Gerring, Hicken, Jerzak, Moser, and Oncel (book manuscript).
#' The raw shortfall accentuates the under-representation of large groups; the
#' ratio accentuates the under-representation of small groups.
#'
#' @usage
#'
#' GroupRepresentation(BodyMemberCharacteristics, PopShares, BodyShares)
#'
#' @param BodyMemberCharacteristics A vector specifying the characteristics for members of a political body.
#'
#' @param PopShares A numeric vector specifying population shares of identities specified in the body-member characteristics input. The names of the entries in `PopShares` should correspond to identities in that body-member characteristics input (see Example).
#'
#' @param BodyShares (optional) A numeric vector with same structure as `PopShares` specifying group population shares of a given body. If supplied with names, they are matched to `PopShares`; otherwise, the order is assumed to correspond to that of `PopShares`.
#'
#' @return A `data.frame` with one row per group and columns:
#' \itemize{
#' \item `Group`: the group label (names of `PopShares`, or the group index if unnamed).
#' \item `PopShare`: the group's population share.
#' \item `BodyShare`: the group's share of the body.
#' \item `Shortfall`: `PopShare - BodyShare`. Positive values indicate under-representation;
#' negative values indicate over-representation.
#' \item `ShortfallRatio`: `Shortfall / PopShare`, the shortfall as a fraction of the group's
#' population share (`Inf` or `NaN` when `PopShare` is zero). A value of 1 means the group is
#' entirely absent from the body; 0 means proportional representation; negative values indicate
#' over-representation.
#' }
#' Returns `NA` if the body is empty or shares cannot be resolved.
#'
#' @export
#'
#' @section References:
#' \itemize{
#' \item John Gerring, Connor T. Jerzak, Erzen Oncel. (2024),
#' The Composition of Descriptive Representation,
#' \emph{American Political Science Review}, 118(2): 784-801.
#' \doi{10.1017/S0003055423000680}
#' \item John Gerring, Allen Hicken, Connor T. Jerzak, Robert G. Moser, Erzen Oncel.
#' Electoral Rules and Descriptive Representation: A Comprehensive View Across
#' Multiple Identities. Book manuscript.
#' }
#'
#' @examples
#'
#' GroupRep <- GroupRepresentation(
#'                     BodyMemberCharacteristics = c("A","A","C","A","C","A"),
#'                     PopShares = c("A"=1/4, "B"=2/4, "C"=1/4))
#'
#' print( GroupRep )
#'
#' @seealso
#' \itemize{
#' \item \code{\link{ObservedRepresentation}} for summarizing these gaps into a single representation index.
#' \item \code{\link{CompareRepresentation}} for comparing the representation indices of two bodies.
#' }
#'
#' @export
#' @md

GroupRepresentation <- function( BodyMemberCharacteristics = NULL,
                                 PopShares,
                                 BodyShares = NULL){
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
  if(length(BodyShares) != length(PopShares)){
    warning("BodyShares could not be aligned with PopShares (check names). Returning NA.")
    return(NA)
  }

  # if any body or pop shares are NA, return NA
  GroupLabels <- if(!is.null(names(PopShares)) && any(names(PopShares) != "")){
    names(PopShares) } else { seq_along(PopShares) }
  if(any(is.na(BodyShares <- f2n(BodyShares)))){ return( NA ) }
  if(any(is.na(PopShares <- f2n(PopShares)))){ return( NA ) }

  Shortfall <- PopShares - BodyShares
  return( data.frame(Group = GroupLabels,
                     PopShare = PopShares,
                     BodyShare = BodyShares,
                     Shortfall = Shortfall,
                     ShortfallRatio = Shortfall / PopShares,
                     row.names = NULL) )
}
