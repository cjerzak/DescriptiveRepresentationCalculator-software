f2n <- function(.){  as.numeric(as.character(.)) }

#' Validate population shares
#'
#' Checks that population shares are valid: non-negative and sum to 1.
#'
#' @param PopShares A numeric vector of population shares.
#' @param tol Tolerance for checking if shares sum to 1.
#' @return TRUE if valid, otherwise throws an error.
#' @keywords internal
validatePopShares <- function(PopShares, tol = 1e-6) {
  if (any(PopShares < 0)) {
    stop("PopShares contains negative values. All population shares must be non-negative.")
  }
  pop_sum <- sum(PopShares)
  if (abs(pop_sum - 1) > tol) {
    stop(sprintf("PopShares sum to %.6f, but must sum to 1. Please provide valid population proportions.", pop_sum))
  }
  invisible(TRUE)
}

#' Check for body members not in population shares
#'
#' Warns if body member characteristics include groups not present in PopShares.
#'
#' @param BodyMemberCharacteristics A vector of body member characteristics.
#' @param PopShares A named numeric vector of population shares.
#' @return Character vector of unmatched groups (invisibly).
#' @keywords internal
checkUnmatchedBodyMembers <- function(BodyMemberCharacteristics, PopShares) {
  if (is.null(names(PopShares)) || all(names(PopShares) == "")) {
    return(invisible(character(0)))
  }
  body_groups <- unique(as.character(BodyMemberCharacteristics))
  pop_groups <- names(PopShares)
  unmatched <- setdiff(body_groups, pop_groups)
  if (length(unmatched) > 0) {
    warning(sprintf(
      "The following body member groups are not in PopShares and will be ignored: %s",
      paste(unmatched, collapse = ", ")
    ))
  }
  invisible(unmatched)
}
