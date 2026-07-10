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

#' Derive body shares aligned with population shares
#'
#' Computes group shares of a body from raw member characteristics when
#' `BodyShares` is not supplied, or aligns a supplied `BodyShares` vector
#' with `PopShares` by name.
#'
#' @param BodyMemberCharacteristics A vector of body member characteristics.
#' @param PopShares A named numeric vector of population shares.
#' @param BodyShares (optional) A numeric vector of pre-computed body shares.
#' @return A numeric vector of body shares aligned with `PopShares`.
#' @keywords internal
deriveBodyShares <- function(BodyMemberCharacteristics, PopShares, BodyShares = NULL) {
  if(is.null(BodyShares)){
    # warn about unmatched body members
    checkUnmatchedBodyMembers(BodyMemberCharacteristics, PopShares)

    BodyShares <- prop.table(table( BodyMemberCharacteristics) )
    BodyShares <- BodyShares[names(PopShares)]
    BodyShares[is.na(BodyShares)] <- 0
  } else {
    # when provided, match by name if names are present
    if(!is.null(names(BodyShares)) && any(names(BodyShares) != "")){
      BodyShares <- BodyShares[names(PopShares)]
    }
  }
  BodyShares
}

#' Compute the deviation between population and body shares
#'
#' Returns per-group absolute (`"L1"`) or squared (`"L2"`) deviations between
#' population and body shares.
#'
#' @param PopShares A numeric vector of population shares.
#' @param BodyShares A numeric vector of body shares.
#' @param metric Either `"L1"` (absolute deviations) or `"L2"` (squared deviations).
#' @return A numeric vector of per-group deviations.
#' @keywords internal
shareDeviations <- function(PopShares, BodyShares, metric) {
  switch(metric,
         "L1" = abs(PopShares - BodyShares),
         "L2" = (PopShares - BodyShares)^2)
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
