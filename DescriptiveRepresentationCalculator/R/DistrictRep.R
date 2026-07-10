#' Compute the expected degree of representation in a district-based electoral body
#'
#' Finds the expected degree of representation for a political body elected from
#' single-member districts, where district-level group compositions are drawn from
#' a Dirichlet distribution, as derived in Gerring, Hicken, Jerzak, Moser, and Oncel
#' (book manuscript on electoral rules and descriptive representation).
#'
#' @usage
#'
#' ExpectedDistrictRepresentation(PopShares, concentration, nDistricts,
#'                                selectionRule = c("random", "affinity"),
#'                                a = -0.5, b = 1, nMonte = 10000)
#'
#' @param PopShares A numeric vector containing the group-level population proportions
#' (the mean district shares). Under `selectionRule = "affinity"`, all entries must be
#' strictly positive; drop zero-share groups before calling.
#'
#' @param concentration A positive number giving the Dirichlet concentration parameter
#' (`alpha_0`) governing the spatial concentration of groups across districts. District
#' compositions are drawn from `Dirichlet(concentration * PopShares)`. Small values produce
#' extreme district compositions (high geographic segregation); large values produce district
#' compositions close to `PopShares` (geographic dispersion).
#'
#' @param nDistricts A positive integer denoting the number of single-member districts,
#' or `Inf` for the large-district limit.
#'
#' @param selectionRule A character string specifying how each district seat is filled.
#' `"random"` (the default): the probability that a group wins a district seat equals its
#' population share in that district (random sampling). `"affinity"`: the largest group in
#' each district deterministically captures the seat (largest-group-wins affinity voting).
#'
#' @param a,b Parameters controlling the affine transformation for how the representation
#' measure is summarized. The expected sum of squared deviations between aggregate population
#' shares and seat shares is multiplied by `a`; `b` is an additive re-scaling term:
#' `a*E[sum of squared deviations]+b`. By default, `a=-0.5` and `b=1` so that the expected
#' squared-deviation representation index is returned.
#'
#' @param nMonte A positive integer denoting the number of Monte Carlo draws used to
#' approximate the plurality-win probabilities when `selectionRule = "affinity"` and more
#' than two groups are present. Ignored otherwise (those cases are computed in closed form).
#'
#' @return The expected degree of representation (a scalar).
#'
#' @details
#' This function computes the expected value of the squared-deviation representation index
#' (the `metric = "L2"` index; see \code{\link{ObservedRepresentation}}), not the L1-based
#' Rose Index of Proportionality. The squared-deviation index upper-bounds the Rose Index
#' and preserves the ordering of representation gaps when there are two groups.
#'
#' Under `selectionRule = "random"`, the expectation has the closed form
#' `1 - (1/(2*nDistricts)) * sum(alpha*(alpha0-alpha)/(alpha0*(alpha0+1)))` (with default
#' `a`, `b`), where `alpha = concentration * PopShares` and `alpha0 = concentration`.
#' As `nDistricts` grows, the expectation approaches 1: with many districts,
#' random-sampling representation converges to the proportional benchmark. Setting
#' `nDistricts = Inf` returns this limit.
#'
#' Under `selectionRule = "affinity"`, the expectation depends on each group's plurality-win
#' probability (the probability it is the largest group in a district) and, for finite
#' `nDistricts`, on the upper-tail moment of the district-share distribution. With two groups,
#' both quantities have closed forms in terms of the Beta distribution function and results
#' are exact. With more than two groups, they are approximated by Monte Carlo simulation of
#' Dirichlet draws (set a seed for reproducibility). Setting `nDistricts = Inf` returns the
#' large-district limit, `1 - (1/2) * sum((PopShares - PluralityProbs)^2)` (with default
#' `a`, `b`), which equals 1 only when each group's probability of being the district
#' plurality matches its population share. The gap between these two quantities captures
#' the representational consequences of geographic concentration under affinity voting.
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
#' # Expected representation under random sampling with 100 districts
#' ExpectedDistrictRep <- ExpectedDistrictRepresentation(
#'                            PopShares = c(1/4, 2/4, 1/4),
#'                            concentration = 2,
#'                            nDistricts = 100)
#' print( ExpectedDistrictRep )
#'
#' # Large-district limit under largest-group-wins affinity voting (two groups;
#' # computed exactly via the Beta distribution function)
#' ExpectedAffinityRep <- ExpectedDistrictRepresentation(
#'                            PopShares = c(0.3, 0.7),
#'                            concentration = 4,
#'                            nDistricts = Inf,
#'                            selectionRule = "affinity")
#' print( ExpectedAffinityRep )
#'
#' @seealso
#' \itemize{
#' \item \code{\link{ExpectedRepresentation}} for expected representation of a single body under random sampling.
#' \item \code{\link{ObservedRepresentation}} for calculating representation scores from observed data.
#' }
#'
#' @importFrom stats pbeta rgamma
#' @export
#' @md

ExpectedDistrictRepresentation <- function(PopShares, concentration, nDistricts,
                                           selectionRule = c("random", "affinity"),
                                           a = -0.5, b = 1, nMonte = 10000){
  selectionRule <- match.arg(selectionRule)

  # if any inputs are NA, return NA
  if(any(is.na(PopShares)) || any(is.na(concentration)) || any(is.na(nDistricts))){ return( NA ) }

  # validate PopShares (non-negative, sum to 1)
  validatePopShares(PopShares)

  if(length(concentration) != 1 || !is.finite(concentration) || concentration <= 0){
    stop("concentration must be a single positive, finite number.")
  }
  if(length(nDistricts) != 1 || nDistricts < 1 ||
     (is.finite(nDistricts) && nDistricts != floor(nDistricts))){
    stop("nDistricts must be a single positive integer (or Inf for the large-district limit).")
  }

  # with a single group, the body trivially mirrors the population
  if(length(PopShares) == 1){ return( a * 0 + b ) }

  PopShares <- f2n(PopShares)
  alpha <- concentration * PopShares
  alpha0 <- concentration
  D <- nDistricts
  K <- length(PopShares)

  if(selectionRule == "random"){
    # Propositions 1-2: E[(P_k - G_k)^2] = alpha_k*(alpha0 - alpha_k) / (D*alpha0*(alpha0+1))
    if(is.infinite(D)){ return( a * 0 + b ) }
    ExpectedSqDev <- alpha * (alpha0 - alpha) / (D * alpha0 * (alpha0 + 1))
    return( a * sum(ExpectedSqDev) + b )
  }

  # affinity: largest-group-wins seat allocation (Propositions 3-4)
  if(any(PopShares == 0)){
    stop("PopShares must be strictly positive under the affinity selection rule.")
  }
  if(K == 2){
    # closed forms via the Beta distribution function
    PluralityProbs <- 1 - pbeta(1/2, alpha, rev(alpha))
    TailMoments <- PopShares * (1 - pbeta(1/2, alpha + 1, rev(alpha)))
  } else {
    # Monte Carlo approximation of plurality-win probabilities and tail moments
    if(length(nMonte) != 1 || is.na(nMonte) || nMonte < 1){
      stop("nMonte must be a single positive integer.")
    }
    DirichletDraws <- matrix(rgamma(nMonte * K, shape = rep(alpha, each = nMonte)),
                             nrow = nMonte, ncol = K)
    RowTotals <- rowSums(DirichletDraws)
    if(any(Degenerate <- RowTotals == 0)){
      # gamma draws can underflow to zero for tiny concentrations; in that limit,
      # districts collapse to a single-group vertex with probability PopShares
      VertexGroups <- sample.int(K, sum(Degenerate), replace = TRUE, prob = PopShares)
      DirichletDraws[Degenerate, ] <- 0
      DirichletDraws[cbind(which(Degenerate), VertexGroups)] <- 1
      RowTotals[Degenerate] <- 1
    }
    DirichletDraws <- DirichletDraws / RowTotals
    Winners <- max.col(DirichletDraws)
    PluralityProbs <- tabulate(Winners, nbins = K) / nMonte
    TailMoments <- vapply(seq_len(K),
                          function(k){ mean(DirichletDraws[, k] * (Winners == k)) },
                          numeric(1))
  }

  if(is.infinite(D)){
    # Proposition 4 and its K > 2 extension
    return( a * sum((PopShares - PluralityProbs)^2) + b )
  }

  # Proposition 3: E[(P_k - G_k)^2] = A - B + C per group
  A_term <- PopShares^2 + alpha * (alpha0 - alpha) / (alpha0^2 * (alpha0 + 1) * D)
  B_term <- 2 * TailMoments / D + 2 * (D - 1) * PopShares * PluralityProbs / D
  C_term <- PluralityProbs / D + (D - 1) * PluralityProbs^2 / D
  ExpectedSqDev <- A_term - B_term + C_term
  return( a * sum(ExpectedSqDev) + b )
}
