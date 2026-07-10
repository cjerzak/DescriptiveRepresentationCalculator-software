library(DescriptiveRepresentationCalculator)
library(testthat)

# Example data from README
body_chars <- c("A","B","A","A","A","A","A","A","A")
pop_shares <- c("A" = 0.8, "B" = 0.2)
body_n <- length(body_chars)

# Independent simulator of the Dirichlet-district model (used to verify closed forms)
SimulateDistrictRep <- function(PopShares, concentration, D, selectionRule, nSim){
  K <- length(PopShares)
  alpha <- concentration * PopShares
  mean(replicate(nSim, {
    P <- matrix(rgamma(D * K, shape = rep(alpha, each = D)), nrow = D, ncol = K)
    P <- P / rowSums(P)
    if(selectionRule == "random"){
      winners <- vapply(seq_len(D),
                        function(d) sample.int(K, 1, prob = P[d, ]), integer(1))
    } else {
      winners <- max.col(P)
    }
    G <- tabulate(winners, nbins = K) / D
    1 - 0.5 * sum((colMeans(P) - G)^2)
  }))
}

# ExpectedDistrictRepresentation: random sampling rule ------------------------

test_that("ExpectedDistrictRepresentation (random) matches Proposition 1 arithmetic", {
  PopShares <- c(0.3, 0.7); conc <- 4; D <- 10
  alpha <- conc * PopShares
  manual <- 1 - sum(alpha * (conc - alpha) / (conc * (conc + 1))) / (2 * D)
  expect_equal(
    ExpectedDistrictRepresentation(PopShares = PopShares,
                                   concentration = conc,
                                   nDistricts = D),
    manual,
    tolerance = 1e-10
  )
})

test_that("ExpectedDistrictRepresentation (random) matches simulation", {
  set.seed(202)
  PopShares <- c(0.3, 0.7); conc <- 4; D <- 20
  sim_mean <- SimulateDistrictRep(PopShares, conc, D, "random", nSim = 3000)
  expect_equal(
    ExpectedDistrictRepresentation(PopShares = PopShares,
                                   concentration = conc,
                                   nDistricts = D),
    sim_mean,
    tolerance = 0.005
  )
})

test_that("ExpectedDistrictRepresentation (random) approaches 1 in the large-district limit", {
  expect_equal(
    ExpectedDistrictRepresentation(PopShares = c(0.3, 0.7),
                                   concentration = 4,
                                   nDistricts = Inf),
    1,
    tolerance = 1e-10
  )
})

# ExpectedDistrictRepresentation: affinity (largest-group-wins) rule ----------

test_that("ExpectedDistrictRepresentation (affinity, K = 2) matches Proposition 3 arithmetic", {
  PopShares <- c(0.3, 0.7); conc <- 4; D <- 10
  alpha <- conc * PopShares
  pi_k <- 1 - pbeta(0.5, alpha, rev(alpha))
  mu_k <- PopShares * (1 - pbeta(0.5, alpha + 1, rev(alpha)))
  A <- PopShares^2 + alpha * (conc - alpha) / (conc^2 * (conc + 1) * D)
  B <- 2 * mu_k / D + 2 * (D - 1) * PopShares * pi_k / D
  C <- pi_k / D + (D - 1) * pi_k^2 / D
  manual <- 1 - 0.5 * sum(A - B + C)
  expect_equal(
    ExpectedDistrictRepresentation(PopShares = PopShares,
                                   concentration = conc,
                                   nDistricts = D,
                                   selectionRule = "affinity"),
    manual,
    tolerance = 1e-10
  )
})

test_that("ExpectedDistrictRepresentation (affinity, K = 2) matches simulation", {
  set.seed(203)
  PopShares <- c(0.3, 0.7); conc <- 4; D <- 10
  sim_mean <- SimulateDistrictRep(PopShares, conc, D, "affinity", nSim = 3000)
  expect_equal(
    ExpectedDistrictRepresentation(PopShares = PopShares,
                                   concentration = conc,
                                   nDistricts = D,
                                   selectionRule = "affinity"),
    sim_mean,
    tolerance = 0.005
  )
})

test_that("ExpectedDistrictRepresentation (affinity, K = 2) large-district limit matches Proposition 4", {
  PopShares <- c(0.3, 0.7); conc <- 4
  alpha <- conc * PopShares
  pi_k <- 1 - pbeta(0.5, alpha, rev(alpha))
  manual <- 1 - 0.5 * sum((PopShares - pi_k)^2)
  limit_val <- ExpectedDistrictRepresentation(PopShares = PopShares,
                                              concentration = conc,
                                              nDistricts = Inf,
                                              selectionRule = "affinity")
  expect_equal(limit_val, manual, tolerance = 1e-10)
  # the finite-district expectation converges to the limit
  expect_equal(
    ExpectedDistrictRepresentation(PopShares = PopShares,
                                   concentration = conc,
                                   nDistricts = 1e7,
                                   selectionRule = "affinity"),
    limit_val,
    tolerance = 1e-5
  )
})

test_that("ExpectedDistrictRepresentation (affinity, K = 3) matches simulation", {
  set.seed(303)
  PopShares <- c(0.2, 0.3, 0.5); conc <- 5; D <- 15
  sim_mean <- SimulateDistrictRep(PopShares, conc, D, "affinity", nSim = 3000)
  set.seed(304)
  expect_equal(
    ExpectedDistrictRepresentation(PopShares = PopShares,
                                   concentration = conc,
                                   nDistricts = D,
                                   selectionRule = "affinity",
                                   nMonte = 50000),
    sim_mean,
    tolerance = 0.005
  )
})

test_that("ExpectedDistrictRepresentation (affinity) limit is 1 for symmetric groups", {
  set.seed(999)
  expect_equal(
    ExpectedDistrictRepresentation(PopShares = c(1, 1, 1) / 3,
                                   concentration = 6,
                                   nDistricts = Inf,
                                   selectionRule = "affinity",
                                   nMonte = 20000),
    1,
    tolerance = 1e-3
  )
})

test_that("ExpectedDistrictRepresentation validates inputs", {
  expect_error(ExpectedDistrictRepresentation(c(0.5, 0.5), concentration = -1, nDistricts = 10))
  expect_error(ExpectedDistrictRepresentation(c(0.5, 0.5), concentration = 2, nDistricts = 2.5))
  expect_error(ExpectedDistrictRepresentation(c(0, 1), concentration = 2, nDistricts = 10,
                                              selectionRule = "affinity"))
  expect_true(is.na(ExpectedDistrictRepresentation(c(NA, 0.5), concentration = 2, nDistricts = 10)))
})

# metric = "L2" (squared-deviation representation index) ----------------------

test_that("ExpectedRepresentation L2 matches closed form", {
  expect_equal(
    ExpectedRepresentation(PopShares = pop_shares, BodyN = body_n, metric = "L2"),
    1 - 0.5 * sum(pop_shares * (1 - pop_shares)) / body_n,
    tolerance = 1e-10
  )
})

test_that("ExpectedRepresentation L2 matches Monte Carlo mean", {
  set.seed(42)
  draws <- rmultinom(20000, size = body_n, prob = pop_shares) / body_n
  mc_mean <- mean(1 - 0.5 * colSums((draws - as.numeric(pop_shares))^2))
  expect_equal(
    ExpectedRepresentation(PopShares = pop_shares, BodyN = body_n, metric = "L2"),
    mc_mean,
    tolerance = 0.001
  )
})

test_that("ObservedRepresentation L2 matches manual calculation", {
  body_shares <- c(8 / 9, 1 / 9)
  manual <- 1 - 0.5 * sum((as.numeric(pop_shares) - body_shares)^2)
  expect_equal(
    ObservedRepresentation(BodyMemberCharacteristics = body_chars,
                           PopShares = pop_shares,
                           metric = "L2"),
    manual,
    tolerance = 1e-10
  )
})

test_that("L2 index upper-bounds the L1 index", {
  expect_gte(
    ObservedRepresentation(body_chars, pop_shares, metric = "L2"),
    ObservedRepresentation(body_chars, pop_shares, metric = "L1")
  )
})

test_that("SDRepresentation L2 matches independent simulation", {
  set.seed(7)
  sd_val <- SDRepresentation(PopShares = pop_shares, BodyN = body_n,
                             nMonte = 20000, metric = "L2")
  set.seed(8)
  draws <- rmultinom(20000, size = body_n, prob = pop_shares) / body_n
  obs <- 1 - 0.5 * colSums((draws - as.numeric(pop_shares))^2)
  manual_sd <- sqrt(mean((obs - ExpectedRepresentation(pop_shares, body_n, metric = "L2"))^2))
  expect_equal(sd_val, manual_sd, tolerance = 0.01)
})

test_that("RelativeRepresentation passes the metric through", {
  expect_equal(
    RelativeRepresentation(body_chars, pop_shares, metric = "L2"),
    ObservedRepresentation(body_chars, pop_shares, metric = "L2") -
      ExpectedRepresentation(pop_shares, body_n, metric = "L2"),
    tolerance = 1e-10
  )
})

test_that("default L1 behavior is unchanged", {
  expect_equal(
    ObservedRepresentation(body_chars, pop_shares),
    0.9111111111,
    tolerance = 1e-6
  )
  expect_equal(
    ExpectedRepresentation(PopShares = pop_shares, BodyN = body_n),
    0.8926258176,
    tolerance = 1e-6
  )
})

# GroupRepresentation ----------------------------------------------------------

test_that("GroupRepresentation returns per-group shortfalls", {
  gr <- GroupRepresentation(BodyMemberCharacteristics = body_chars,
                            PopShares = pop_shares)
  expect_s3_class(gr, "data.frame")
  expect_equal(gr$Group, c("A", "B"))
  expect_equal(gr$PopShare, as.numeric(pop_shares))
  expect_equal(gr$BodyShare, c(8 / 9, 1 / 9), tolerance = 1e-10)
  expect_equal(gr$Shortfall, c(0.8 - 8 / 9, 0.2 - 1 / 9), tolerance = 1e-10)
  expect_equal(gr$ShortfallRatio, c((0.8 - 8 / 9) / 0.8, (0.2 - 1 / 9) / 0.2),
               tolerance = 1e-10)
})

test_that("GroupRepresentation accepts pre-computed body shares", {
  gr <- GroupRepresentation(PopShares = pop_shares,
                            BodyShares = c("B" = 0.5, "A" = 0.5))
  expect_equal(gr$BodyShare, c(0.5, 0.5))
  expect_equal(gr$Shortfall, c(0.3, -0.3))
})

test_that("GroupRepresentation handles empty bodies", {
  expect_warning(out <- GroupRepresentation(BodyMemberCharacteristics = c(),
                                            PopShares = pop_shares))
  expect_true(is.na(out))
})

# CompareRepresentation --------------------------------------------------------

test_that("CompareRepresentation equals the difference of observed indices", {
  body1 <- c("A", "B", "A", "A")
  expect_equal(
    CompareRepresentation(BodyMemberCharacteristics1 = body1,
                          BodyMemberCharacteristics2 = body_chars,
                          PopShares = pop_shares),
    ObservedRepresentation(body1, pop_shares) -
      ObservedRepresentation(body_chars, pop_shares),
    tolerance = 1e-10
  )
})

test_that("CompareRepresentation of a body with itself is zero", {
  expect_equal(
    CompareRepresentation(BodyMemberCharacteristics1 = body_chars,
                          BodyMemberCharacteristics2 = body_chars,
                          PopShares = pop_shares),
    0,
    tolerance = 1e-10
  )
})

test_that("CompareRepresentation accepts pre-computed body shares", {
  expect_equal(
    CompareRepresentation(PopShares = pop_shares,
                          BodyShares1 = c("A" = 0.8, "B" = 0.2),
                          BodyShares2 = c("A" = 1.0, "B" = 0.0)),
    ObservedRepresentation(PopShares = pop_shares, BodyShares = c("A" = 0.8, "B" = 0.2)) -
      ObservedRepresentation(PopShares = pop_shares, BodyShares = c("A" = 1.0, "B" = 0.0)),
    tolerance = 1e-10
  )
})
