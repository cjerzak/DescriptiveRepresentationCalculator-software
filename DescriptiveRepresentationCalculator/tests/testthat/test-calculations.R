library(DescriptiveRepresentationCalculator)
library(testthat)

# Example data from README
body_chars <- c("A","B","A","A","A","A","A","A","A")
pop_shares <- c("A" = 0.8, "B" = 0.2)
body_n <- length(body_chars)

# ExpectedRepresentation -------------------------------------------------------

test_that("ExpectedRepresentation matches theoretical value", {
  expect_equal(
    ExpectedRepresentation(PopShares = pop_shares, BodyN = body_n),
    0.8926258176,
    tolerance = 1e-6
  )
})

# ObservedRepresentation -------------------------------------------------------

test_that("ObservedRepresentation matches manual calculation", {
  expect_equal(
    ObservedRepresentation(BodyMemberCharacteristics = body_chars,
                           PopShares = pop_shares),
    0.9111111111,
    tolerance = 1e-6
  )
})

# SDRepresentation -------------------------------------------------------------

test_that("SDRepresentation is close to theoretical value", {
  set.seed(123)
  sd_val <- SDRepresentation(PopShares = pop_shares,
                             BodyN = body_n,
                             nMonte = 10000)
  expect_equal(sd_val, 0.07904785, tolerance = 0.005)
})

# RelativeRepresentation ------------------------------------------------------

test_that("RelativeRepresentation returns difference", {
  expect_equal(
    RelativeRepresentation(body_chars, pop_shares),
    0.01848529,
    tolerance = 1e-6
  )
})

test_that("RelativeRepresentation standardized is reasonable", {
  set.seed(123)
  val <- RelativeRepresentation(body_chars, pop_shares,
                                standardize = TRUE,
                                nMonte = 10000)
  expect_equal(val, 0.2338494, tolerance = 0.05)
})
