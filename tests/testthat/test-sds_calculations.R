box::use(
  testthat[expect_equal, expect_true, expect_type, test_that],
)

box::use(
  app/logic/sds_calculations[
    apply_decision_scheme,
    calculate_entropy,
    generate_decision_matrix
  ],
)

test_that("generate_decision_matrix works with majority scheme", {
  result <- generate_decision_matrix("majority", 3)
  expect_type(result, "double")
  expect_equal(dim(result), c(3, 3))
  expect_equal(rowSums(result), rep(1, 3))
})

test_that("generate_decision_matrix works with proportional scheme", {
  result <- generate_decision_matrix("proportional", 3)
  expect_equal(result, diag(3))
})

test_that("apply_decision_scheme returns valid probabilities", {
  res <- diag(3)
  prefs <- c(0.5, 0.3, 0.2)
  result <- apply_decision_scheme(prefs, res)

  expect_type(result, "double")
  expect_equal(sum(result), 1)
  expect_equal(length(result), 3)
})

test_that("calculate_entropy returns valid value", {
  probs <- c(0.5, 0.3, 0.2)
  entropy <- calculate_entropy(probs)

  expect_type(entropy, "double")
  expect_true(entropy >= 0)
  expect_true(entropy <= log2(length(probs)))
})

test_that("calculate_entropy handles zero probabilities", {
  probs <- c(0.7, 0, 0.3)
  entropy <- calculate_entropy(probs)

  expect_type(entropy, "double")
  expect_equal(entropy, -0.7 * log2(0.7) - 0.3 * log2(0.3))
})
