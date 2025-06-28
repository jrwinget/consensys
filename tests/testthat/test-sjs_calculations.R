box::use(
  testthat[expect_equal, expect_true, expect_type, test_that],
)

box::use(
  app/logic/sjs_calculations[
    apply_sjs_model,
    calculate_sjs_weights,
    simulate_sjs_process
  ],
)

test_that("calculate_sjs_weights returns valid weight matrix", {
  positions <- c(1, 2, 4)
  weights <- calculate_sjs_weights(positions)

  expect_type(weights, "double")
  expect_equal(dim(weights), c(3, 3))
  expect_equal(rowSums(weights), rep(1, 3))
})

test_that("apply_sjs_model returns valid group judgment", {
  positions <- c(1, 2, 4)
  result <- apply_sjs_model(positions)

  expect_type(result, "double")
  expect_equal(length(result), length(positions))
  expect_true(all(!is.na(result)))
})

test_that("simulate_sjs_process returns valid positions matrix", {
  initial_positions <- c(1, 2, 4)
  n_rounds <- 3
  result <- simulate_sjs_process(initial_positions, n_rounds)

  expect_type(result, "double")
  expect_equal(dim(result), c(n_rounds + 1, length(initial_positions)))
})
