box::use(
  shiny[testServer],
  testthat[
    expect_equal,
    expect_error,
    expect_length,
    expect_named,
    expect_true,
    expect_type,
    test_that
  ],
)

box::use(
  app/logic/sds_calculations[
    apply_decision_scheme,
    calculate_entropy,
    calculate_polarization,
    generate_decision_matrix
  ],
  app/logic/sjs_calculations[
    apply_sjs_round,
    calculate_sjs_consensus,
    calculate_sjs_weights,
    simulate_sjs_process
  ],
)

# SDS Calculations Tests -------------------------------------------------------

test_that("generate_decision_matrix validates inputs correctly", {
  expect_error(
    generate_decision_matrix("majority", 1),
    "`n_alternatives` must be at least 2"
  )
  expect_error(
    generate_decision_matrix("invalid_scheme", 3),
    "Invalid scheme"
  )
})

test_that("generate_decision_matrix produces valid matrices", {
  schemes <- c(
    "majority",
    "proportional",
    "equiprobability",
    "two_thirds",
    "unanimity",
    "truth_wins"
  )

  for (scheme in schemes) {
    result <- generate_decision_matrix(scheme, 3, 5)

    expect_type(result, "double")
    expect_true(is.matrix(result))
    expect_true(all(result >= 0))
    expect_true(all(result <= 1))
    expect_equal(rowSums(result), rep(1, nrow(result)), tolerance = 1e-10)
  }
})

test_that("generate_decision_matrix majority rule works correctly", {
  result <- generate_decision_matrix("majority", 2, 3)
  distributions <- attr(result, "distributions")

  for (i in seq_len(nrow(distributions))) {
    dist <- distributions[i, ]
    decision_row <- result[i, ]

    if (max(dist) > 3 / 2) {
      winner_idx <- which.max(dist)
      expect_equal(decision_row[winner_idx], 1, tolerance = 1e-10)
    } else {
      supported <- which(dist > 0)
      if (length(supported) > 0) {
        expect_equal(
          decision_row[supported],
          rep(1 / length(supported), length(supported)),
          tolerance = 1e-10
        )
      }
    }
  }
})

test_that("apply_decision_scheme returns valid probabilities", {
  decision_matrix <- generate_decision_matrix("majority", 3, 4)
  prefs <- c(0.5, 0.3, 0.2)
  result <- apply_decision_scheme(prefs, decision_matrix)

  expect_type(result, "double")
  expect_length(result, 3)
  expect_equal(sum(result), 1, tolerance = 1e-10)
  expect_true(all(result >= 0))
})

test_that("apply_decision_scheme validates inputs", {
  matrix_with_attrs <- generate_decision_matrix("majority", 3)
  plain_matrix <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), 3, 3)

  expect_error(
    apply_decision_scheme(c(0.5, 0.3, 0.2), plain_matrix),
    "Decision matrix must be generated"
  )
})

test_that("calculate_entropy returns valid values", {
  uniform_probs <- rep(1 / 4, 4)
  expect_equal(calculate_entropy(uniform_probs), 2, tolerance = 1e-10)

  certain_probs <- c(1, 0, 0, 0)
  expect_equal(calculate_entropy(certain_probs), 0, tolerance = 1e-10)

  probs_with_zero <- c(0.7, 0, 0.3)
  entropy <- calculate_entropy(probs_with_zero)
  expected <- -0.7 * log2(0.7) - 0.3 * log2(0.3)
  expect_equal(entropy, expected, tolerance = 1e-10)
})

test_that("calculate_polarization returns valid values", {
  uniform_probs <- rep(1 / 4, 4)
  expect_equal(calculate_polarization(uniform_probs), 0, tolerance = 1e-10)

  extreme_probs <- c(1, 0, 0, 0)
  expect_equal(calculate_polarization(extreme_probs), 1, tolerance = 1e-10)
})

# SJS Calculations Tests -------------------------------------------------------

test_that("calculate_sjs_weights validates inputs", {
  expect_error(calculate_sjs_weights(c(1)), "Need at least 2 positions")
})

test_that("calculate_sjs_weights returns valid weight matrix", {
  positions <- c(1, 3, 7, 10)
  weights <- calculate_sjs_weights(positions)

  expect_type(weights, "double")
  expect_true(is.matrix(weights))
  expect_equal(dim(weights), c(4, 4))
  expect_equal(rowSums(weights), rep(1, 4), tolerance = 1e-10)
  expect_true(all(weights >= 0))
})

test_that("calculate_sjs_weights handles identical positions", {
  identical_positions <- c(5, 5, 5, 5)
  weights <- calculate_sjs_weights(identical_positions)
  expected <- matrix(0.25, 4, 4)
  expect_equal(weights, expected, tolerance = 1e-10)
})

test_that("calculate_sjs_weights distance logic works correctly", {
  positions <- c(0, 50, 100)
  weights <- calculate_sjs_weights(positions, decay_parameter = 2)

  # diagonal is set to 0 (no self-influence), check that weights are symmetric
  # person 2 should receive equal influence from persons 1 and 3 (equidistant)
  expect_equal(weights[2, 1], weights[2, 3], tolerance = 1e-10)
  # diagonal should be 0 (no self-influence in social component)
  expect_equal(weights[2, 2], 0, tolerance = 1e-10)
})

test_that("apply_sjs_round returns valid positions", {
  positions <- c(10, 30, 50, 70)
  result <- apply_sjs_round(positions)

  expect_type(result, "double")
  expect_length(result, length(positions))
  expect_true(all(!is.na(result)))
})

test_that("apply_sjs_round respects self_weight parameter", {
  positions <- c(0, 100)

  result_no_influence <- apply_sjs_round(positions, self_weight = 1)
  expect_equal(result_no_influence, positions, tolerance = 1e-10)

  result_full_influence <- apply_sjs_round(positions, self_weight = 0)
  expect_true(result_full_influence[1] > positions[1])
  expect_true(result_full_influence[2] < positions[2])
})

test_that("simulate_sjs_process validates inputs", {
  expect_error(simulate_sjs_process(c(1)), "Need at least 2 initial positions")
})

test_that("simulate_sjs_process returns valid matrix", {
  initial_positions <- c(10, 30, 50, 70)
  n_rounds <- 5
  result <- simulate_sjs_process(initial_positions, n_rounds)

  expect_type(result, "double")
  expect_true(is.matrix(result))
  expect_equal(dim(result), c(n_rounds + 1, length(initial_positions)))
  expect_equal(result[1, ], initial_positions, tolerance = 1e-10)
})

test_that("simulate_sjs_process convergence detection works", {
  close_positions <- c(49, 50, 51)
  result <- simulate_sjs_process(
    close_positions,
    n_rounds = 20,
    convergence_threshold = 0.01
  )

  expect_true(attr(result, "converged"))
  expect_true(attr(result, "final_round") <= 20)
})

test_that("calculate_sjs_consensus returns valid metrics", {
  initial_positions <- c(10, 50, 90)
  result_matrix <- simulate_sjs_process(initial_positions, n_rounds = 5)
  consensus <- calculate_sjs_consensus(result_matrix)

  expected_names <- c(
    "initial_variance",
    "final_variance",
    "variance_reduction",
    "consensus_level",
    "mean_initial_position",
    "mean_final_position",
    "center_shift",
    "initial_range",
    "final_range",
    "range_reduction",
    "converged",
    "final_round"
  )

  expect_named(consensus, expected_names)
  expect_true(consensus$initial_variance >= 0)
  expect_true(consensus$final_variance >= 0)
})

# Integration Tests ------------------------------------------------------------

test_that("SDS calculations produce consistent results", {
  matrix1 <- generate_decision_matrix("majority", 3)
  matrix2 <- generate_decision_matrix("majority", 3)
  expect_equal(matrix1, matrix2)

  prefs <- c(0.5, 0.3, 0.2)
  result1 <- apply_decision_scheme(prefs, matrix1)
  result2 <- apply_decision_scheme(prefs, matrix2)
  expect_equal(result1, result2)
})

test_that("SJS simulation shows convergence behavior", {
  initial_positions <- c(45, 50, 55)
  results <- simulate_sjs_process(initial_positions, n_rounds = 10)

  initial_spread <- sd(results[1, ])
  final_spread <- sd(results[nrow(results), ])
  expect_true(final_spread <= initial_spread)
})

test_that("SDS schemes produce different outcomes", {
  prefs <- c(0.6, 0.3, 0.1)

  maj_matrix <- generate_decision_matrix("majority", 3)
  prop_matrix <- generate_decision_matrix("proportional", 3)

  maj_result <- apply_decision_scheme(prefs, maj_matrix)
  prop_result <- apply_decision_scheme(prefs, prop_matrix)

  expect_equal(as.vector(prop_result), prefs, tolerance = 1e-10)
  expect_true(!identical(maj_result, prop_result))
})

# TODO: add UI module tests