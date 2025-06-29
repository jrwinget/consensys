box::use(
  testthat[expect_equal, expect_true, expect_type, test_that],
)

box::use(
  app/logic/sds_calculations[
    apply_decision_scheme,
    generate_decision_matrix
  ],
  app/logic/sjs_calculations[
    simulate_sjs_process
  ]
)

test_that("SDS calculations produce consistent results", {
  # Test that the same inputs produce the same outputs
  matrix1 <- generate_decision_matrix("majority", 3)
  matrix2 <- generate_decision_matrix("majority", 3)
  expect_equal(matrix1, matrix2)
  
  prefs <- c(0.5, 0.3, 0.2)
  result1 <- apply_decision_scheme(prefs, matrix1)
  result2 <- apply_decision_scheme(prefs, matrix2)
  expect_equal(result1, result2)
})

test_that("SJS simulation shows convergence behavior", {
  # Test with positions that should converge
  initial_positions <- c(45, 50, 55)  # Close positions
  results <- simulate_sjs_process(initial_positions, n_rounds = 10)
  
  initial_spread <- sd(results[1, ])
  final_spread <- sd(results[nrow(results), ])
  
  # Should show some convergence (final spread <= initial spread)
  expect_true(final_spread <= initial_spread)
  
  # Test with positions that are far apart
  divergent_positions <- c(10, 50, 90)  # Far apart positions
  results2 <- simulate_sjs_process(divergent_positions, n_rounds = 10)
  
  # Should still be valid results
  expect_equal(nrow(results2), 11)  # 10 rounds + initial
  expect_equal(ncol(results2), 3)   # 3 individuals
})

test_that("Multiple rounds actually occur in SJS simulation", {
  initial_positions <- c(20, 80)
  results <- simulate_sjs_process(initial_positions, n_rounds = 5)
  
  # Check that positions change over rounds
  round1 <- results[1, ]
  round2 <- results[2, ]
  round_final <- results[nrow(results), ]
  
  # Positions should change between rounds
  expect_true(!identical(round1, round2))
  expect_true(!identical(round1, round_final))
  
  # All positions should be valid (between original bounds)
  expect_true(all(results >= min(initial_positions) - 10))  # Allow some flexibility
  expect_true(all(results <= max(initial_positions) + 10))
})

test_that("SDS schemes produce different outcomes", {
  prefs <- c(0.6, 0.3, 0.1)  # Clear majority for first alternative
  
  maj_matrix <- generate_decision_matrix("majority", 3)
  prop_matrix <- generate_decision_matrix("proportional", 3)
  
  maj_result <- apply_decision_scheme(prefs, maj_matrix)
  prop_result <- apply_decision_scheme(prefs, prop_matrix)
  
  # Proportional should equal input preferences
  expect_equal(as.vector(prop_result), prefs, tolerance = 1e-10)
  
  # Majority rule should be different from proportional
  expect_true(!identical(maj_result, prop_result))
})