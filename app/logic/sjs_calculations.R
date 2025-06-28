#' Calculate SJS weights based on distances between positions
#' @param positions Vector of individual positions
#' @return Matrix of weights
#' @export
calculate_sjs_weights <- function(positions) {
  n <- length(positions)
  weights <- matrix(0, n, n)

  for (i in 1:n) {
    for (j in 1:n) {
      # calculate weight based on distance
      distance <- abs(positions[i] - positions[j])
      weights[i, j] <- 1 / (1 + distance)
    }
  }

  # normalize weights by row
  weights <- t(apply(weights, 1, function(x) x / sum(x)))

  weights
}

#' Apply SJS model to individual judgments
#' @param positions Vector of individual positions
#' @param weights Matrix of weights (optional)
#' @return Final group judgment
#' @export
apply_sjs_model <- function(positions, weights = NULL) {
  if (is.null(weights)) {
    weights <- calculate_sjs_weights(positions)
  }

  # weighted average for each individual
  new_positions <- numeric(length(positions))
  for (i in seq_along(positions)) {
    new_positions[i] <- sum(weights[i, ] * positions)
  }

  new_positions
}

#' Simulate SJS process over multiple rounds
#' @param initial_positions Vector of initial positions
#' @param n_rounds Number of rounds
#' @return Matrix of positions over time
#' @export
simulate_sjs_process <- function(initial_positions, n_rounds = 10) {
  n_individuals <- length(initial_positions)
  positions_matrix <- matrix(0, n_rounds + 1, n_individuals)
  positions_matrix[1, ] <- initial_positions

  for (round in 1:n_rounds) {
    current_positions <- positions_matrix[round, ]
    new_positions <- apply_sjs_model(current_positions)
    positions_matrix[round + 1, ] <- new_positions
  }

  positions_matrix
}
