box::use(
  purrr[accumulate],
  tibble[as_tibble],
  dplyr[c_across, rowwise, ungroup, mutate, pull],
  stats[var]
)

#' Calculate SJS influence weights based on distance between positions
#' @param positions Vector of individual positions
#' @param distance_function Function to calculate distance (default: absolute
#'        difference)
#' @param decay_parameter Parameter controlling how quickly influence decays
#'        with distance (higher = more decay)
#' @return Matrix of influence weights where W[i,j] is influence of j on i
#' @export
calculate_sjs_weights <- function(
    positions,
    distance_function = function(x, y) abs(x - y),
    decay_parameter = 1) {
  
  n <- length(positions)
  if (n < 2) {
    stop("Need at least 2 positions to calculate weights")
  }
  
  # calculate pairwise distances
  position_range <- diff(range(positions))
  if (position_range == 0) {
    # all positions identical: equal weights
    return(matrix(1 / n, n, n))
  }
  
  # compute distance matrix
  dist_matrix <- outer(positions, positions, distance_function)
  
  # convert distances to weights using exponential decay
  # closer positions have higher influence
  weights <- exp(-decay_parameter * dist_matrix / position_range)
  
  # normalize rows so each person's received influence sums to 1
  row_sums <- rowSums(weights)
  weights / row_sums
}

#' Apply one round of SJS influence
#' @param positions Current vector of individual positions
#' @param weights Matrix of influence weights (optional, will be calculated if
#'        NULL)
#' @param self_weight How much weight individuals place on their own position
#'        (0-1, where 1 = no influence from others)
#' @return Updated positions after one round of influence
#' @export
apply_sjs_round <- function(
    positions,
    weights = NULL,
    self_weight = 0.5) {
  
  if (is.null(weights)) {
    weights <- calculate_sjs_weights(positions)
  }
  
  n <- length(positions)
  if (nrow(weights) != n || ncol(weights) != n) {
    stop("Weights matrix dimensions must match number of positions")
  }
  
  # for each person, compute weighted average of all positions
  # then blend with their own position using self_weight
  influenced_positions <- as.vector(weights %*% positions)
  
  # blend self and social influence
  self_weight * positions + (1 - self_weight) * influenced_positions
}

#' Simulate complete SJS process over multiple rounds
#' @param initial_positions Vector of initial positions
#' @param n_rounds Number of rounds to simulate
#' @param convergence_threshold Stop if max change < threshold (optional)
#' @param self_weight How much weight individuals place on their own position
#' @return Matrix of positions over time (rows = rounds, cols = individuals)
#' @export
simulate_sjs_process <- function(
    initial_positions,
    n_rounds = 10,
    convergence_threshold = NULL,
    self_weight = 0.5) {
  
  if (length(initial_positions) < 2) {
    stop("Need at least 2 initial positions")
  }
  
  # simulate rounds using accumulate
  position_sequence <- accumulate(
    seq_len(n_rounds),
    ~ apply_sjs_round(.x, self_weight = self_weight),
    .init = initial_positions
  )
  
  # convert to matrix (rows = time points, cols = individuals)
  positions_matrix <- do.call(rbind, position_sequence)
  
  # check for convergence if threshold specified
  converged <- FALSE
  final_round <- n_rounds
  
  if (!is.null(convergence_threshold) && n_rounds > 1) {
    # calculate round-to-round changes
    for (round in 2:nrow(positions_matrix)) {
      current_pos <- positions_matrix[round, ]
      previous_pos <- positions_matrix[round - 1, ]
      max_change <- max(abs(current_pos - previous_pos))
      
      if (max_change < convergence_threshold) {
        # convergence achieved: truncate matrix
        positions_matrix <- positions_matrix[1:round, , drop = FALSE]
        converged <- TRUE
        final_round <- round - 1  # actual simulation rounds
        break
      }
    }
  }
  
  # add metadata attributes
  attr(positions_matrix, "converged") <- converged
  attr(positions_matrix, "final_round") <- final_round
  attr(positions_matrix, "convergence_threshold") <- convergence_threshold
  
  positions_matrix
}

#' Calculate consensus measure for SJS results
#' @param positions_matrix Matrix from simulate_sjs_process
#' @return List with consensus metrics
#' @export
calculate_sjs_consensus <- function(positions_matrix) {
  if (nrow(positions_matrix) < 2) {
    stop("Need at least 2 time points to calculate consensus")
  }
  
  final_positions <- positions_matrix[nrow(positions_matrix), ]
  initial_positions <- positions_matrix[1, ]

  initial_variance <- var(initial_positions)
  final_variance <- var(final_positions)
  
  # handle case where initial variance is 0 (all start at same position)
  variance_reduction <- if (initial_variance == 0) {
    0
  } else {
    (initial_variance - final_variance) / initial_variance
  }
  
  consensus_level <- if (initial_variance == 0) {
    1  # perfect consensus maintained
  } else {
    1 - (final_variance / initial_variance)
  }

  list(
    initial_variance = initial_variance,
    final_variance = final_variance,
    variance_reduction = variance_reduction,
    consensus_level = consensus_level,
    mean_final_position = mean(final_positions),
    position_range = diff(range(final_positions)),
    converged = attr(positions_matrix, "converged") %||% FALSE,
    final_round = attr(positions_matrix, "final_round") %||% 
      nrow(positions_matrix) - 1
  )
}
