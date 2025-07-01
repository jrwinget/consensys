box::use(
  stats[var],
)

# define %||% operator for null coalescing
`%||%` <- function(a, b) if (is.null(a)) b else a

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

  # handle identical positions case according to SJS theory
  position_range <- diff(range(positions))
  if (position_range == 0) {
    # all positions identical: equal weights per Davis (1996) SJS model
    return(matrix(1 / n, n, n))
  }

  # compute distance matrix
  dist_matrix <- outer(positions, positions, distance_function)

  # apply SJS exponential decay formula
  # Davis (1996): influence decreases exponentially with distance
  # normalize distances to [0,1] range before applying decay
  normalized_distances <- dist_matrix / position_range

  # SJS weights: higher similarity (lower distance) = higher influence
  weights <- exp(-decay_parameter * normalized_distances)

  # ensure diagonal elements (self-influence) are handled properly
  # in SJS theory, self-weight is typically handled separately
  # set diagonal to 0 for pure social influence calculation
  diag(weights) <- 0

  # normalize rows so each person's received influence sums to 1
  # handle zero-row case (person with no social influence)
  row_sums <- rowSums(weights)
  zero_rows <- row_sums == 0

  if (any(zero_rows)) {
    # if someone has no influence, treat as equal influence from all others
    weights[zero_rows, ] <- 1 / (n - 1)
    weights[zero_rows, zero_rows] <- 0 # no self-influence in social component
    row_sums[zero_rows] <- 1
  }

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
  if (self_weight < 0 || self_weight > 1) {
    stop("self_weight must be between 0 and 1")
  }

  if (is.null(weights)) {
    weights <- calculate_sjs_weights(positions)
  }

  n <- length(positions)
  if (nrow(weights) != n || ncol(weights) != n) {
    stop("Weights matrix dimensions must match number of positions")
  }

  # implement SJS social influence aggregation
  # Davis (1996): weighted average of others' positions, not including self
  soc_influence <- as.vector(weights %*% positions)

  # apply SJS combination rule: blend self and social influence
  new_positions <- self_weight * positions + (1 - self_weight) * soc_influence

  new_positions
}

#' Simulate complete SJS process over multiple rounds
#' @param initial_positions Vector of initial positions
#' @param n_rounds Number of rounds to simulate
#' @param convergence_threshold Stop if max change < threshold (optional)
#' @param self_weight How much weight individuals place on their own position
#' @param decay_parameter Parameter controlling influence decay with distance
#' @return Matrix of positions over time (rows = rounds, cols = individuals)
#' @export
simulate_sjs_process <- function(
    initial_positions,
    n_rounds = 10,
    convergence_threshold = NULL,
    self_weight = 0.5,
    decay_parameter = 1) {
  if (length(initial_positions) < 2) {
    stop("Need at least 2 initial positions")
  }

  # store parameters for consistent influence calculations
  # recalculate weights each round to reflect changing positions (dynamic SJS)
  current_positions <- initial_positions
  position_history <- list(initial_positions)

  converged <- FALSE
  final_round <- n_rounds

  for (round in seq_len(n_rounds)) {
    # recalculate weights based on current positions (dynamic influence)
    weights <- calculate_sjs_weights(
      current_positions,
      decay_parameter = decay_parameter
    )

    # apply one round of influence
    new_positions <- apply_sjs_round(
      current_positions,
      weights = weights,
      self_weight = self_weight
    )

    # check for convergence if threshold specified
    if (!is.null(convergence_threshold)) {
      max_change <- max(abs(new_positions - current_positions))
      if (max_change < convergence_threshold) {
        converged <- TRUE
        final_round <- round
        position_history[[round + 1]] <- new_positions
        break
      }
    }

    position_history[[round + 1]] <- new_positions
    current_positions <- new_positions
  }

  # convert to matrix (rows = time points, cols = individuals)
  positions_matrix <- do.call(rbind, position_history)

  # add metadata attributes
  attr(positions_matrix, "converged") <- converged
  attr(positions_matrix, "final_round") <- final_round
  attr(positions_matrix, "convergence_threshold") <- convergence_threshold
  attr(positions_matrix, "self_weight") <- self_weight
  attr(positions_matrix, "decay_parameter") <- decay_parameter

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

  # when initial variance is 0, consensus measures need special handling
  if (initial_variance == 0) {
    variance_reduction <- 0
    consensus_level <- 1 # perfect consensus maintained
  } else {
    variance_reduction <- (initial_variance - final_variance) / initial_variance
    consensus_level <- 1 - (final_variance / initial_variance)
  }

  # add additional SJS-specific metrics
  # movement toward center (convergence indicator)
  initial_mean <- mean(initial_positions)
  final_mean <- mean(final_positions)
  center_shift <- abs(final_mean - initial_mean)

  # calculate polarization index as per SJS literature
  initial_range <- diff(range(initial_positions))
  final_range <- diff(range(final_positions))
  range_reduction <- if (initial_range == 0) {
    0
  } else {
    (initial_range - final_range) / initial_range
  }

  list(
    initial_variance = initial_variance,
    final_variance = final_variance,
    variance_reduction = variance_reduction,
    consensus_level = consensus_level,
    mean_initial_position = initial_mean,
    mean_final_position = final_mean,
    center_shift = center_shift,
    initial_range = initial_range,
    final_range = final_range,
    range_reduction = range_reduction,
    converged = attr(positions_matrix, "converged") %||% FALSE,
    final_round = attr(positions_matrix, "final_round") %||%
      nrow(positions_matrix) -
      1
  )
}
