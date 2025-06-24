box::use(
  glue[glue],
  purrr[map],
)

#' Generate a decision matrix based on the specified scheme
#' @param scheme The type of decision scheme to use
#' @param n_alternatives Number of alternatives
#' @return A transition probability matrix
#' @export
generate_decision_matrix <- function(scheme, n_alternatives) {
  if (!is.numeric(n_alternatives) || n_alternatives < 1) {
    stop("n_alternatives must be a positive integer")
  }

  valid_schemes <- c(
    "majority", "proportional", "twothirds", "unanimity", "truth"
  )

  if (!scheme %in% valid_schemes) {
    stop(
      glue("Invalid scheme. Must be one of: {valid_schemes}")
    )
  }

  # return proportional scheme immediately
  if (scheme == "proportional") {
    return(diag(n_alternatives))
  }

  # generate distributions for current state
  distributions <- matrix(
    1 / n_alternatives,
    nrow = n_alternatives,
    ncol = n_alternatives
  )

  schemes <- list(
    majority = function(probs) {
      if (max(probs) > 0.5) {
        v <- numeric(length(probs))
        v[which.max(probs)] <- 1
        v
      } else {
        probs
      }
    },
    twothirds = function(probs) {
      if (max(probs) >= 2 / 3) {
        v <- numeric(length(probs))
        v[which.max(probs)] <- 1
        v
      } else {
        probs
      }
    },
    unanimity = function(probs) {
      idx <- which(probs == 1)
      if (length(idx)) {
        v <- numeric(length(probs))
        v[idx[1]] <- 1
        v
      } else {
        probs
      }
    },
    truth = function(probs) {
      if (probs[1] > 0) {
        v <- numeric(length(probs))
        v[1] <- 1
        v
      } else {
        probs
      }
    }
  )

  row_fn <- schemes[[scheme]]

  matrix_decision <- if (!is.null(row_fn)) {
    do.call(
      rbind,
      map(seq_len(n_alternatives), ~ row_fn(distributions[.x, ]))
    )
  } else {
    diag(n_alternatives)
  }

  # normalize rows
  matrix_decision <- matrix_decision / rowSums(matrix_decision)

  return(matrix_decision)
}

#' Generate all possible compositions of n numbers that sum to total
#' @param total The sum to achieve
#' @param n Number of elements
#' @return Matrix of compositions
#' @keywords internal
compositions <- function(total, n) {
  if (n == 1) {
    return(matrix(total, 1, 1))
  }
  result <- matrix(0, 0, n)
  for (i in 0:total) {
    sub_comps <- compositions(total - i, n - 1)
    result <- rbind(result, cbind(i, sub_comps))
  }
  return(result)
}

#' Apply a decision scheme to individual preferences
#' @param individual_prefs Vector of individual preferences
#' @param matrix_decision Decision matrix
#' @return Vector of group decision probabilities
#' @export
apply_decision_scheme <- function(individual_prefs, matrix_decision) {
  if (length(individual_prefs) != nrow(matrix_decision)) {
    stop("Dimension mismatch between preferences and decision matrix")
  }

  # ensure preferences sum to 1
  individual_prefs <- individual_prefs / sum(individual_prefs)

  # apply decision scheme
  group_prefs <- matrix_decision %*% individual_prefs

  # ensure output sums to 1
  # TODO: check for any numerical precision issues
  group_prefs / sum(group_prefs)
}

#' Calculate entropy of a probability distribution
#' @param probs Vector of probabilities
#' @return Entropy value
#' @export
calculate_entropy <- function(probs) {
  probs <- probs[probs > 0] # remove zero probabilities
  -sum(probs * log2(probs))
}
