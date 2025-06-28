box::use(
  glue[glue],
)

#' Generate a decision matrix based on the specified scheme
#' @param scheme The type of decision scheme to use
#' @param n_alternatives Number of alternatives
#' @return A transition probability matrix
#' @export
generate_decision_matrix <- function(scheme, n_alternatives) {
  if (!is.numeric(n_alternatives) || n_alternatives < 1) {
    stop("`n_alternatives` must be a positive integer.")
  }

  valid_schemes <- c(
    "majority",
    "proportional",
    "two_thirds",
    "unanimity",
    "truth"
  )

  if (!scheme %in% valid_schemes) {
    stop(glue(
      "Invalid scheme. Must be one of: {paste(valid_schemes, collapse = \", \")}"
    ))
  }

  matrix_decision <- if (scheme == "proportional") {
    diag(n_alternatives)
  } else {
    # uniform base distributions
    distr <- matrix(1 / n_alternatives, n_alternatives, n_alternatives)

    # scheme-specific row transformers
    schemes <- list(
      majority = function(p) {
        if (max(p) > 0.5) {
          v <- numeric(length(p))
          v[which.max(p)] <- 1
          v
        } else {
          p
        }
      },
      two_thirds = function(p) {
        if (max(p) >= 2 / 3) {
          v <- numeric(length(p))
          v[which.max(p)] <- 1
          v
        } else {
          p
        }
      },
      unanimity = function(p) {
        idx <- which(p == 1)
        if (length(idx)) {
          v <- numeric(length(p))
          v[idx[1]] <- 1
          v
        } else {
          p
        }
      },
      truth = function(p) {
        if (p[1] > 0) {
          v <- numeric(length(p))
          v[1] <- 1
          v
        } else {
          p
        }
      }
    )

    row_fn <- schemes[[scheme]]

    mat <- t(
      vapply(
        seq_len(n_alternatives),
        function(i) row_fn(distr[i, ]),
        numeric(n_alternatives)
      )
    )

    mat / rowSums(mat)
  }

  matrix_decision
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

  result
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
