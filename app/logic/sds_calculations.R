box::use(
  dplyr[c_across, everything, mutate, pull, rowwise, ungroup],
  glue[glue],
  tibble[as_tibble],
)

#' Apply a decision scheme to transform individual preferences to group
#' probabilities
#' @param individual_prefs Vector of individual preference probabilities (must
#'        sum to 1)
#' @param decision_matrix Decision matrix from generate_decision_matrix()
#' @return Vector of group decision probabilities
#' @export
apply_decision_scheme <- function(individual_prefs, decision_matrix) {
  # validate inputs
  distributions <- attr(decision_matrix, "distributions")
  if (is.null(distributions)) {
    stop("Decision matrix must be generated with generate_decision_matrix()")
  }
  n_alternatives <- attr(decision_matrix, "n_alternatives")
  if (length(individual_prefs) != n_alternatives) {
    stop(glue("individual_prefs must have length {n_alternatives}"))
  }

  # ensure preferences sum to 1 (normalize)
  individual_prefs <- individual_prefs / sum(individual_prefs)

  # calculate probability of each distinguishable distribution
  distributions_df <- as_tibble(distributions, .name_repair = "minimal")
  names(distributions_df) <- paste0("alt_", seq_len(n_alternatives))

  distribution_probs <- distributions_df |>
    rowwise() |>
    mutate(
      prob = multinomial_probability(c_across(everything()), individual_prefs)
    ) |>
    ungroup() |>
    pull(prob)

  # apply decision scheme: π · D where π is distribution probabilities
  group_probs <- as.vector(distribution_probs %*% decision_matrix)

  # ensure output sums to 1 (handle numerical precision)
  group_probs / sum(group_probs)
}

#' Calculate entropy of a probability distribution
#' @param probs Vector of probabilities
#' @return Entropy value (in bits)
#' @export
calculate_entropy <- function(probs) {
  # remove zero probabilities to avoid log(0)
  nonzero_probs <- probs[probs > .Machine$double.eps]
  if (length(nonzero_probs) <= 1) {
    return(0)
  }
  -sum(nonzero_probs * log2(nonzero_probs))
}

#' Calculate polarization index
#' @param probs Vector of probabilities
#' @return Polarization measure (0 = uniform, 1 = maximum polarization)
#' @export
calculate_polarization <- function(probs) {
  n <- length(probs)
  if (n <= 1) {
    return(0)
  }
  # deviation from uniform distribution
  uniform_prob <- 1 / n
  sum(abs(probs - uniform_prob)) / (2 * (n - 1) / n)
}

#' Generate a decision matrix based on the specified scheme
#' @param scheme The type of decision scheme to use
#' @param n_alternatives Number of alternatives
#' @param group_size Size of the group (default 5)
#' @return A decision scheme matrix D where D[i,j] is probability that
#'         distribution i leads to choice of alternative j
#' @export
generate_decision_matrix <- function(scheme, n_alternatives, group_size = 5) {
  # input validation
  if (!is.numeric(n_alternatives) || n_alternatives < 2) {
    stop("`n_alternatives` must be at least 2.")
  }
  if (!is.numeric(group_size) || group_size < 2) {
    stop("`group_size` must be at least 2.")
  }

  valid_schemes <- c(
    "majority",
    "proportional",
    "equiprobability",
    "two_thirds",
    "unanimity",
    "truth_wins"
  )
  if (!scheme %in% valid_schemes) {
    stop(glue(
      "Invalid scheme. Must be one of: {paste(valid_schemes, collapse = ', ')}"
    ))
  }

  # generate all distinguishable distributions
  distributions <- generate_dist_distribs(group_size, n_alternatives)

  # create tibble with proper column names
  distributions_df <- as_tibble(distributions, .name_repair = "minimal")
  names(distributions_df) <- paste0("alt_", seq_len(n_alternatives))

  # apply decision rule to each distribution using vectorized approach
  decisions <- distributions_df |>
    rowwise() |>
    mutate(
      decision = list({
        comp <- c_across(everything())
        decision_vec <- numeric(n_alternatives)
        # convert to numeric vector for easier handling
        comp_vec <- as.numeric(comp)

        if (scheme == "majority") {
          max_support <- max(comp_vec)
          if (max_support > group_size / 2) {
            # clear majority winner
            decision_vec[which.max(comp_vec)] <- 1
          } else {
            # no majority fallback should be proportional, not equiprobability
            # aligns with Davis (1973) majority-proportionality sub-scheme
            if (sum(comp_vec) > 0) {
              decision_vec <- comp_vec / sum(comp_vec)
            } else {
              decision_vec <- rep(1 / n_alternatives, n_alternatives)
            }
          }
        } else if (scheme == "proportional") {
          # probability proportional to support
          total_support <- sum(comp_vec)
          if (total_support > 0) {
            decision_vec <- comp_vec / total_support
          } else {
            decision_vec <- rep(1 / n_alternatives, n_alternatives)
          }
        } else if (scheme == "equiprobability") {
          # equal probability among alternatives with any support
          supported <- which(comp_vec > 0)
          if (length(supported) > 0) {
            decision_vec[supported] <- 1 / length(supported)
          } else {
            decision_vec <- rep(1 / n_alternatives, n_alternatives)
          }
        } else if (scheme == "two_thirds") {
          max_support <- max(comp_vec)
          if (max_support >= (2 * group_size) / 3) {
            decision_vec[which.max(comp_vec)] <- 1
          } else {
            # fallback should be proportional per Davis theory
            if (sum(comp_vec) > 0) {
              decision_vec <- comp_vec / sum(comp_vec)
            } else {
              decision_vec <- rep(1 / n_alternatives, n_alternatives)
            }
          }
        } else if (scheme == "unanimity") {
          if (max(comp_vec) == group_size) {
            # unanimous support
            decision_vec[which.max(comp_vec)] <- 1
          } else {
            # Davis (1973) suggests hung jury for non-unanimous cases
            # represented as equiprobability among supported alternatives
            supported <- which(comp_vec > 0)
            if (length(supported) > 0) {
              decision_vec[supported] <- 1 / length(supported)
            } else {
              decision_vec <- rep(1 / n_alternatives, n_alternatives)
            }
          }
        } else if (scheme == "truth_wins") {
          # truth-wins should follow Lorge-Solomon Model A exactly
          # if truth (alt 1) has any support, it wins with probability 1
          if (comp_vec[1] > 0) {
            decision_vec[1] <- 1
          } else {
            # if no truth support, random choice among remaining
            remaining_support <- comp_vec[-1]
            if (sum(remaining_support) > 0) {
              decision_vec[-1] <- remaining_support / sum(remaining_support)
            } else {
              # all zero support - uniform across non-truth alternatives
              decision_vec[-1] <- 1 / (n_alternatives - 1)
            }
          }
        }
        decision_vec
      })
    ) |>
    ungroup() |>
    pull(decision)

  # convert list of decisions to matrix
  decision_matrix <- do.call(rbind, decisions)

  # ensure numerical stability: rows should sum to 1
  row_sums <- rowSums(decision_matrix)
  zero_rows <- row_sums == 0
  if (any(zero_rows)) {
    decision_matrix[zero_rows, ] <- 1 / n_alternatives
    row_sums[zero_rows] <- 1
  }
  decision_matrix <- decision_matrix / row_sums

  # store metadata as attributes
  attr(decision_matrix, "distributions") <- distributions
  attr(decision_matrix, "group_size") <- group_size
  attr(decision_matrix, "scheme") <- scheme
  attr(decision_matrix, "n_alternatives") <- n_alternatives

  decision_matrix
}

#' Generate all distinguishable preference distributions for r individuals and n
#' alternatives using composition enumeration
#' @param r Number of group members
#' @param n Number of alternatives
#' @return Matrix where each row is a distinguishable distribution
#' @keywords internal
generate_dist_distribs <- function(r, n) {
  # use recursive composition generation for accuracy
  generate_compositions_recurs <- function(
      remaining,
      positions,
      current = c()) {
    if (positions == 1) {
      return(list(c(current, remaining)))
    }

    compositions <- list()
    for (i in 0:remaining) {
      sub_compositions <- generate_compositions_recurs(
        remaining - i,
        positions - 1,
        c(current, i)
      )
      compositions <- c(compositions, sub_compositions)
    }
    compositions
  }

  # generate all compositions and convert to matrix
  compositions_list <- generate_compositions_recurs(r, n)
  do.call(rbind, compositions_list)
}

#' Calculate multinomial probability for a given distribution
#' @param distribution Vector representing number of individuals preferring each
#'        alternative
#' @param individual_prefs Vector of individual preference probabilities
#' @return Probability of this distribution occurring
#' @keywords internal
multinomial_probability <- function(distribution, individual_prefs) {
  r <- sum(distribution)

  # handle r=0 case according to multinomial theory
  if (r == 0) {
    # empty distrib has probability 1 for the degenerate case
    return(1)
  }

  # handle case where individual_prefs has zero elements
  # but distribution has positive counts
  if (any(individual_prefs == 0 & distribution > 0)) {
    return(0)
  }

  # multinomial coefficient: r! / (r1! * r2! * ... * rn!)
  coeff <- factorial(r) / prod(factorial(distribution))

  # multinomial probability: coeff * (p1^r1 * p2^r2 * ... * pn^rn)
  # handle 0^0 case properly: should equal 1
  prob_terms <- ifelse(
    distribution == 0 & individual_prefs == 0,
    1,
    individual_prefs^distribution
  )
  prob <- prod(prob_terms)

  coeff * prob
}
