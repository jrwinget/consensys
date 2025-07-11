box::use(
  cli[cli_bullets, cli_h1, cli_h2, cli_h3],
)

box::use(
  app/logic/sjs_calculations[calculate_sjs_consensus, simulate_sjs_process],
)

cli_h1("SJS Decay Parameter Demonstration Script")

# initial positions with clear spread
initial_positions <- c(10, 30, 50, 70, 90)
n_rounds <- 8
self_weight <- 0.5

cli_h2("Simulation Parameters")

cli_bullets(c(
  "*" = "Initial positions: {initial_positions}",
  "*" = "Rounds: {n_rounds}",
  "*" = "Self weight: {self_weight}"
))

# set different decay parameters
decay_values <- c(0.5, 1.0, 2.0, 5.0)

cli_h2("Testing Decay Parameters")
for (decay in decay_values) {
  cli_h3("Decay Parameter: {decay}")

  result <- simulate_sjs_process(
    initial_positions,
    n_rounds = n_rounds,
    self_weight = self_weight,
    decay_parameter = decay
  )

  consensus <- calculate_sjs_consensus(result)
  final_positions <- result[nrow(result), ]

  cli_bullets(c(
    "*" = "Final positions: {final_positions}",
    "*" = "Variance reduction: {consensus$variance_reduction * 100}",
    "*" = "Range reduction: {consensus$range_reduction * 100}",
    "*" = "Final spread (SD): {sqrt(consensus$final_variance)}"
  ))
}

cli_h2("Summary of Results")

cli_h3("Lower decay parameter (e.g., 0.5):")
cli_bullets(c(
  "i" = "More influence from distant positions",
  "i" = "Generally leads to greater convergence",
  "i" = "Positions move more toward group center"
))

cli_h3("Higher decay parameter (e.g., 5.0):")
cli_bullets(c(
  "i" = "Less influence from distant positions",
  "i" = "Positions change more slowly",
  "i" = "May maintain more individual differences"
))
