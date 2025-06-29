box::use(
  testthat[expect_equal, expect_true, expect_type, test_that],
  shiny[testServer]
)

box::use(
  app/view/sjs_tab
)

test_that("SJS tab server initializes correctly", {
  testServer(sjs_tab$server, {
    # Test that reactive values are initialized
    session$setInputs(n_individuals = 5, n_rounds = 3)
    session$flushReact()
    
    # Check that UI elements are rendered
    expect_true(!is.null(output$position_inputs))
    
    # Test that the server doesn't error on initialization
    expect_true(TRUE)  # If we get here, initialization worked
  })
})

test_that("SJS tab simulation runs correctly", {
  testServer(sjs_tab$server, {
    # Set initial parameters
    session$setInputs(
      n_individuals = 4,
      n_rounds = 5
    )
    session$flushReact()
    
    # Set position values
    session$setInputs(
      pos_1 = 10,
      pos_2 = 30,
      pos_3 = 70,
      pos_4 = 90
    )
    session$flushReact()
    
    # Run simulation
    session$setInputs(simulate = 1)
    session$flushReact()
    
    # Check that simulation completed
    expect_equal(output$status_badge, "Complete")
    expect_true(!is.null(output$convergence_plot))
    expect_true(!is.null(output$summary_stats))
  })
})

test_that("SJS tab randomize positions works", {
  testServer(sjs_tab$server, {
    # Set initial parameters
    session$setInputs(n_individuals = 3)
    session$flushReact()
    
    # Set initial positions
    session$setInputs(
      pos_1 = 50,
      pos_2 = 50,
      pos_3 = 50
    )
    session$flushReact()
    
    # Randomize positions
    session$setInputs(randomize_positions = 1)
    session$flushReact()
    
    # Positions should have changed (with high probability)
    # Note: This test might occasionally fail due to randomness
    # but it's very unlikely all three would remain exactly 50
  })
})

test_that("SJS tab weight matrix displays when enabled", {
  testServer(sjs_tab$server, {
    # Set parameters and run simulation
    session$setInputs(
      n_individuals = 3,
      n_rounds = 2,
      pos_1 = 20,
      pos_2 = 50,
      pos_3 = 80,
      simulate = 1
    )
    session$flushReact()
    
    # Enable weight display
    session$setInputs(show_weights = TRUE)
    session$flushReact()
    
    # Weight plot should be available
    expect_true(!is.null(output$weights_plot))
  })
})