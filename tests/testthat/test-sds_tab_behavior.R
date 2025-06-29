box::use(
  testthat[expect_equal, expect_true, expect_type, test_that],
  shiny[testServer]
)

box::use(
  app/view/sds_tab
)

test_that("SDS tab server initializes correctly", {
  testServer(sds_tab$server, {
    # Test that reactive values are initialized
    session$setInputs(num_alternatives = 3)
    session$flushReact()
    
    # Check that UI elements are rendered
    expect_true(!is.null(output$preference_sliders))
    
    # Check initial status
    expect_equal(output$model_status, "Ready")
  })
})

test_that("SDS tab only updates on button click", {
  testServer(sds_tab$server, {
    # Set initial inputs
    session$setInputs(
      num_alternatives = 3,
      sds_type = "majority"
    )
    session$flushReact()
    
    # Set preference values
    session$setInputs(
      pref_1 = 0.5,
      pref_2 = 0.3,
      pref_3 = 0.2
    )
    session$flushReact()
    
    # Model should not be updated yet (no button click)
    initial_status <- output$model_status
    
    # Click update button
    session$setInputs(update_sds = 1)
    session$flushReact()
    
    # Now model should be updated
    updated_status <- output$model_status
    expect_true(updated_status != initial_status)
  })
})

test_that("SDS tab handles scheme changes correctly", {
  testServer(sds_tab$server, {
    # Test different schemes
    schemes <- c("majority", "proportional", "unanimity")
    
    for (scheme in schemes) {
      session$setInputs(
        num_alternatives = 3,
        sds_type = scheme,
        update_sds = sample(1:100, 1)  # Trigger update
      )
      session$flushReact()
      
      # Should not error and should produce output
      expect_true(!is.null(output$decision_matrix_plot))
      expect_true(!is.null(output$outcome_plot))
    }
  })
})