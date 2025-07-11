box::use(
  shiny[testServer],
  testthat[
    expect_equal,
    expect_true,
    expect_type,
    test_that
  ],
)

box::use(
  app/logic/sjs_calculations[simulate_sjs_process],
  app/main,
)

test_that("SJS decay parameter is properly integrated in UI", {
  testServer(main$server, {
    session$setInputs(
      model_type = "sjs",
      group_size = 3,
      n_rounds = 5,
      self_weight = 0.6,
      decay_parameter = 2.0,
      pos_1 = 20,
      pos_2 = 50,
      pos_3 = 80
    )

    session$setInputs(run_sjs = 1)

    expect_equal(input$decay_parameter, 2.0)
    expect_equal(input$self_weight, 0.6)
    expect_equal(input$group_size, 3)
  })
})

test_that("SJS simulation with different decay parameters produces different results", {
  positions <- c(10, 50, 90)

  result_low_decay <- simulate_sjs_process(
    positions,
    n_rounds = 5,
    self_weight = 0.5,
    decay_parameter = 0.5
  )

  result_high_decay <- simulate_sjs_process(
    positions,
    n_rounds = 5,
    self_weight = 0.5,
    decay_parameter = 3.0
  )

  expect_type(result_low_decay, "double")
  expect_type(result_high_decay, "double")
  expect_true(is.matrix(result_low_decay))
  expect_true(is.matrix(result_high_decay))

  expect_true(!identical(result_low_decay, result_high_decay))

  expect_equal(attr(result_low_decay, "decay_parameter"), 0.5)
  expect_equal(attr(result_high_decay, "decay_parameter"), 3.0)
})
