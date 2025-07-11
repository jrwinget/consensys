box::use(
  shiny[testServer],
  testthat[
    expect_true,
    expect_type,
    expect_match,
    test_that
  ],
)

box::use(
  app/main,
)

test_that("main UI includes walkthrough overlay", {
  ui <- main$ui("test")
  expect_type(ui, "list")
  expect_match(as.character(ui), "walkthrough-overlay")
  expect_match(as.character(ui), "walkthrough-tooltip")
})

test_that("walkthrough steps are defined correctly", {
  # Test that walkthrough_steps exists in the main.R environment
  expect_true(exists("walkthrough_steps", envir = environment(main$ui)))
})

test_that("main app loads with walkthrough integration", {
  testServer(main$server, args = list(id = "test"), {
    # Test that main app initializes without errors
    expect_true(TRUE)
  })
})