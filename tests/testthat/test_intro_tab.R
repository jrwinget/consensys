box::use(
  testthat[
    expect_match,
    expect_true,
    expect_type,
    test_that
  ],
)

box::use(
  app/view/intro_tab,
)

test_that("intro_tab UI renders correctly", {
  ui <- intro_tab$ui("test")
  expect_type(ui, "list")
  expect_match(as.character(ui), "Quick Start Guide")
})

test_that("intro_tab server function exists", {
  expect_true("server" %in% names(intro_tab))
})
