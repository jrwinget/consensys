box::use(
  testthat[
    expect_equal,
    expect_true,
    expect_type,
    expect_match,
    test_that
  ],
)

box::use(
  app/view/intro_tab,
  app/main,
)

test_that("intro_tab UI renders correctly", {
  ui <- intro_tab$ui("test")
  expect_type(ui, "list")
  expect_match(as.character(ui), "Quick Start Guide")
})

test_that("intro_tab server function exists", {
  expect_true("server" %in% names(intro_tab))
})
