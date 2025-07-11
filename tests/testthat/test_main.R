box::use(
  shiny[testServer],
  testthat[expect_match, expect_s3_class, expect_true, expect_type, test_that],
)

box::use(
  app/main,
)

test_that("main UI structure is correct", {
  ui <- main$ui("test")
  expect_type(ui, "list")
  expect_s3_class(ui, "bslib_page")
  expect_match(as.character(ui), "Introduction")
})

test_that("main server function can be initialized", {
  testServer(main$server, args = list(id = "test"), {
    expect_true(TRUE)
  })
})
