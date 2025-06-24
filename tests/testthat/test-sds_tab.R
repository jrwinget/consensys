box::use(
  shiny[testServer],
  testthat[expect_true, expect_type, test_that],
)

box::use(
  app/view/sds_tab,
)

test_that("sds tab UI renders", {
  ui <- sds_tab$ui("test")
  expect_type(ui, "list")
})

test_that("sds tab server initializes", {
  testServer(sds_tab$server, args = list(id = "test"), {
    expect_true(TRUE)
  })
})
