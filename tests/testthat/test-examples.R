test_that("all exported functions have examples", {
  sapply(ls("package:quickPlot"), function(x) {
    expect_warning(example(x, package = "quickPlot", character.only = TRUE), NA)
  })
})

test_that("check all examples", {
  test_examples(path = "../../man")
})
