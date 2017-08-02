test_that("all exported functions have examples", {
  fns <- ls("package:quickPlot")
  omit <- which(fns %in% c("gpar"))
  sapply(fns[-omit], function(x) {
    expect_warning(example(x, package = "quickPlot", character.only = TRUE,
                           echo = FALSE), NA)
  })
})

test_that("check all examples", {
  test_examples(path = "../../man")
})
