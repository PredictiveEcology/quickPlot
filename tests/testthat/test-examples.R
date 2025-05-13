test_that("all exported functions have examples", {
  skip_on_ci()
  skip_on_cran()
  exFiles <- normalizePath(dir("../../man", full.names = TRUE))
  tmpdir <- withr::local_tempdir()
  withr::local_dir(tmpdir)

  for (file in exFiles) {
    test_example(file)
  }
})
