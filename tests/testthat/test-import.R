context("import")

test_that("import_coo1 works fine", {
  x <- import_coo1("3parts.coo")
  expect_true(is.list(x))
  expect_true(all(c("coo", "cov") %in% names(x)))
})
