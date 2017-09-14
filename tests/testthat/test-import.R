context("import")

test_that("import_coo1 works fine", {
  x <- import_coo1("3parts.coo")
  expect_true(is.list(x))
  expect_true(all(c("coo", "cov") %in% names(x)))
  expect_true(is.list(x$coo))
  expect_true(is.data.frame(x$cov))

  x <- import_coo1("out.coo")
  expect_true(is.list(x))
  expect_true(all(c("coo", "cov") %in% names(x)))
  expect_true(is.matrix(x$coo))
  expect_true(is.data.frame(x$cov))
})

test_that("import_coo", {
  x <- import_coo("olea.coo")
  expect_true(is.list(x))
  expect_true(all(sapply(x, function(.) is.matrix(.$coo))))
  expect_true(all(sapply(x, function(.) is.data.frame(.$cov))))
  expect_true(is.data.frame(do.call("rbind", lapply(x, function(.) .$cov))))
})

