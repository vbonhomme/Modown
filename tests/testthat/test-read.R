context("read")

test_that("read_mod1", {
  x <- read_mod1("3parts.mod")
  expect_true(is.list(x))
  expect_true(all(c("coo", "cov") %in% names(x)))
  expect_true(is.list(x$coo))
  expect_true(is.data.frame(x$cov))

  x <- read_mod1("out.mod")
  expect_true(is.list(x))
  expect_true(all(c("coo", "cov") %in% names(x)))
  expect_true(is.matrix(x$coo))
  expect_true(is.data.frame(x$cov))
})

test_that("read_mod", {
  x <- read_mod("olea.mod")
  expect_true(is.list(x))
  expect_true(all(sapply(x, function(.) is.matrix(.$coo))))
  expect_true(all(sapply(x, function(.) is.data.frame(.$cov))))
  expect_true(is.data.frame(do.call("rbind", lapply(x, function(.) .$cov))))
})

