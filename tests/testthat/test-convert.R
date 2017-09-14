context("convert")

test_that("convert", {
  x <- list(coo=matrix(1:12, 6),
            cov=data.frame(plop=45, plip="ee 45 ff"))
  expect_true(convert(x$coo) %>% is.character)
  expect_true(convert(x$cov) %>% is.character)
  expect_true(convert(x) %>% is.character)
})
