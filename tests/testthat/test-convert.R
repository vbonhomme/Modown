context("convert")

test_that("convert", {
  x <- list(coo=matrix(1:12, 6),
            cov=data.frame(plop=45, plip="ee 45 ff"))
  expect_true(convert(x$coo) %>% is.character)
  expect_true(convert(x$cov) %>% is.character)
  expect_true(convert(x) %>% is.character)
})

suppressPackageStartupMessages(library(Momocs))

test_that("convert_Coo1", {

  expect_silent(convert_Coo1(bot, 1) %>% inspect)
  x <- convert_Coo1(bot, 1) %>% import_coo1()
  expect_true(is_shp(x$coo))
  expect_true(is.data.frame(x$cov))

  expect_silent(convert_Coo1(olea, 1) %>% inspect)
  x <- convert_Coo1(olea, 1) %>% import_coo1()
  expect_true(is_shp(x$coo))
  expect_true(is.data.frame(x$cov))

  expect_silent(convert_Coo1(shapes, 4) %>% inspect)
  x <- convert_Coo1(shapes, 4) %>% import_coo1()
  expect_true(is_shp(x$coo))
  expect_true(is.data.frame(x$cov))
})

test_that("convert_Coo", {
  .test_convert_Coo <- function(x){
    expect_true(is.list(x$coo))
    expect_true(all(sapply(x$coo, is.matrix)))
    expect_true(is.data.frame(x$cov))
  }
  expect_silent(convert_Coo(bot) %>% inspect)
  convert_Coo(bot) %>% import_coo() %>% group %>%
    .test_convert_Coo()

  expect_silent(convert_Coo(olea) %>% inspect)
  convert_Coo(olea) %>% import_coo() %>% group %>%
    .test_convert_Coo()

  expect_silent(convert_Coo(shapes) %>% inspect)
  convert_Coo(shapes) %>% import_coo() %>% group %>%
    .test_convert_Coo()
})

