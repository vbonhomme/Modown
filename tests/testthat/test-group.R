context("group")

test_that("group",{
  .test_group <- function(x){
    expect_true(is.list(x$coo))
    expect_true(all(sapply(x$coo, is.matrix)))
    expect_true(is.data.frame(x$cov))
  }

  mod1 <- list(coo=matrix(1:12, ncol=2),
               cov=data.frame())
  list(mod1, mod1) %>% group %>% .test_group

  mod2 <- list(coo=matrix(1:12, 2),
               cov=data.frame(plop=45, plip="ee 54"))
  list(mod2, mod2) %>% group %>% .test_group

  list(mod1, mod2) %>% group %>% .test_group

})

