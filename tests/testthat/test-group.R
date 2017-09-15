context("group")

test_that("group",{
  .test_group <- function(x){
    expect_true(is.list(x$coo))
    expect_true(all(sapply(x$coo, is.matrix)))
    expect_true(is.data.frame(x$cov))
  }

  coo1 <- list(coo=matrix(1:12, ncol=2),
               cov=data.frame())
  list(coo1, coo1) %>% group %>% .test_group

  coo2 <- list(coo=matrix(1:12, 2),
               cov=data.frame(plop=45, plip="ee 54"))
  list(coo2, coo2) %>% group %>% .test_group

  list(coo1, coo2) %>% group %>% .test_group

})

