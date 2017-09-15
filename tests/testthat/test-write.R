context("write")

.test_write <- function(x){
  f_rm <- function(){
    if (file.exists("plop.coo"))
      silent <- file.remove("plop.coo")
  }

  f_rm()
  write(x, "plop.coo")
  expect_true(file.exists("plop.coo"))
  expect_error(write(x))
  expect_silent(write(x, "plop.coo", force=TRUE))
  expect_true(file.exists("plop.coo"))
  f_rm()

}

# library(Momocs)
test_that("write", {
  matrix(1:12, ncol=2) %>% .test_write
})


