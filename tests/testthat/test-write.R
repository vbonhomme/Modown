context("write")

.test_write <- function(x="plop.coo"){
  expect_true(file.exists(x))
  silent <- file.remove(x)
}

.test_write_stop <- function(x="plop.coo"){
  expect_error(file.exists(x))
  silent <- file.remove(x)
}

f_cr <- function()
  file.create("plop.coo")

f_rm <- function()
  file.remove("plop.coo")

# library(Momocs)
test_that("write", {
  matrix(1:12, ncol=2) %>% write("plop.coo")
  .test_write("plop.coo")
  .f_cr()
  matrix(1:12, ncol=2) %>% write("plop.coo")
  .f_rm()
})


