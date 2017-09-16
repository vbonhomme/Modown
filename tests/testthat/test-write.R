context("write")



# library(Momocs)
test_that("write", {
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

  matrix(1:12, ncol=2) %>% .test_write
})

load("bot.rda")
load("olea.rda")
load("shapes.rda")

test_that("write_Coo1", {

  .test_write_Coo1 <- function(x){
    f_rm <- function(){
      if (file.exists("plop.coo"))
        silent <- file.remove("plop.coo")
    }

    f_rm()
    write_Coo1(x, id=2, file="plop.coo")
    expect_true(file.exists("plop.coo"))
    expect_error(write_Coo1(x, id=2, file="plop.coo"))
    expect_silent(write_Coo1(x, id=2, file="plop.coo", force=TRUE))
    expect_true(file.exists("plop.coo"))
    f_rm()
  }

  bot %>% .test_write_Coo1()
  olea %>% .test_write_Coo1()
  shapes %>% .test_write_Coo1()

})

test_that("write_Coo", {

  .test_write_Coo <- function(x){
    f_rm <- function(){
      if (file.exists("plop.coo"))
        silent <- file.remove("plop.coo")
    }

    f_rm()
    write_Coo(x, id=2, file="plop.coo")
    expect_true(file.exists("plop.coo"))
    expect_error(write_Coo(x, id=2, file="plop.coo"))
    expect_silent(write_Coo(x, id=2, file="plop.coo", force=TRUE))
    expect_true(file.exists("plop.coo"))
    f_rm()
  }

  bot %>% .test_write_Coo()
  olea %>% .test_write_Coo()
  shapes %>% .test_write_Coo()

})
