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
    expect_error(write(x, file="plop.coo"))
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

  expect_silent(write_Coo1(bot, 1))
  expect_true(file.exists("brahma.coo"))
  silent <- file.remove("brahma.coo")

  expect_silent(write_Coo1(olea, 1))
  expect_true(file.exists("0023-cPicMa_O14VL.coo"))
  file.remove("0023-cPicMa_O14VL.coo")

  expect_silent(write_Coo1(shapes, 1))
  expect_true(file.exists("arrow.coo"))
  silent <- file.remove("arrow.coo")

})

test_that("write_Coo", {

  .test_write_Coo <- function(x){
    f_rm <- function(){
      if (file.exists("plop.coo"))
        silent <- file.remove("plop.coo")
    }

    f_rm()
    write_Coo(x, file="plop.coo")
    expect_true(file.exists("plop.coo"))
    expect_error(write_Coo(x, file="plop.coo"))
    expect_silent(write_Coo(x, file="plop.coo", force=TRUE))
    expect_true(file.exists("plop.coo"))
    f_rm()
  }

  bot %>% .test_write_Coo()
  olea %>% .test_write_Coo()
  shapes %>% .test_write_Coo()

  expect_silent(write_Coo(bot))
  expect_true(file.exists("bot.coo"))
  silent <- file.remove("bot.coo")

  olea2 <- olea
  expect_silent(write_Coo(olea2))
  expect_true(file.exists("olea2.coo"))
  silent <- file.remove("olea2.coo")

  expect_silent(write_Coo(shapes))
  expect_true(file.exists("shapes.coo"))
  silent <- file.remove("shapes.coo")

})

test_that("write_Coo_separate", {
  bot_files <- c("brahma.coo",
                 "caney.coo",
                 "chimay.coo")
  olea_files <- c("0023-cPicMa_O14VL.coo",
                  "0023-cPicMa_O15VD.coo",
                  "0023-cPicMa_O15VL.coo")
  shapes_files <- c("arrow.coo",
                    "bone.coo",
                    "buttefly.coo",
                    "cat.coo",
                    "check.coo",
                    "cross.coo",
                    "dog.coo",
                    "fish.coo",
                    "hand.coo",
                    "hands.coo")
  .test_write_Coo_separate <- function(x, x_files){
    f_rm <- function(files){
      lapply(x_files,
             function(.) {
               if (file.exists(.))
                 silent <- file.remove(.)
             })
    }

    f_rm(x_files)
    write_Coo(x, separate = TRUE)
    expect_true(all(file.exists(x_files)))
    expect_error(write_Coo(x, separate = TRUE))
    expect_silent(write_Coo(x, separate = TRUE, force=TRUE))
    expect_true(all(file.exists(x_files)))
    f_rm(x_files)
  }

  bot %>% .test_write_Coo_separate(bot_files)
  olea %>% .test_write_Coo_separate(olea_files)
  shapes %>% .test_write_Coo_separate(shapes_files)

})

