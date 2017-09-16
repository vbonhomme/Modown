context("write")

test_that("write", {
  .test_write <- function(x){
    f_rm <- function(){
      if (file.exists("plop.mod"))
        silent <- file.remove("plop.mod")
    }

    f_rm()
    write(x, "plop.mod")
    expect_true(file.exists("plop.mod"))
    expect_error(write(x, file="plop.mod"))
    expect_silent(write(x, "plop.mod", force=TRUE))
    expect_true(file.exists("plop.mod"))
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
      if (file.exists("plop.mod"))
        silent <- file.remove("plop.mod")
    }

    f_rm()
    write_Coo1(x, id=2, file="plop.mod")
    expect_true(file.exists("plop.mod"))
    expect_error(write_Coo1(x, id=2, file="plop.mod"))
    expect_silent(write_Coo1(x, id=2, file="plop.mod", force=TRUE))
    expect_true(file.exists("plop.mod"))
    f_rm()
  }

  bot %>% .test_write_Coo1()
  olea %>% .test_write_Coo1()
  shapes %>% .test_write_Coo1()

  expect_silent(write_Coo1(bot, 1))
  expect_true(file.exists("brahma.mod"))
  silent <- file.remove("brahma.mod")

  expect_silent(write_Coo1(olea, 1))
  expect_true(file.exists("0023-cPicMa_O14VL.mod"))
  file.remove("0023-cPicMa_O14VL.mod")

  expect_silent(write_Coo1(shapes, 1))
  expect_true(file.exists("arrow.mod"))
  silent <- file.remove("arrow.mod")

})

test_that("write_Coo", {

  .test_write_Coo <- function(x){
    f_rm <- function(){
      if (file.exists("plop.mod"))
        silent <- file.remove("plop.mod")
    }

    f_rm()
    write_Coo(x, file="plop.mod")
    expect_true(file.exists("plop.mod"))
    expect_error(write_Coo(x, file="plop.mod"))
    expect_silent(write_Coo(x, file="plop.mod", force=TRUE))
    expect_true(file.exists("plop.mod"))
    f_rm()
  }

  bot %>% .test_write_Coo()
  olea %>% .test_write_Coo()
  shapes %>% .test_write_Coo()

  expect_silent(write_Coo(bot))
  expect_true(file.exists("bot.mod"))
  silent <- file.remove("bot.mod")

  olea2 <- olea
  expect_silent(write_Coo(olea2))
  expect_true(file.exists("olea2.mod"))
  silent <- file.remove("olea2.mod")

  expect_silent(write_Coo(olea2, file="olea2"))
  expect_true(file.exists("olea2.mod"))
  silent <- file.remove("olea2.mod")

  expect_silent(write_Coo(shapes))
  expect_true(file.exists("shapes.mod"))
  silent <- file.remove("shapes.mod")

})

test_that("write_Coo_separate", {
  bot_files <- c("brahma.mod",
                 "caney.mod",
                 "chimay.mod")
  olea_files <- c("0023-cPicMa_O14VL.mod",
                  "0023-cPicMa_O15VD.mod",
                  "0023-cPicMa_O15VL.mod")
  shapes_files <- c("arrow.mod",
                    "bone.mod",
                    "buttefly.mod",
                    "cat.mod",
                    "check.mod",
                    "cross.mod",
                    "dog.mod",
                    "fish.mod",
                    "hand.mod",
                    "hands.mod")
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

