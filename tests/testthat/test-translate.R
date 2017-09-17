context("translate")

load("bot.rda"); bot$cov <- bot$fac
load("olea.rda"); olea$cov <- olea$fac
load("shapes.rda"); shapes$cov <- shapes$fac

.test_translate <- function(x, class){
  expect_true(is.list(x$coo))
  expect_true(is.data.frame(x$fac))
  expect_true(class %in% class(x))
  expect_true("Coo" %in% class(x))
}

test_that("translate", {
  read_mod("bot_lite.mod") %>% translate_Out() %>% .test_translate("Out")
  read_mod("bot_lite.mod") %>% translate_Opn() %>% .test_translate("Opn")
  read_mod("bot_lite.mod") %>% translate_Ldk() %>% .test_translate("Ldk")
})
