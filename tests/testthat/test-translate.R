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
test_that("translate_Out", {
  import_coo("bot_lite.coo") %>% translate_Out() %>% .test_translate("Out")
  import_coo("bot_lite.coo") %>% translate_Opn() %>% .test_translate("Opn")
  import_coo("bot_lite.coo") %>% translate_Ldk() %>% .test_translate("Ldk")

})
