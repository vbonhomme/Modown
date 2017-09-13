context("domestic functions")

test_that(".shave", {
  x <- readLines("3parts.coo") %>% .shave()
  expect_length(x %>% grep("^ | $| {2,}", .), 0)
  expect_length(grep("[[:alnum:]]+", x), length(x))
})

test_that(".str2mtx", {
  x <- c("766 991", "704 1046")
  expect_true(x %>%  .str2mtx %>% is.matrix)
})

test_that(".str2df", {
  x <- c("plop plip", "plup 45", "plap p l o p") %>% .str2df()
  expect_true(is.data.frame(x) && ncol(x)==3)
})

test_that(".is.path", {
  expect_true("mini.coo" %>% .is.path)
  expect_false(c("mini.coo", "out.coo") %>% .is.path)
  expect_false("wrong_path.coo" %>% .is.path)
})

test_that(".replace_na_with_last", {
  expect_false(c(1, NA, NA, 2, NA, NA) %>%
                 .replace_na_with_last() %>%
                 is.na() %>%
                 any())
})

test_that(".cuts_into_list", {
  expect_length(c("~plop", 1, 2, "~plip", 3, 4) %>%
                  .cuts_into_list("^~"), 2)
})



# .is.path()
# .str2df()
# .str2mtx()
# .replace_na_with_last()
# .cuts_into_list()
# test_that()
