context("domestic functions")

test_that(".shave", {
  x <- readLines("3parts.mod") %>% .shave()
  expect_length(x %>% grep("^ | $| {2,}", .), 0)
  expect_length(grep("[[:alnum:]]+", x), length(x))
})

test_that(".str2mtx", {
  x <- c("766 991", "704 1046")
  expect_true(x %>%  .str2mtx %>% is.matrix)
})

test_that(".mtx2str", {
  expect_true(matrix(1:12, 6, 2) %>% .mtx2str %>% is.character)
})

test_that(".str2df", {
  x <- c("plop plip", "plup 45", "plap p l o p") %>% .str2df()
  expect_true(is.data.frame(x) && ncol(x)==3)
})

test_that(".df2str", {
  x <- data.frame(plop=1, plip="ee 56 ff") %>% .df2str()
  expect_true(is.character(x))
  expect_length(x, 2)
})



test_that(".is.path", {
  expect_true("mini.mod" %>% .is.path)
  expect_false(c("mini.mod", "out.mod") %>% .is.path)
  expect_false("wrong_path.mod" %>% .is.path)
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
