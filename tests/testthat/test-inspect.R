context("inspect")

test_that("inspect", {
  expect_message(inspect(c("~plop", "~ ")),
                 "names .* filled")
  expect_message(inspect(c("~plop", "~plop")),
                 "names .* duplicated")
  expect_message(inspect(c("~plop", "foo 45",
                            "~plip", "~plup")),
                 "cov .* everywhere")
  expect_message(inspect(c("~plop", "foo 45",
                            "~plip", "foo 54", "foo2 12")),
                 "cov .* length")
  expect_message(inspect(c("~plop", "foo 45",
                            "~plip", "foo 54", "foo2 12")),
                 "cov .* homogeneous")
  expect_message(inspect(c(" ~plip")),
                 "leading spaces")
  expect_message(inspect(c(" ~plip ")),
                 "trailing spaces")
  expect_message(inspect(c("~plip", "45    123")),
                 "multiple spaces")
})
