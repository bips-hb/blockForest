library(blockForest)
library(survival)
context("blockForest_maxstat")

test_that("maxstat splitting works for survival", {
  rf <- blockForest(Surv(time, status) ~ ., veteran, splitrule = "maxstat")
  expect_is(rf, "blockForest")
  expect_lt(rf$prediction.error, 0.4)
})

test_that("maxstat splitting works for regression", {
  rf <- blockForest(Sepal.Length ~ ., iris, splitrule = "maxstat")
  expect_is(rf, "blockForest")
  expect_gt(rf$r.squared, 0.5)
})

test_that("maxstat splitting, alpha or minprop out of range throws error", {
  expect_error(blockForest(Surv(time, status) ~ ., veteran, splitrule = "maxstat", alpha = -1))
  expect_error(blockForest(Surv(time, status) ~ ., veteran, splitrule = "maxstat", alpha = 2))
  expect_error(blockForest(Surv(time, status) ~ ., veteran, splitrule = "maxstat", minprop = -1))
  expect_error(blockForest(Surv(time, status) ~ ., veteran, splitrule = "maxstat", minprop = 1))
})

test_that("maxstat splitting not working for classification", {
  expect_error(blockForest(Species ~ ., iris, splitrule = "maxstat"))
})