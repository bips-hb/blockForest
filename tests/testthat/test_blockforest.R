library(blockForest)
library(survival)
context("blockForest_blockforest")

test_that("Block forest runs without error", {
  expect_silent(blockForest(Species ~ ., iris, num.trees = 5, 
                       blocks = list(1:2, 3:4), 
                       block.weights = c(0.5, 0.5),
                       mtry = c(1, 2)))
  expect_silent(blockForest(Species ~ ., iris, num.trees = 5, probability = TRUE,
                       blocks = list(1:2, 3:4), 
                       block.weights = c(0.5, 0.5),
                       mtry = c(1, 2)))
  expect_silent(blockForest(Sepal.Length ~ ., iris, num.trees = 5, 
                       blocks = list(1:2, 3:4), 
                       block.weights = c(0.5, 0.5),
                       mtry = c(1, 2)))
  expect_silent(blockForest(Surv(time, status) ~ ., veteran, num.trees = 5, 
                       blocks = list(1:2, 3:4), 
                       block.weights = c(0.5, 0.5),
                       mtry = c(1, 2)))
})

test_that("Error if blocks and mtry are of different size", {
  expect_error(blockForest(Species ~ ., iris, num.trees = 5, 
                       blocks = list(1:2, 3:4), 
                       mtry = c(1, 2, 3)), 
               "Error: Length of 'blocks' and 'mtry' arguments not matching.")
})

test_that("Error if blocks and weights are of different size", {
  expect_error(blockForest(Species ~ ., iris, num.trees = 5, 
                      blocks = list(1:2, 3:4), 
                      block.weights = c(0.5, 0.5, 0.5), 
                      mtry = c(1, 2)), 
               "Error: Length of 'blocks' and 'block.weights' arguments not matching.")
})

test_that("Error if blocks is no list of numeric vectors", {
  expect_error(blockForest(Species ~ ., iris, num.trees = 5, 
                      blocks = list(1:2, list(1, 2), "A"), 
                      mtry = c(1, 2, 3)), 
               "Error: The 'blocks' argument is no list of numeric vectors.")
})

test_that("Error if blocks elements are not integers", {
  expect_error(blockForest(Species ~ ., iris, num.trees = 5, 
                      blocks = list(1:2, c(3.5, 1.2), 4:5), 
                      block.weights = c(.5, .5, .5),
                      mtry = c(1, 2, 3)), 
               "Error: The 'blocks' argument contains non-integers.")
})

test_that("Error if blocks elements are too large", {
  expect_error(blockForest(Species ~ ., iris, num.trees = 5, 
                      blocks = list(1:2, 5), 
                      block.weights = c(.5, .5),
                      mtry = c(2, 2)), 
               "Error: The 'blocks' argument contains variable indices not present in the data.")
  expect_error(blockForest(Species ~ ., iris, num.trees = 5, 
                      blocks = list(1:2, 3:4, 5:6), 
                      block.weights = c(.5, .5, .5),
                      mtry = c(1, 2, 3)), 
               "Error: The 'blocks' argument contains variable indices not present in the data.") 
  expect_error(blockForest(Surv(time, status) ~ ., veteran, num.trees = 5, 
                      blocks = list(1:2, 3:4, 7), 
                      block.weights = c(.5, .5, .5),
                      mtry = c(1, 2, 1)), 
               "Error: The 'blocks' argument contains variable indices not present in the data.")
})

test_that("A block with mtry=0 is not selected", {
  rf <- blockForest(Species ~ ., iris, num.trees = 5, 
               blocks = list(1:2, 3:4), 
               block.weights = c(.5, .5),
               mtry = c(0, 1))
  split_vars <- sort(unique(unlist(rf$forest$split.varIDs)))
  expect_true(all(split_vars %in% c(0, 3, 4)))
})

test_that("A block with weight=0 is not selected", {
  rf <- blockForest(Species ~ ., iris, num.trees = 5, 
               blocks = list(1:2, 3:4), 
               block.weights = c(0, 1),
               mtry = c(1, 2))
  split_vars <- sort(unique(unlist(rf$forest$split.varIDs)))
  expect_true(all(split_vars %in% c(0, 3, 4)))
})

test_that("Different block coding for survival", {
  rf <- blockForest(Surv(time, status) ~ ., veteran, num.trees = 5, 
               blocks = list(1:2, 3:4, 5:6), 
               block.weights = c(.5, .5, .5),
               mtry = c(1, 1, 2))
  split_vars <- sort(unique(unlist(rf$forest$split.varIDs)))
  expect_equal(split_vars, c(0, 2:7))
})

test_that("Tree-wise block weights are accepted", {
  expect_silent(blockForest(Species ~ ., iris, num.trees = 2,
                       blocks = list(1:2, 3:4),
                       block.weights = list(c(0, 1), c(0.5, 0.5)),
                       mtry = c(1, 2)))
})

test_that("Tree-wise block weights error if number missmatch", {
  expect_error(blockForest(Species ~ ., iris, num.trees = 5,
                      blocks = list(1:2, 3:4),
                      block.weights = list(c(0, 1), c(0.5, 0.5)),
                      mtry = c(1, 2)), 
               "Error: Length of 'block.weights' not matching number of trees.")
})
test_that("Same result with alternative interface", {
  set.seed(42)
  rf1 <- blockForest(Species ~ ., iris, num.trees = 5, 
                     blocks = list(1:2, 3:4), 
                     block.weights = c(.5, .5),
                     mtry = c(1,1))
  set.seed(42)
  rf2 <- blockForest(dependent.variable.name = "Species", data = iris[, c(5, 1:4)], num.trees = 5, 
                     blocks = list(1:2, 3:4), 
                     block.weights = c(.5, .5),
                     mtry = c(1,1))
  set.seed(42)
  rf3 <- blockForest(dependent.variable.name = "Species", data = iris, num.trees = 5, 
                     blocks = list(1:2, 3:4), 
                     block.weights = c(.5, .5),
                     mtry = c(1,1))
  expect_equal(rf1$prediction.error, rf2$prediction.error)
  expect_equal(rf1$predictions, rf2$predictions)
  expect_equal(rf1$prediction.error, rf3$prediction.error)
  expect_equal(rf1$predictions, rf3$predictions)
})

