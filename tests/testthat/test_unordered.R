library(blockForest)
library(survival)
context("blockForest_unordered")

test_that("Old parameters still work", {
  n <- 20
  dt <- data.frame(x = sample(c("A", "B", "C", "D"), n, replace = TRUE), 
                   y = rbinom(n, 1, 0.5), 
                   stringsAsFactors = FALSE)
  
  rf.false <- blockForest(y ~ ., data = dt, num.trees = 5, write.forest = TRUE, 
                     respect.unordered.factors = FALSE)
  rf.true <- blockForest(y ~ ., data = dt, num.trees = 5, write.forest = TRUE, 
                    respect.unordered.factors = TRUE)
  
  expect_null(rf.false$forest$covariate.levels)
  expect_equal(length(rf.true$forest$covariate.levels), 1)
})

test_that("If respect.unordered.factors='partition', regard characters as unordered", {
  n <- 20
  dt <- data.frame(x = sample(c("A", "B", "C", "D"), n, replace = TRUE), 
                   y = rbinom(n, 1, 0.5), 
                   stringsAsFactors = FALSE)
  
  set.seed(2)
  rf.char <- blockForest(y ~ ., data = dt, num.trees = 5, min.node.size = n/2, respect.unordered.factors = 'partition')
  
  dt$x <- factor(dt$x, ordered = FALSE)
  set.seed(2)
  rf.fac <- blockForest(y ~ ., data = dt, num.trees = 5, min.node.size = n/2, respect.unordered.factors = 'partition')
  
  expect_equal(rf.char$prediction.error, rf.fac$prediction.error)
})

test_that("If respect.unordered.factors='ignore', regard characters as ordered", {
  n <- 20
  dt <- data.frame(x = sample(c("A", "B", "C", "D"), n, replace = TRUE), 
                   y = rbinom(n, 1, 0.5), 
                   stringsAsFactors = FALSE)
  
  set.seed(2)
  rf.char <- blockForest(y ~ ., data = dt, num.trees = 5, min.node.size = n/2, respect.unordered.factors = 'ignore')
  
  dt$x <- factor(dt$x, ordered = FALSE)
  set.seed(2)
  rf.fac <- blockForest(y ~ ., data = dt, num.trees = 5, min.node.size = n/2, respect.unordered.factors = 'ignore')
  
  expect_equal(rf.char$prediction.error, rf.fac$prediction.error)
})

test_that("Error if other value for respect.unordered.factors", {
  expect_error(blockForest(y ~ ., iris, num.trees = 5, respect.unordered.factors = NULL))
})

test_that("Same results if no unordered factors", {
  set.seed(100)
  rf1 <- blockForest(Species ~ ., iris, num.trees = 5, respect.unordered.factors = 'ignore')
  set.seed(100)
  expect_warning(rf2 <- blockForest(Species ~ ., iris, num.trees = 5, respect.unordered.factors = 'order'))
  set.seed(100)
  rf3 <- blockForest(Species ~ ., iris, num.trees = 5, respect.unordered.factors = 'partition')
  
  expect_equal(rf1$confusion.matrix, 
               rf2$confusion.matrix)
  expect_equal(rf1$confusion.matrix, 
               rf3$confusion.matrix)
})

test_that("Unordered splitting working for classification", {
  n <- 20
  dt <- data.frame(x = sample(c("A", "B", "C", "D"), n, replace = TRUE), 
                   y = factor(rbinom(n, 1, 0.5)),
                   stringsAsFactors = FALSE)
  
  rf <- blockForest(y ~ ., data = dt, num.trees = 5, min.node.size = n/2, respect.unordered.factors = 'partition')
  expect_true(any(!rf$forest$is.ordered))
})

test_that("Unordered splitting working for probability", {
  n <- 20
  dt <- data.frame(x = sample(c("A", "B", "C", "D"), n, replace = TRUE), 
                   y = factor(rbinom(n, 1, 0.5)),
                   stringsAsFactors = FALSE)
  
  rf <- blockForest(y ~ ., data = dt, num.trees = 5, min.node.size = n/2, 
               respect.unordered.factors = 'partition', probability = TRUE)
  expect_true(any(!rf$forest$is.ordered))
})

test_that("Unordered splitting working for survival", {
  rf <- blockForest(Surv(time, status) ~ ., veteran, num.trees = 5, min.node.size = 50, respect.unordered.factors = 'partition')
  expect_true(any(!rf$forest$is.ordered))
})

test_that("Error if too many factors in 'partition' mode", {
  n <- 100
  dt <- data.frame(x = factor(1:100, ordered = FALSE),  
                   y = rbinom(n, 1, 0.5))
  
  expect_error(blockForest(y ~ ., data = dt, num.trees = 5, respect.unordered.factors = 'partition'))
})

test_that("Survival forest with 'order' mode works", {
  expect_warning(rf <- blockForest(Surv(time, status) ~ ., veteran, num.trees = 5, 
                              write.forest = TRUE, respect.unordered.factors = 'order'))
  expect_equal(sort(rf$forest$covariate.levels$celltype), 
               sort(levels(veteran$celltype)))
    
  pred <- predict(rf, veteran)
  expect_is(pred, "blockForest.prediction")
})

test_that("maxstat splitting not working with unordered factors", {
  expect_error(blockForest(Sepal.Length ~ ., iris, splitrule = "maxstat", respect.unordered.factors = "partition"))
  expect_error(blockForest(Surv(time, status) ~ ., veteran, splitrule = "maxstat", respect.unordered.factors = "partition"))
})

test_that("C splitting not working with unordered factors", {
  expect_error(blockForest(Surv(time, status) ~ ., veteran, splitrule = "C", respect.unordered.factors = "partition"))
})

test_that("Warning for survival, multiclass classification/probability and maxstat with 'order' mode", {
  expect_warning(blockForest(Surv(time, status) ~ ., veteran, num.trees = 5, 
                        respect.unordered.factors = 'order'))
  expect_warning(blockForest(Species ~ ., iris, num.trees = 5, 
                        respect.unordered.factors = 'order'))
  expect_warning(blockForest(Species ~ ., iris, num.trees = 5, probability = TRUE,
                        respect.unordered.factors = 'order'))
  expect_warning(blockForest(Sepal.Length ~ ., iris, num.trees = 5, splitrule = "maxstat",
                        respect.unordered.factors = 'order'))
})

test_that("No error if new levels in predict, 1 column", {
  set.seed(1)
  n <- 20
  train <- data.frame(x = sample(c("A", "B", "C"), n, replace = TRUE), 
                   y = rbinom(n, 1, 0.5), 
                   stringsAsFactors = FALSE)
  
  test <- data.frame(x = sample(c("A", "B", "C", "D"), n, replace = TRUE), 
                      y = rbinom(n, 1, 0.5), 
                      stringsAsFactors = FALSE)
  
  ## ignore
  rf.ignore <- blockForest(y ~ ., data = train, num.trees = 5, respect.unordered.factors = 'ignore')
  expect_silent(predict(rf.ignore, test))

  ## partition  
  rf.partition <- blockForest(y ~ ., data = train, num.trees = 5, respect.unordered.factors = 'partition')
  expect_silent(predict(rf.partition, test))
  
  ## order
  rf.order <- blockForest(y ~ ., data = train, num.trees = 5, respect.unordered.factors = 'order')
  expect_silent(predict(rf.order, test))
})

test_that("No error if new levels in predict, 2 columns", {
  set.seed(1)
  n <- 20
  train <- data.frame(x1 = sample(c("A", "B", "C"), n, replace = TRUE), 
                      x2 = sample(c("A", "B", "C"), n, replace = TRUE),
                      y = rbinom(n, 1, 0.5), 
                      stringsAsFactors = FALSE)
  
  test <- data.frame(x1 = sample(c("A", "B", "C", "D"), n, replace = TRUE), 
                     x2 = sample(c("A", "B", "C", "D"), n, replace = TRUE), 
                     y = rbinom(n, 1, 0.5), 
                     stringsAsFactors = FALSE)
  
  ## ignore
  rf.ignore <- blockForest(y ~ ., data = train, num.trees = 5, respect.unordered.factors = 'ignore')
  expect_silent(predict(rf.ignore, test))
  
  ## partition  
  rf.partition <- blockForest(y ~ ., data = train, num.trees = 5, respect.unordered.factors = 'partition')
  expect_silent(predict(rf.partition, test))
  
  ## order
  rf.order <- blockForest(y ~ ., data = train, num.trees = 5, respect.unordered.factors = 'order')
  expect_silent(predict(rf.order, test))
})
