## Tests for print function

library(blockForest)
context("blockForest_print")

## Initialize the random forest
rf <- blockForest(Species ~ ., iris, num.trees = 5, write.forest = TRUE)

## Test print blockForest function
expect_that(print(rf), prints_text("blockForest result"))

## Test print forest function
expect_that(print(rf$forest), prints_text("blockForest forest object"))

## Test print prediction function
expect_that(print(predict(rf, iris)), prints_text("blockForest prediction"))

## Test str blockForest function
expect_that(str(rf), prints_text("List of 14"))

## Test str forest function
expect_that(str(rf$forest), prints_text("List of 10"))
