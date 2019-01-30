library(blockForest)
library(survival)
context("blockForest_blockfor")

# Generate data:
################

# Covariate matrix:
X <- cbind(matrix(nrow=40, ncol=5, data=rnorm(40*5)), 
           matrix(nrow=40, ncol=30, data=rnorm(40*30, mean=1, sd=2)),
           matrix(nrow=40, ncol=100, data=rnorm(40*100, mean=2, sd=3)))

# Block variable (list):
blocks <- rep(1:3, times=c(5, 30, 100))
blocks <- lapply(1:3, function(x) which(blocks==x))

# Binary outcome:
ybin <- factor(sample(c(0,1), size=40, replace=TRUE), levels=c(0,1))

# Survival outcome:
ysurv <- cbind(rnorm(40), sample(c(0,1), size=40, replace=TRUE))

test_that("Split rule parameter is used in blockForest", {
  blockforobj <- blockfor(X, ybin, num.trees = 5, replace = TRUE, blocks = blocks,
                          nsets = 2, num.trees.pre = 3, 
                          block.method = "BlockForest")
  expect_equal(blockforobj$forest$splitrule, "extratrees")
  
  blockforobj <- blockfor(X, ysurv, num.trees = 5, replace = TRUE, blocks = blocks,
                          nsets = 2, num.trees.pre = 3, splitrule = "maxstat", 
                          block.method = "BlockForest")
  expect_equal(blockforobj$forest$splitrule, "maxstat")
})

