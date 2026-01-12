# Prediction using Random Forest variants for block-structured covariate data

This function is to be applied to the entry 'forest' of the output of
[`blockfor`](https://bips-hb.github.io/blockForest/reference/blockfor.md).
See the example section for illustration.

## Usage

``` r
# S3 method for class 'blockForest'
predict(
  object,
  data = NULL,
  predict.all = FALSE,
  num.trees = object$num.trees,
  type = "response",
  se.method = "infjack",
  quantiles = c(0.1, 0.5, 0.9),
  seed = NULL,
  num.threads = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- object:

  `blockForest` object.

- data:

  New test data of class `data.frame` or `gwaa.data` (GenABEL).

- predict.all:

  Return individual predictions for each tree instead of aggregated
  predictions for all trees. Return a matrix (sample x tree) for
  classification and regression, a 3d array for probability estimation
  (sample x class x tree) and survival (sample x time x tree).

- num.trees:

  Number of trees used for prediction. The first `num.trees` in the
  forest are used.

- type:

  Type of prediction. One of 'response', 'se', 'terminalNodes',
  'quantiles' with default 'response'. See below for details.

- se.method:

  Method to compute standard errors. One of 'jack', 'infjack' with
  default 'infjack'. Only applicable if type = 'se'. See below for
  details.

- quantiles:

  Vector of quantiles for quantile prediction. Set `type = 'quantiles'`
  to use.

- seed:

  Random seed. Default is `NULL`, which generates the seed from `R`. Set
  to `0` to ignore the `R` seed. The seed is used in case of ties in
  classification mode.

- num.threads:

  Number of threads. Default is number of CPUs available.

- verbose:

  Verbose output on or off.

- ...:

  further arguments passed to or from other methods.

## Value

Object of class `blockForest.prediction` with elements

|                             |                                                                           |
|-----------------------------|---------------------------------------------------------------------------|
| `predictions`               | Predicted classes/values (only for classification and regression)         |
| `unique.death.times`        | Unique death times (only for survival).                                   |
| `chf`                       | Estimated cumulative hazard function for each sample (only for survival). |
| `survival`                  | Estimated survival function for each sample (only for survival).          |
| `num.trees`                 | Number of trees.                                                          |
| `num.independent.variables` | Number of independent variables.                                          |
| `treetype`                  | Type of forest/tree. Classification, regression or survival.              |
| `num.samples`               | Number of samples.                                                        |

## Details

For `type = 'response'` (the default), the predicted classes
(classification), predicted numeric values (regression), predicted
probabilities (probability estimation) or survival probabilities
(survival) are returned. For `type = 'se'`, the standard error of the
predictions are returned (regression only). The
jackknife-after-bootstrap or infinitesimal jackknife for bagging is used
to estimate the standard errors based on out-of-bag predictions. See
Wager et al. (2014) for details. For `type = 'terminalNodes'`, the IDs
of the terminal node in each tree for each observation in the given
dataset are returned. For `type = 'quantiles'`, the selected quantiles
for each observation are estimated. See Meinshausen (2006) for details.

If `type = 'se'` is selected, the method to estimate the variances can
be chosen with `se.method`. Set `se.method = 'jack'` for
jackknife-after-bootstrap and `se.method = 'infjack'` for the
infinitesimal jackknife for bagging.

For classification and `predict.all = TRUE`, a factor levels are
returned as numerics. To retrieve the corresponding factor levels, use
`rf$forest$levels`, if `rf` is the ranger object.

## References

- Wright, M. N. & Ziegler, A. (2017). ranger: A Fast Implementation of
  Random Forests for High Dimensional Data in C++ and R. J Stat Softw
  77:1-17.
  [doi:10.18637/jss.v077.i01](https://doi.org/10.18637/jss.v077.i01) .

- Wager, S., Hastie T., & Efron, B. (2014). Confidence Intervals for
  Random Forests: The Jackknife and the Infinitesimal Jackknife. J Mach
  Learn Res 15:1625-1651. <https://jmlr.org/papers/v15/wager14a.html>.

- Meinshausen (2006). Quantile Regression Forests. J Mach Learn Res
  7:983-999. <https://www.jmlr.org/papers/v7/meinshausen06a.html>.

## See also

[`blockForest`](https://bips-hb.github.io/blockForest/reference/blockForest.md)

## Author

Marvin N. Wright

## Examples

``` r
# NOTE: There is no association between covariates and response for the
# simulated data below.
# Moreover, the input parameters of blockfor() are highly unrealistic
# (e.g., nsets = 10 is specified much too small).
# The purpose of the shown examples is merely to illustrate the
# application of predict.blockForest().


# Generate data:
################

set.seed(1234)

# Covariate matrix:
X <- cbind(matrix(nrow=40, ncol=5, data=rnorm(40*5)), 
           matrix(nrow=40, ncol=30, data=rnorm(40*30, mean=1, sd=2)),
           matrix(nrow=40, ncol=100, data=rnorm(40*100, mean=2, sd=3)))
colnames(X) <- paste("X", 1:ncol(X), sep="")

# Block variable (list):
block <- rep(1:3, times=c(5, 30, 100))
block <- lapply(1:3, function(x) which(block==x))

# Binary outcome:
ybin <- factor(sample(c(0,1), size=40, replace=TRUE), levels=c(0,1))

# Survival outcome:
ysurv <- cbind(rnorm(40), sample(c(0,1), size=40, replace=TRUE))



# Divide in training and test data:

Xtrain <- X[1:30,]
Xtest <- X[31:40,]

ybintrain <- ybin[1:30]
ybintest <- ybin[31:40]

ysurvtrain <- ysurv[1:30,]
ysurvtest <- ysurv[31:40,]




# Binary outcome: Apply algorithm to training data and obtain predictions
# for the test data:
#########################################################################

# Apply a variant to the training data:

blockforobj <- blockfor(Xtrain, ybintrain, num.trees = 100, replace = TRUE, block=block,
                        nsets = 10, num.trees.pre = 50, splitrule="extratrees", 
                        block.method = "SplitWeights")
blockforobj$paramvalues
#> [1] 1.00000000 0.06193513 0.57704991


# Obtain prediction for the test data:

(predres <- predict(blockforobj$forest, data = Xtest, block.method = "SplitWeights"))
#> blockForest prediction
#> 
#> Type:                             Classification 
#> Sample size:                      10 
#> Number of independent variables:  135 
predres$predictions
#>  [1] 0 0 0 0 0 0 0 0 0 0
#> Levels: 0 1



# Survival outcome: Apply algorithm to training data and obtain predictions
# for the test data:
###########################################################################

# Apply a variant to the training data:

blockforobj <- blockfor(Xtrain, ysurvtrain, num.trees = 100, replace = TRUE, block=block,
                        nsets = 10, num.trees.pre = 50, splitrule="extratrees", 
                        block.method = "SplitWeights")
blockforobj$paramvalues
#> [1] 1.00000000 0.07620874 0.86167920


# Obtain prediction for the test data:

(predres <- predict(blockforobj$forest, data = Xtest, block.method = "SplitWeights"))
#> blockForest prediction
#> 
#> Type:                             Survival 
#> Sample size:                      10 
#> Number of independent variables:  135 
#> Number of unique death times:     30 
rowSums(predres$chf)
#>  [1] 12.156000 14.736000 13.539500 13.278667  9.685667 12.356167 12.604167
#>  [8] 14.348333 12.657833 14.365833
```
