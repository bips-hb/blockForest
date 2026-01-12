# blockForest

Block forests without parameter tuning. Use
[`blockfor`](https://bips-hb.github.io/blockForest/reference/blockfor.md)
for standard interface. This function is called by
[`blockfor`](https://bips-hb.github.io/blockForest/reference/blockfor.md)
and will rarely be considered directly by the user (since parameter
tuning is required in applications).

## Usage

``` r
blockForest(
  formula = NULL,
  data = NULL,
  num.trees = 500,
  mtry = NULL,
  importance = "none",
  write.forest = TRUE,
  probability = FALSE,
  min.node.size = NULL,
  replace = TRUE,
  sample.fraction = ifelse(replace, 1, 0.632),
  case.weights = NULL,
  splitrule = NULL,
  num.random.splits = 1,
  alpha = 0.5,
  minprop = 0.1,
  split.select.weights = NULL,
  always.split.variables = NULL,
  blocks = NULL,
  block.method = "BlockForest",
  block.weights = NULL,
  respect.unordered.factors = NULL,
  scale.permutation.importance = FALSE,
  keep.inbag = FALSE,
  holdout = FALSE,
  quantreg = FALSE,
  num.threads = NULL,
  save.memory = FALSE,
  verbose = TRUE,
  seed = NULL,
  dependent.variable.name = NULL,
  status.variable.name = NULL,
  classification = NULL
)
```

## Arguments

- formula:

  Object of class `formula` or `character` describing the model to fit.
  Interaction terms supported only for numerical variables.

- data:

  Training data of class `data.frame`, `matrix`, `dgCMatrix` (Matrix) or
  `gwaa.data` (GenABEL).

- num.trees:

  Number of trees.

- mtry:

  This is either a number specifying the number of variables sampled for
  each split from all variables (for variants "VarProb" and
  "SplitWeights") or a vector of length equal to the number of blocks,
  where the m-th entry of the vector gives the number of variables to
  sample from block m (for variants "BlockForest", "RandomBlock", and
  "BlockVarSel"). The default values are sqrt(p_1) + sqrt(p_2) + ...
  sqrt(p_M) and (sqrt(p_1), sqrt(p_2), ..., sqrt(p_M)), respectively,
  where p_m denotes the number of variables in the m-th block (m = 1,
  ..., M) and sqrt() denoted the square root function.

- importance:

  Variable importance mode, one of 'none', 'impurity',
  'impurity_corrected', 'permutation'. The 'impurity' measure is the
  Gini index for classification, the variance of the responses for
  regression and the sum of test statistics (see `splitrule`) for
  survival.

- write.forest:

  Save `blockForest.forest` object, required for prediction. Set to
  `FALSE` to reduce memory usage if no prediction intended.

- probability:

  Grow a probability forest as in Malley et al. (2012).

- min.node.size:

  Minimal node size. Default 1 for classification, 5 for regression, 3
  for survival, and 10 for probability.

- replace:

  Sample with replacement.

- sample.fraction:

  Fraction of observations to sample. Default is 1 for sampling with
  replacement and 0.632 for sampling without replacement. For
  classification, this can be a vector of class-specific values.

- case.weights:

  Weights for sampling of training observations. Observations with
  larger weights will be selected with higher probability in the
  bootstrap (or subsampled) samples for the trees.

- splitrule:

  Splitting rule, default "extratrees". Other options are "gini" for
  classification and probability estimation, "variance", or "maxstat"
  for regression and "logrank", "C" or "maxstat" for survival.

- num.random.splits:

  For "extratrees" splitrule.: Number of random splits to consider for
  each candidate splitting variable.

- alpha:

  For "maxstat" splitrule: Significance threshold to allow splitting.

- minprop:

  For "maxstat" splitrule: Lower quantile of covariate distribution to
  be considered for splitting.

- split.select.weights:

  Numeric vector with weights between 0 and 1, representing the
  probability to select variables for splitting. Alternatively, a list
  of size num.trees, containing split select weight vectors for each
  tree can be used. Use this for the "VarProb" variant.

- always.split.variables:

  Character vector with variable names to be always selected in addition
  to the `mtry` variables tried for splitting.

- blocks:

  Block memberships of the variables. See
  [`blockfor`](https://bips-hb.github.io/blockForest/reference/blockfor.md)
  for details.

- block.method:

  Variant to use. Options are: "BlockForest" (default), "RandomBlock",
  "BlockVarSel", "SplitWeights".

- block.weights:

  Tuning parameter values for the blocks in the variants. A vector of
  length equal to the number of blocks or a list with vectors containing
  tree-wise values. For block.method='RandomBlock' these are the block
  sample probabilities.

- respect.unordered.factors:

  Handling of unordered factor covariates. One of 'ignore', 'order' and
  'partition'. For the "extratrees" splitrule the default is "partition"
  for all other splitrules 'ignore'. Alternatively TRUE (='order') or
  FALSE (='ignore') can be used. See below for details.

- scale.permutation.importance:

  Scale permutation importance by standard error as in (Breiman 2001).
  Only applicable if permutation variable importance mode selected.

- keep.inbag:

  Save how often observations are in-bag in each tree.

- holdout:

  Hold-out mode. Hold-out all samples with case weight 0 and use these
  for variable importance and prediction error.

- quantreg:

  Prepare quantile prediction as in quantile regression forests
  (Meinshausen 2006). Regression only. Set `keep.inbag = TRUE` to
  prepare out-of-bag quantile prediction.

- num.threads:

  Number of threads. Default is number of CPUs available.

- save.memory:

  Use memory saving (but slower) splitting mode. No effect for survival
  and GWAS data. Warning: This option slows down the tree growing, use
  only if you encounter memory problems.

- verbose:

  Show computation status and estimated runtime.

- seed:

  Random seed. Default is `NULL`, which generates the seed from `R`. Set
  to `0` to ignore the `R` seed.

- dependent.variable.name:

  Name of dependent variable, needed if no formula given. For survival
  forests this is the time variable.

- status.variable.name:

  Name of status variable, only applicable to survival data and needed
  if no formula given. Use 1 for event and 0 for censoring.

- classification:

  Only needed if data is a matrix. Set to `TRUE` to grow a
  classification forest.

## Value

Object of class `blockForest` with elements

- `forest`:

  Saved forest (If write.forest set to TRUE). Note that the variable IDs
  in the `split.varIDs` object do not necessarily represent the column
  number in R.

- `predictions`:

  Predicted classes/values, based on out of bag samples (classification
  and regression only).

- `variable.importance`:

  Variable importance for each independent variable.

- `prediction.error`:

  Overall out of bag prediction error. For classification this is the
  fraction of missclassified samples, for probability estimation and
  regression the mean squared error and for survival one minus Harrell's
  C-index.

- `r.squared`:

  R squared. Also called explained variance or coefficient of
  determination (regression only). Computed on out of bag data.

- `confusion.matrix`:

  Contingency table for classes and predictions based on out of bag
  samples (classification only).

- `unique.death.times`:

  Unique death times (survival only).

- `chf`:

  Estimated cumulative hazard function for each sample (survival only).

- `survival`:

  Estimated survival function for each sample (survival only).

- `call`:

  Function call.

- `num.trees`:

  Number of trees.

- `num.independent.variables`:

  Number of independent variables.

- `mtry`:

  Value of mtry used.

- `min.node.size`:

  Value of minimal node size used.

- `treetype`:

  Type of forest/tree. classification, regression or survival.

- `importance.mode`:

  Importance mode used.

- `num.samples`:

  Number of samples.

- `inbag.counts`:

  Number of times the observations are in-bag in the trees.

## Details

See
[`blockfor`](https://bips-hb.github.io/blockForest/reference/blockfor.md)
and the `ranger` package.

## References

- Hornung, R. & Wright, M. N. (2019) Block Forests: random forests for
  blocks of clinical and omics covariate data. BMC Bioinformatics
  20:358.
  [doi:10.1186/s12859-019-2942-y](https://doi.org/10.1186/s12859-019-2942-y)
  .

- Wright, M. N. & Ziegler, A. (2017). ranger: A Fast Implementation of
  Random Forests for High Dimensional Data in C++ and R. J Stat Softw
  77:1-17.
  [doi:10.18637/jss.v077.i01](https://doi.org/10.18637/jss.v077.i01) .

- Schmid, M., Wright, M. N. & Ziegler, A. (2016). On the use of
  Harrell's C for clinical risk prediction via random survival forests.
  Expert Syst Appl 63:450-459.
  [doi:10.1016/j.eswa.2016.07.018](https://doi.org/10.1016/j.eswa.2016.07.018)
  .

- Wright, M. N., Dankowski, T. & Ziegler, A. (2017). Unbiased split
  variable selection for random survival forests using maximally
  selected rank statistics. Stat Med.
  [doi:10.1002/sim.7212](https://doi.org/10.1002/sim.7212) .

- Breiman, L. (2001). Random forests. Mach Learn, 45(1), 5-32.
  [doi:10.1023/A:1010933404324](https://doi.org/10.1023/A%3A1010933404324)
  .

- Ishwaran, H., Kogalur, U. B., Blackstone, E. H., & Lauer, M. S.
  (2008). Random survival forests. Ann Appl Stat 2:841-860.
  [doi:10.1097/JTO.0b013e318233d835](https://doi.org/10.1097/JTO.0b013e318233d835)
  .

- Malley, J. D., Kruppa, J., Dasgupta, A., Malley, K. G., & Ziegler, A.
  (2012). Probability machines: consistent probability estimation using
  nonparametric learning machines. Methods Inf Med 51:74-81.
  [doi:10.3414/ME00-01-0052](https://doi.org/10.3414/ME00-01-0052) .

- Hastie, T., Tibshirani, R., Friedman, J. (2009). The Elements of
  Statistical Learning. Springer, New York. 2nd edition.

- Geurts, P., Ernst, D., Wehenkel, L. (2006). Extremely randomized
  trees. Mach Learn 63:3-42.
  [doi:10.1007/s10994-006-6226-1](https://doi.org/10.1007/s10994-006-6226-1)
  .

- Meinshausen (2006). Quantile Regression Forests. J Mach Learn Res
  7:983-999. <https://www.jmlr.org/papers/v7/meinshausen06a.html>.

## See also

[`predict.blockForest`](https://bips-hb.github.io/blockForest/reference/predict.blockForest.md)

## Author

Marvin N. Wright

## Examples

``` r
require(blockForest)

# Standard Block Forest
blockForest(Species ~ ., iris, 
            blocks = list(1:2, 3:4), 
            mtry = c(1, 2), 
            block.weights = c(0.1, 0.9), 
            block.method = "BlockForest")
#> blockForest result
#> 
#> Call:
#>  blockForest(Species ~ ., iris, blocks = list(1:2, 3:4), mtry = c(1,      2), block.weights = c(0.1, 0.9), block.method = "BlockForest") 
#> 
#> Type:                             Classification 
#> Number of trees:                  500 
#> Sample size:                      150 
#> Number of independent variables:  4 
#> Mtry:                             1 2 
#> Target node size:                 1 
#> Variable importance mode:         none 
#> OOB prediction error:             4.00 % 

# Without blocks, grow standard random forest
blockForest(Species ~ ., iris)
#> blockForest result
#> 
#> Call:
#>  blockForest(Species ~ ., iris) 
#> 
#> Type:                             Classification 
#> Number of trees:                  500 
#> Sample size:                      150 
#> Number of independent variables:  4 
#> Mtry:                             2 
#> Target node size:                 1 
#> Variable importance mode:         none 
#> OOB prediction error:             4.67 % 
```
