# Tree information in human readable format

Extract tree information of a `blockForest` object.

## Usage

``` r
treeInfo(object, tree = 1)
```

## Arguments

- object:

  `blockForest` object.

- tree:

  Number of the tree of interest.

## Value

A data.frame with the columns

|                |                                                                                                                                                                                                                                                                             |
|----------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `nodeID`       | The nodeID, 0-indexed.                                                                                                                                                                                                                                                      |
| `leftChild`    | ID of the left child node, 0-indexed.                                                                                                                                                                                                                                       |
| `rightChild`   | ID of the right child node, 0-indexed.                                                                                                                                                                                                                                      |
| `splitvarID`   | ID of the splitting variable, 0-indexed. Caution, the variable order changes if the formula interface is used.                                                                                                                                                              |
| `splitvarName` | Name of the splitting variable.                                                                                                                                                                                                                                             |
| `splitval`     | The splitting value. For numeric or ordinal variables, all values smaller or equal go to the left, larger values to the right. For unordered factor variables see above.                                                                                                    |
| `terminal`     | Logical, TRUE for terminal nodes.                                                                                                                                                                                                                                           |
| `prediction`   | One column with the predicted class (factor) for classification and the predicted numerical value for regression. One probability per class for probability estimation in several columns. Nothing for survival, refer to `object$forest$chf` for the CHF node predictions. |

## Details

Node and variable ID's are 0-indexed, i.e., node 0 is the root node. If
the formula interface is used in the `blockForest` call, the variable
ID's are usually different to the original data used to grow the tree.
Refer to the variable name instead to be sure.

Splitting at unordered factors (nominal variables) depends on the option
`respect.unordered.factors` in the `blockForest` call. For the "ignore"
and "order" approaches, all values smaller or equal the `splitval` value
go to the left and all values larger go to the right, as usual. However,
with "order" the values correspond to the order in
`object$forest$covariate.levels` instead of the original order (usually
alphabetical). In the "partition" mode, the `splitval` values for
unordered factor are comma separated lists of values, representing the
factor levels (in the original order) going to the left.

## Author

Marvin N. Wright

## Examples

``` r
require(blockForest)
rf <- blockForest(Species ~ ., data = iris)
treeInfo(rf, 1)
#>    nodeID leftChild rightChild splitvarID splitvarName splitval terminal
#> 1       0         1          2          3 Petal.Length     2.45    FALSE
#> 2       1        NA         NA         NA         <NA>       NA     TRUE
#> 3       2         3          4          3 Petal.Length     4.75    FALSE
#> 4       3        NA         NA         NA         <NA>       NA     TRUE
#> 5       4         5          6          4  Petal.Width     1.75    FALSE
#> 6       5         7          8          2  Sepal.Width     2.35    FALSE
#> 7       6         9         10          1 Sepal.Length     5.95    FALSE
#> 8       7        NA         NA         NA         <NA>       NA     TRUE
#> 9       8        11         12          1 Sepal.Length     7.05    FALSE
#> 10      9        13         14          1 Sepal.Length     5.85    FALSE
#> 11     10        NA         NA         NA         <NA>       NA     TRUE
#> 12     11        15         16          2  Sepal.Width     2.90    FALSE
#> 13     12        NA         NA         NA         <NA>       NA     TRUE
#> 14     13        NA         NA         NA         <NA>       NA     TRUE
#> 15     14        NA         NA         NA         <NA>       NA     TRUE
#> 16     15        17         18          2  Sepal.Width     2.75    FALSE
#> 17     16        NA         NA         NA         <NA>       NA     TRUE
#> 18     17        NA         NA         NA         <NA>       NA     TRUE
#> 19     18        19         20          1 Sepal.Length     6.55    FALSE
#> 20     19        NA         NA         NA         <NA>       NA     TRUE
#> 21     20        NA         NA         NA         <NA>       NA     TRUE
#>    prediction
#> 1        <NA>
#> 2      setosa
#> 3        <NA>
#> 4  versicolor
#> 5        <NA>
#> 6        <NA>
#> 7        <NA>
#> 8   virginica
#> 9        <NA>
#> 10       <NA>
#> 11  virginica
#> 12       <NA>
#> 13  virginica
#> 14  virginica
#> 15 versicolor
#> 16       <NA>
#> 17 versicolor
#> 18 versicolor
#> 19       <NA>
#> 20  virginica
#> 21 versicolor
```
