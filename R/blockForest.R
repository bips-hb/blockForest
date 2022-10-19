# -------------------------------------------------------------------------------
#   This file is part of blockForest
#
# blockForest is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# blockForest is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with blockForest If not, see <http://www.gnu.org/licenses/>.
# -------------------------------------------------------------------------------

##' Block forests without parameter tuning. 
##' Use \code{\link{blockfor}} for standard interface.
##' This function is called by \code{\link{blockfor}}
##' and will rarely be considered directly by the user (since parameter tuning
##' is required in applications).
##' 
##' See \code{\link{blockfor}} and the \code{ranger} package.
##' 
##' @title blockForest
##' @param formula Object of class \code{formula} or \code{character} describing the model to fit. Interaction terms supported only for numerical variables.
##' @param data Training data of class \code{data.frame}, \code{matrix}, \code{dgCMatrix} (Matrix) or \code{gwaa.data} (GenABEL).
##' @param num.trees Number of trees.
##' @param mtry This is either a number specifying the number of variables sampled for each
##' split from all variables (for variants "VarProb" and "SplitWeights")
##' or a vector of length equal to the number of blocks, where the m-th entry of the
##' vector gives the number of variables to sample from block m (for variants "BlockForest", "RandomBlock", and "BlockVarSel").
##' The default values are sqrt(p_1) + sqrt(p_2) + ... sqrt(p_M) and (sqrt(p_1), sqrt(p_2), ..., sqrt(p_M)), respectively,
##' where p_m denotes the number of variables in the m-th block (m = 1, ..., M) and sqrt() denoted the square root function.
##' @param importance Variable importance mode, one of 'none', 'impurity', 'impurity_corrected', 'permutation'. The 'impurity' measure is the Gini index for classification, the variance of the responses for regression and the sum of test statistics (see \code{splitrule}) for survival. 
##' @param write.forest Save \code{blockForest.forest} object, required for prediction. Set to \code{FALSE} to reduce memory usage if no prediction intended.
##' @param probability Grow a probability forest as in Malley et al. (2012). 
##' @param min.node.size Minimal node size. Default 1 for classification, 5 for regression, 3 for survival, and 10 for probability.
##' @param replace Sample with replacement. 
##' @param sample.fraction Fraction of observations to sample. Default is 1 for sampling with replacement and 0.632 for sampling without replacement. For classification, this can be a vector of class-specific values. 
##' @param case.weights Weights for sampling of training observations. Observations with larger weights will be selected with higher probability in the bootstrap (or subsampled) samples for the trees.
##' @param splitrule Splitting rule, default "extratrees". Other options are "gini" for classification and probability estimation, "variance", or "maxstat" for regression and "logrank", "C" or "maxstat" for survival.
##' @param num.random.splits For "extratrees" splitrule.: Number of random splits to consider for each candidate splitting variable.
##' @param alpha For "maxstat" splitrule: Significance threshold to allow splitting.
##' @param minprop For "maxstat" splitrule: Lower quantile of covariate distribution to be considered for splitting.
##' @param split.select.weights Numeric vector with weights between 0 and 1, representing the probability to select variables for splitting. Alternatively, a list of size num.trees, containing split select weight vectors for each tree can be used. Use this for the "VarProb" variant.
##' @param always.split.variables Character vector with variable names to be always selected in addition to the \code{mtry} variables tried for splitting.
##' @param blocks Block memberships of the variables. See \code{\link{blockfor}} for details. 
##' @param block.method Variant to use. Options are: "BlockForest" (default), "RandomBlock", "BlockVarSel", "SplitWeights".
##' @param block.weights Tuning parameter values for the blocks in the variants. A vector of length equal to the number of blocks or a list with vectors containing tree-wise values. For block.method='RandomBlock' these are the block sample probabilities.
##' @param respect.unordered.factors Handling of unordered factor covariates. One of 'ignore', 'order' and 'partition'. For the "extratrees" splitrule the default is "partition" for all other splitrules 'ignore'. Alternatively TRUE (='order') or FALSE (='ignore') can be used. See below for details. 
##' @param scale.permutation.importance Scale permutation importance by standard error as in (Breiman 2001). Only applicable if permutation variable importance mode selected.
##' @param keep.inbag Save how often observations are in-bag in each tree. 
##' @param holdout Hold-out mode. Hold-out all samples with case weight 0 and use these for variable importance and prediction error.
##' @param quantreg Prepare quantile prediction as in quantile regression forests (Meinshausen 2006). Regression only. Set \code{keep.inbag = TRUE} to prepare out-of-bag quantile prediction.
##' @param num.threads Number of threads. Default is number of CPUs available.
##' @param save.memory Use memory saving (but slower) splitting mode. No effect for survival and GWAS data. Warning: This option slows down the tree growing, use only if you encounter memory problems.
##' @param verbose Show computation status and estimated runtime.
##' @param seed Random seed. Default is \code{NULL}, which generates the seed from \code{R}. Set to \code{0} to ignore the \code{R} seed. 
##' @param dependent.variable.name Name of dependent variable, needed if no formula given. For survival forests this is the time variable.
##' @param status.variable.name Name of status variable, only applicable to survival data and needed if no formula given. Use 1 for event and 0 for censoring.
##' @param classification Only needed if data is a matrix. Set to \code{TRUE} to grow a classification forest.
##' @return Object of class \code{blockForest} with elements
##'   \item{\code{forest}}{Saved forest (If write.forest set to TRUE). Note that the variable IDs in the \code{split.varIDs} object do not necessarily represent the column number in R.}
##'   \item{\code{predictions}}{Predicted classes/values, based on out of bag samples (classification and regression only).}
##'   \item{\code{variable.importance}}{Variable importance for each independent variable.}
##'   \item{\code{prediction.error}}{Overall out of bag prediction error. For classification this is the fraction of missclassified samples, for probability estimation and regression the mean squared error and for survival one minus Harrell's C-index.}
##'   \item{\code{r.squared}}{R squared. Also called explained variance or coefficient of determination (regression only). Computed on out of bag data.}
##'   \item{\code{confusion.matrix}}{Contingency table for classes and predictions based on out of bag samples (classification only).}
##'   \item{\code{unique.death.times}}{Unique death times (survival only).}
##'   \item{\code{chf}}{Estimated cumulative hazard function for each sample (survival only).}
##'   \item{\code{survival}}{Estimated survival function for each sample (survival only).}
##'   \item{\code{call}}{Function call.}
##'   \item{\code{num.trees}}{Number of trees.}
##'   \item{\code{num.independent.variables}}{Number of independent variables.}
##'   \item{\code{mtry}}{Value of mtry used.}
##'   \item{\code{min.node.size}}{Value of minimal node size used.}
##'   \item{\code{treetype}}{Type of forest/tree. classification, regression or survival.}
##'   \item{\code{importance.mode}}{Importance mode used.}
##'   \item{\code{num.samples}}{Number of samples.}
##'   \item{\code{inbag.counts}}{Number of times the observations are in-bag in the trees.}
##' @examples
##' require(blockForest)
##' 
##' # Standard Block Forest
##' blockForest(Species ~ ., iris, 
##'             blocks = list(1:2, 3:4), 
##'             mtry = c(1, 2), 
##'             block.weights = c(0.1, 0.9), 
##'             block.method = "BlockForest")
##'
##' # Without blocks, grow standard random forest
##' blockForest(Species ~ ., iris)
##'
##' @author Marvin N. Wright
##' @references
##' \itemize{
##'   \item Hornung, R. & Wright, M. N. (2019) Block Forests: random forests for blocks of clinical and omics covariate data. BMC Bioinformatics 20:358. \doi{10.1186/s12859-019-2942-y}.
##'   \item Wright, M. N. & Ziegler, A. (2017). ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R. J Stat Softw 77:1-17. \doi{10.18637/jss.v077.i01}.
##'   \item Schmid, M., Wright, M. N. & Ziegler, A. (2016). On the use of Harrell's C for clinical risk prediction via random survival forests. Expert Syst Appl 63:450-459. \doi{10.1016/j.eswa.2016.07.018}. 
##'   \item Wright, M. N., Dankowski, T. & Ziegler, A. (2017). Unbiased split variable selection for random survival forests using maximally selected rank statistics. Stat Med. \doi{10.1002/sim.7212}.
##'   \item Breiman, L. (2001). Random forests. Mach Learn, 45(1), 5-32. \doi{10.1023/A:1010933404324}. 
##'   \item Ishwaran, H., Kogalur, U. B., Blackstone, E. H., & Lauer, M. S. (2008). Random survival forests. Ann Appl Stat 2:841-860. \doi{10.1097/JTO.0b013e318233d835}. 
##'   \item Malley, J. D., Kruppa, J., Dasgupta, A., Malley, K. G., & Ziegler, A. (2012). Probability machines: consistent probability estimation using nonparametric learning machines. Methods Inf Med 51:74-81. \doi{10.3414/ME00-01-0052}.
##'   \item Hastie, T., Tibshirani, R., Friedman, J. (2009). The Elements of Statistical Learning. Springer, New York. 2nd edition.
##'   \item Geurts, P., Ernst, D., Wehenkel, L. (2006). Extremely randomized trees. Mach Learn 63:3-42. \doi{10.1007/s10994-006-6226-1}.
##'   \item Meinshausen (2006). Quantile Regression Forests. J Mach Learn Res 7:983-999. \url{https://www.jmlr.org/papers/v7/meinshausen06a.html}.  
##'   }
##' @seealso \code{\link{predict.blockForest}}
##' @useDynLib blockForest, .registration = TRUE
##' @importFrom Rcpp evalCpp
##' @import stats 
##' @import utils
##' @importFrom Matrix Matrix
##' @export
blockForest <- function(formula = NULL, data = NULL, num.trees = 500, mtry = NULL,
                        importance = "none", write.forest = TRUE, probability = FALSE,
                        min.node.size = NULL, replace = TRUE, 
                        sample.fraction = ifelse(replace, 1, 0.632), 
                        case.weights = NULL, splitrule = NULL, 
                        num.random.splits = 1, alpha = 0.5, minprop = 0.1,
                        split.select.weights = NULL, always.split.variables = NULL,
                        blocks = NULL, block.method = "BlockForest", block.weights = NULL, 
                        respect.unordered.factors = NULL,
                        scale.permutation.importance = FALSE,
                        keep.inbag = FALSE, holdout = FALSE,
                        quantreg = FALSE,
                        num.threads = NULL, save.memory = FALSE,
                        verbose = TRUE, seed = NULL, 
                        dependent.variable.name = NULL, status.variable.name = NULL, 
                        classification = NULL) {
  
  ## GenABEL GWA data
  if (inherits(data, "gwaa.data")) {
    snp.names <- data@gtdata@snpnames
    snp.data <- data@gtdata@gtps@.Data
    data <- data@phdata
    if ("id" %in% names(data)) {
      data$"id" <- NULL
    }
    gwa.mode <- TRUE
    save.memory <- FALSE
  } else {
    snp.data <- as.matrix(0)
    gwa.mode <- FALSE
  }
  
  ## Sparse matrix data
  if (inherits(data, "Matrix")) {
    if (!inherits(data, "dgCMatrix")) {
      stop("Error: Currently only sparse data of class 'dgCMatrix' supported.")
    }
    
    if (!is.null(formula)) {
      stop("Error: Sparse matrices only supported with alternative interface. Use dependent.variable.name instead of formula.")
    }
  }
  
  ## Formula interface. Use whole data frame is no formula provided and depvarname given
  if (is.null(formula)) {
    if (is.null(dependent.variable.name)) {
      stop("Error: Please give formula or dependent variable name.")
    }
    if (is.null(status.variable.name)) {
      status.variable.name <- "none"
      response <- data[, dependent.variable.name]
    } else {
      response <- data[, c(dependent.variable.name, status.variable.name)]
    }
    data.selected <- data
    
    # Dependent variable has to be first in blockForest
    if (status.variable.name == "none" || is.null(status.variable.name)) {
      if (colnames(data.selected)[1] != dependent.variable.name) {
        dependent.varID <- which(colnames(data.selected) == dependent.variable.name)
        data.selected <- cbind(data.selected[, dependent.varID], data.selected[, -dependent.varID, drop = FALSE])
        colnames(data.selected)[1] <- dependent.variable.name
      }
    } else {
      if ((colnames(data.selected)[1] != dependent.variable.name) || (colnames(data.selected)[2] != status.variable.name)) {
        dependent.varID <- which(colnames(data.selected) == dependent.variable.name)
        status.varID <- which(colnames(data.selected) == status.variable.name)
        data.selected <- cbind(data.selected[, c(dependent.varID, status.varID)], data.selected[, c(-dependent.varID, -status.varID), drop = FALSE])
        colnames(data.selected)[c(1,2)] <- c(dependent.variable.name, status.variable.name)
      }
    }
  } else {
    formula <- formula(formula)
    if (!inherits(formula, "formula")) {
      stop("Error: Invalid formula.")
    }
    data.selected <- parse.formula(formula, data)
    response <- data.selected[, 1]
  }
  
  ## Check missing values
  if (any(is.na(data.selected))) {
    offending_columns <- colnames(data.selected)[colSums(is.na(data.selected)) > 0]
    stop("Missing data in columns: ",
         paste0(offending_columns, collapse = ", "), ".", call. = FALSE)
  }
  
  ## Check response levels
  if (is.factor(response)) {
    if (nlevels(response) != nlevels(droplevels(response))) {
      dropped_levels <- setdiff(levels(response), levels(droplevels(response)))
      warning("Dropped unused factor level(s) in dependent variable: ",
              paste0(dropped_levels, collapse = ", "), ".", call. = FALSE)
    }
  }
  
  ## Treetype
  if (is.factor(response)) {
    if (probability) {
      treetype <- 9
    } else {
      treetype <- 1
    }
  } else if (is.numeric(response) && is.vector(response)) {
    if (!is.null(classification) && classification) {
      treetype <- 1
    } else if (probability) {
      treetype <- 9
    } else {
      treetype <- 3
    }
  } else if (inherits(response, "Surv") || is.data.frame(response) || is.matrix(response)) {
    treetype <- 5
  } else {
    stop("Error: Unsupported type of dependent variable.")
  }
  
  ## Qunatile prediction only for regression
  if (quantreg && treetype != 3) {
    stop("Error: Quantile prediction implemented only for regression outcomes.")
  }
  
  ## Dependent and status variable name. For non-survival dummy status variable name.
  if (!is.null(formula)) {
    if (treetype == 5) {
      dependent.variable.name <- dimnames(response)[[2]][1]
      status.variable.name <- dimnames(response)[[2]][2]
    } else {
      dependent.variable.name <- names(data.selected)[1]
      status.variable.name <- "none"
    }
    independent.variable.names <- names(data.selected)[-1]
  } else {
    independent.variable.names <- colnames(data.selected)[colnames(data.selected) != dependent.variable.name &
                                                            colnames(data.selected) != status.variable.name]
  }
  
  ## respect.unordered.factors
  if (is.null(respect.unordered.factors)) {
    if (!is.null(splitrule) && splitrule == "extratrees") {
      respect.unordered.factors <- "partition"
    } else {
      respect.unordered.factors <- "ignore"
    }
  }
  
  ## Old version of respect.unordered.factors
  if (respect.unordered.factors == TRUE) {
    respect.unordered.factors <- "order"
  } else if (respect.unordered.factors == FALSE) {
    respect.unordered.factors <- "ignore"
  }
  
  ## Recode characters as factors and recode factors if 'order' mode
  if (!is.matrix(data.selected) && !inherits(data.selected, "Matrix")) {
    character.idx <- sapply(data.selected, is.character)
    
    if (respect.unordered.factors == "order") {
      ## Recode characters and unordered factors
      names.selected <- names(data.selected)
      ordered.idx <- sapply(data.selected, is.ordered)
      factor.idx <- sapply(data.selected, is.factor)
      independent.idx <- names.selected != dependent.variable.name & 
        names.selected != status.variable.name & 
        names.selected != paste0("Surv(", dependent.variable.name, ", ", status.variable.name, ")")
      recode.idx <- independent.idx & (character.idx | (factor.idx & !ordered.idx))
      
      ## Numeric response
      if (is.factor(response)) {
        num.response <- as.numeric(response)
      } else if (!is.null(dim(response))) {
        num.response <- response[, 1]
      } else {
        num.response <- response
      }
      
      ## Recode each column
      data.selected[recode.idx] <- lapply(data.selected[recode.idx], function(x) {
        ## Order factor levels
        means <- aggregate(num.response~x, FUN=mean)
        levels.ordered <- means$x[order(means$num.response)]
        
        ## Return reordered factor
        factor(x, levels = levels.ordered)
      })
      
      ## Save levels
      covariate.levels <- lapply(data.selected[independent.idx], levels)
    } else {
      ## Recode characters only
      data.selected[character.idx] <- lapply(data.selected[character.idx], factor)
    }
  }
  
  ## Input data and variable names, create final data matrix
  if (!is.null(formula) && treetype == 5) {
    data.final <- data.matrix(cbind(response[, 1], response[, 2],
                                    data.selected[-1]))
    colnames(data.final) <- c(dependent.variable.name, status.variable.name,
                              independent.variable.names)
  } else if (is.matrix(data.selected) || inherits(data.selected, "Matrix")) {
    data.final <- data.selected
  } else {
    data.final <- data.matrix(data.selected)
  }
  variable.names <- colnames(data.final)
  
  ## If gwa mode, add snp variable names
  if (gwa.mode) {
    variable.names <- c(variable.names, snp.names)
    all.independent.variable.names <- c(independent.variable.names, snp.names)
  } else {
    all.independent.variable.names <- independent.variable.names
  }
  
  ## Error if no covariates
  if (length(all.independent.variable.names) < 1) {
    stop("Error: No covariates found.")
  }
  
  ## Number of trees
  if (!is.numeric(num.trees) || num.trees < 1) {
    stop("Error: Invalid value for num.trees.")
  }
  
  ## mtry
  if (is.null(mtry)) {
    mtry <- 0
  } else if (!is.numeric(mtry) || any(mtry < 0)) {
    stop("Error: Invalid value for mtry")
  }
  
  ## Seed
  if (is.null(seed)) {
    seed <- runif(1 , 0, .Machine$integer.max)
  }
  
  ## Keep inbag
  if (!is.logical(keep.inbag)) {
    stop("Error: Invalid value for keep.inbag")
  }
  
  ## Num threads
  ## Default 0 -> detect from system in C++.
  if (is.null(num.threads)) {
    num.threads = 0
  } else if (!is.numeric(num.threads) || num.threads < 0) {
    stop("Error: Invalid value for num.threads")
  }
  
  ## Minumum node size
  if (is.null(min.node.size)) {
    min.node.size <- 0
  } else if (!is.numeric(min.node.size) || min.node.size < 0) {
    stop("Error: Invalid value for min.node.size")
  }
  
  ## Sample fraction
  if (!is.numeric(sample.fraction)) {
    stop("Error: Invalid value for sample.fraction. Please give a value in (0,1] or a vector of values in [0,1].")
  }
  if (length(sample.fraction) > 1) {
    if (!(treetype %in% c(1, 9))) {
      stop("Error: Invalid value for sample.fraction. Vector values only valid for classification forests.")
    }
    if (any(sample.fraction < 0) || any(sample.fraction > 1)) {
      stop("Error: Invalid value for sample.fraction. Please give a value in (0,1] or a vector of values in [0,1].")
    }
    if (sum(sample.fraction) <= 0) {
      stop("Error: Invalid value for sample.fraction. Sum of values must be >0.")
    }
    if (length(sample.fraction) != nlevels(response)) {
      stop("Error: Invalid value for sample.fraction. Expecting ", nlevels(response), " values, provided ", length(sample.fraction), ".")
    }
    if (!replace & any(sample.fraction * length(response) > table(response))) {
      idx <- which(sample.fraction * length(response) > table(response))[1]
      stop("Error: Not enough samples in class ", names(idx), 
           "; available: ", table(response)[idx], 
           ", requested: ", (sample.fraction * length(response))[idx], ".")
    }
    if (!is.null(case.weights)) {
      stop("Error: Combination of case.weights and class-wise sampling not supported.")
    }
  } else {
    if (sample.fraction <= 0 || sample.fraction > 1) {
      stop("Error: Invalid value for sample.fraction. Please give a value in (0,1] or a vector of values in [0,1].")
    }
  }
  
  ## Importance mode
  if (is.null(importance) || importance == "none") {
    importance.mode <- 0
  } else if (importance == "impurity") {
    importance.mode <- 1
  } else if (importance == "impurity_corrected" || importance == "impurity_unbiased") {
    importance.mode <- 5
    if (!is.null(split.select.weights)) {
      stop("Corrected impurity importance not supported in combination with split.select.weights.")
    }
  } else if (importance == "permutation") {
    if (scale.permutation.importance) {
      importance.mode <- 2
    } else {
      importance.mode <- 3
    }
  } else {
    stop("Error: Unknown importance mode.")
  }
  
  ## Case weights: NULL for no weights
  if (is.null(case.weights)) {
    case.weights <- c(0,0)
    use.case.weights <- FALSE
    if (holdout) {
      stop("Error: Case weights required to use holdout mode.")
    }
  } else {
    use.case.weights <- TRUE
    
    ## Sample from non-zero weights in holdout mode
    if (holdout) {
      sample.fraction <- sample.fraction * mean(case.weights > 0)
    }
    
    if (!replace && sum(case.weights > 0) < sample.fraction * nrow(data.final)) {
      stop("Error: Fewer non-zero case weights than observations to sample.")
    }
  }
  
  ## Split select weights: NULL for no weights
  if (is.null(split.select.weights)) {
    split.select.weights <- list(c(0,0))
    use.split.select.weights <- FALSE
  } else if (is.numeric(split.select.weights)) {
    if (length(split.select.weights) != length(all.independent.variable.names)) {
      stop("Error: Number of split select weights not equal to number of independent variables.")
    }
    split.select.weights <- list(split.select.weights)
    use.split.select.weights <- TRUE
  } else if (is.list(split.select.weights)) {
    if (length(split.select.weights) != num.trees) {
      stop("Error: Size of split select weights list not equal to number of trees.")
    }
    use.split.select.weights <- TRUE
  } else {
    stop("Error: Invalid split select weights.")
  }
  
  ## Always split variables: NULL for no variables
  if (is.null(always.split.variables)) {
    always.split.variables <- c("0", "0")
    use.always.split.variables <- FALSE
  } else {
    use.always.split.variables <- TRUE
  }
  
  if (use.split.select.weights && use.always.split.variables) {
    stop("Error: Please use only one option of split.select.weights and always.split.variables.")
  }
  
  ## Splitting rule
  if (is.null(splitrule)) {
    if (treetype == 5) {
      splitrule <- "logrank"
    } else if (treetype == 3) {
      splitrule <- "variance"
    } else if (treetype %in% c(1, 9)) {
      splitrule <- "gini"
    }
    splitrule.num <- 1
  } else if (splitrule == "logrank") {
    if (treetype == 5) {
      splitrule.num <- 1
    } else {
      stop("Error: logrank splitrule applicable to survival data only.")
    }
  } else if (splitrule == "gini") {
    if (treetype %in% c(1, 9)) {
      splitrule.num <- 1
    } else {
      stop("Error: Gini splitrule applicable to classification data only.")
    }
  } else if (splitrule == "variance") {
    if (treetype == 3) {
      splitrule.num <- 1
    } else {
      stop("Error: variance splitrule applicable to regression data only.")
    }
  } else if (splitrule == "auc" || splitrule == "C") {
    if (treetype == 5) {
      splitrule.num <- 2
    } else {
      stop("Error: C index splitrule applicable to survival data only.")
    }
  } else if (splitrule == "auc_ignore_ties" || splitrule == "C_ignore_ties") {
    if (treetype == 5) {
      splitrule.num <- 3
    } else {
      stop("Error: C index splitrule applicable to survival data only.")
    }
  } else if (splitrule == "maxstat") {
    if (treetype == 5 || treetype == 3) {
      splitrule.num <- 4
    } else {
      stop("Error: maxstat splitrule applicable to regression or survival data only.")
    }
  } else if (splitrule == "extratrees") {
    splitrule.num <- 5
  } else {
    stop("Error: Unknown splitrule.")
  }
  
  ## Maxstat splitting
  if (alpha < 0 || alpha > 1) {
    stop("Error: Invalid value for alpha, please give a value between 0 and 1.")
  }
  if (minprop < 0 || minprop > 0.5) {
    stop("Error: Invalid value for minprop, please give a value between 0 and 0.5.")
  }
  
  ## Extra trees
  if (!is.numeric(num.random.splits) || num.random.splits < 1) {
    stop("Error: Invalid value for num.random.splits, please give a positive integer.")
  }
  if (splitrule.num == 5 && save.memory && respect.unordered.factors == "partition") {
    stop("Error: save.memory option not possible in extraTrees mode with unordered predictors.")
  }
  
  ## Unordered factors  
  if (respect.unordered.factors == "partition") {
    names.selected <- names(data.selected)
    ordered.idx <- sapply(data.selected, is.ordered)
    factor.idx <- sapply(data.selected, is.factor)
    independent.idx <- names.selected != dependent.variable.name & names.selected != status.variable.name
    unordered.factor.variables <- names.selected[factor.idx & !ordered.idx & independent.idx]
    
    if (length(unordered.factor.variables) > 0) {
      use.unordered.factor.variables <- TRUE
      ## Check level count
      num.levels <- sapply(data.selected[, factor.idx & !ordered.idx & independent.idx, drop = FALSE], nlevels)
      max.level.count <- .Machine$double.digits
      if (max(num.levels) > max.level.count) {
        stop(paste("Too many levels in unordered categorical variable ", unordered.factor.variables[which.max(num.levels)], 
                   ". Only ", max.level.count, " levels allowed on this system. Consider using the 'order' option.", sep = ""))
      } 
    } else {
      unordered.factor.variables <- c("0", "0")
      use.unordered.factor.variables <- FALSE
    } 
  } else if (respect.unordered.factors == "ignore" || respect.unordered.factors == "order") {
    ## Ordering for "order" is handled above
    unordered.factor.variables <- c("0", "0")
    use.unordered.factor.variables <- FALSE
  } else {
    stop("Error: Invalid value for respect.unordered.factors, please use 'order', 'partition' or 'ignore'.")
  }
  
  ## Unordered maxstat splitting not possible
  if (use.unordered.factor.variables && !is.null(splitrule)) {
    if (splitrule == "maxstat") {
      stop("Error: Unordered factor splitting not implemented for 'maxstat' splitting rule.")
    } else if (splitrule %in% c("C", "auc", "C_ignore_ties", "auc_ignore_ties")) {
      stop("Error: Unordered factor splitting not implemented for 'C' splitting rule.")
    }
  }
  
  ## Warning for experimental 'order' splitting 
  if (respect.unordered.factors == "order") {
    if (treetype == 5) {
      warning("Warning: The 'order' mode for unordered factor handling for survival outcomes is experimental.")
    } else if (treetype == 1 || treetype == 9) {
      if (nlevels(response) > 2) {
        warning("Warning: The 'order' mode for unordered factor handling for multiclass classification is experimental.")
      }
    } else if (treetype == 3 && splitrule == "maxstat") {
      warning("Warning: The 'order' mode for unordered factor handling with the 'maxstat' splitrule is experimental.")
    }
  }
  
  ## Block forests
  if (!is.null(blocks)) {
    if (block.method == "BlockVarSel") {
      block.method <- 1
    } else if (block.method == "RandomBlock") {
      block.method <- 2
    } else if (block.method == "SplitWeights") {
      block.method <- 3
    } else if (block.method == "BlockForest") {
      block.method <- 4
    } else {
      stop("Error: Unknown value for 'block.method'.") 
    }
    if (!inherits(blocks, "list") || !all(sapply(blocks, is.numeric))) {
      stop("Error: The 'blocks' argument is no list of numeric vectors.")
    }
    if (length(mtry) != length(blocks) & block.method != 3) {
      stop("Error: Length of 'blocks' and 'mtry' arguments not matching.")
    }
    if (!all(sapply(block.weights, is.numeric))) {
      stop("Error: Only numeric values accepted for 'block.weights'.")
    }
    if (is.list(block.weights)) {
      if (num.trees != length(block.weights)) {
        stop("Error: Length of 'block.weights' not matching number of trees.")
      } 
      if (any(sapply(block.weights, length) != length(blocks))) {
        stop("Error: Length of 'blocks' and 'block.weights' arguments not matching.")
      }
    } else {
      if (length(block.weights) != length(blocks)) {
        stop("Error: Length of 'blocks' and 'block.weights' arguments not matching.")
      } 
      block.weights <- list(block.weights)
    }
    if (block.method == 3 & (!is.numeric(mtry) | length(mtry) > 1)) {
      stop("Error: Single value for 'mtry' expected in 'SplitWeights' method.")
    }
    if (any(sapply(blocks, function(x) {any(round(x) != x)}))) {
      stop("Error: The 'blocks' argument contains non-integers.")
    }
    if (treetype == 5) {
      if (any(sapply(blocks, function(x) {any(x > (ncol(data.final) - 2))}))) {
        stop("Error: The 'blocks' argument contains variable indices not present in the data.")
      }
    } else {
      if (any(sapply(blocks, function(x) {any(x > (ncol(data.final) - 1))}))) {
        stop("Error: The 'blocks' argument contains variable indices not present in the data.")
      }
    }
    
    # All blocks +1 for survival (additional status variables)
    if (treetype == 5) {
      blocks <- lapply(blocks, function(x) {
        x + 1
      })
    }
  } else {
    if (length(mtry) > 1) {
      stop("Error: Vector arguments for mtry only accepted for block forests.")
    }
    if (!is.null(block.weights)) {
      stop("Error: block.weights argument only accepted for block forests.")
    }
    block.weights <- list()
    blocks <- list()
    block.method <- 0
  }
  
  ## Prediction mode always false. Use predict.blockForest() method.
  prediction.mode <- FALSE
  predict.all <- FALSE
  prediction.type <- 1
  
  ## No loaded forest object
  loaded.forest <- list()
  
  ## Use sparse matrix
  if (inherits(data.final, "dgCMatrix")) {
    sparse.data <- data.final
    data.final <- matrix(c(0, 0))
    use.sparse.data <- TRUE
  } else {
    sparse.data <- Matrix(matrix(c(0, 0)))
    use.sparse.data <- FALSE
  }
  
  ## Clean up
  rm("data.selected")
  
  ## Call Ranger
  result <- rangerCpp(treetype, dependent.variable.name, data.final, variable.names, mtry,
                      num.trees, verbose, seed, num.threads, write.forest, importance.mode,
                      min.node.size, split.select.weights, use.split.select.weights,
                      always.split.variables, use.always.split.variables,
                      status.variable.name, prediction.mode, loaded.forest, snp.data,
                      replace, probability, unordered.factor.variables, use.unordered.factor.variables, 
                      save.memory, splitrule.num, case.weights, use.case.weights, predict.all, 
                      keep.inbag, sample.fraction, alpha, minprop, holdout, prediction.type, 
                      num.random.splits, sparse.data, use.sparse.data, 
                      blocks, block.weights, block.method)
  
  if (length(result) == 0) {
    stop("User interrupt or internal error.")
  }
  
  ## Prepare results
  if (importance.mode != 0) {
    names(result$variable.importance) <- all.independent.variable.names
  }
  
  ## Set predictions
  if (treetype == 1 && is.factor(response)) {
    result$predictions <- integer.to.factor(result$predictions,
                                            levels(response))
    true.values <- integer.to.factor(unlist(data.final[, dependent.variable.name]),
                                     levels(response))
    result$confusion.matrix <- table(true.values, result$predictions, 
                                     dnn = c("true", "predicted"), useNA = "ifany")
  } else if (treetype == 5) {
    if (is.list(result$predictions)) {
      result$predictions <- do.call(rbind, result$predictions)
    } 
    if (is.vector(result$predictions)) {
      result$predictions <- matrix(result$predictions, nrow = 1)
    }
    result$chf <- result$predictions
    result$predictions <- NULL
    result$survival <- exp(-result$chf)
  } else if (treetype == 9 && !is.matrix(data)) {
    if (is.list(result$predictions)) {
      result$predictions <- do.call(rbind, result$predictions)
    } 
    if (is.vector(result$predictions)) {
      result$predictions <- matrix(result$predictions, nrow = 1)
    }
    
    ## Set colnames and sort by levels
    colnames(result$predictions) <- unique(response)
    result$predictions <- result$predictions[, levels(droplevels(response)), drop = FALSE]
  }
  
  ## Splitrule
  result$splitrule <- splitrule
  
  ## Set treetype
  if (treetype == 1) {
    result$treetype <- "Classification"
  } else if (treetype == 3) {
    result$treetype <- "Regression"
  } else if (treetype == 5) {
    result$treetype <- "Survival"
  } else if (treetype == 9) {
    result$treetype <- "Probability estimation"
  }
  if (treetype == 3) {
    result$r.squared <- 1 - result$prediction.error / var(response)
  }
  result$call <- sys.call()
  result$importance.mode <- importance
  result$num.samples <- nrow(data.final)
  result$replace <- replace
  
  ## Write forest object
  if (write.forest) {
    if (is.factor(response)) {
      result$forest$levels <- levels(droplevels(response))
    }
    result$forest$independent.variable.names <- independent.variable.names
    result$forest$treetype <- result$treetype
    class(result$forest) <- "blockForest.forest"
    
    ## In 'ordered' mode, save covariate levels
    if (respect.unordered.factors == "order" && !is.matrix(data)) {
      result$forest$covariate.levels <- covariate.levels
    }
  }
  
  class(result) <- "blockForest"
  
  ## Prepare quantile prediction
  if (quantreg) {
    terminal.nodes <- predict(result, data, type = "terminalNodes")$predictions
    n <- result$num.samples
    result$random.node.values <- matrix(nrow = max(terminal.nodes), ncol = num.trees)
    
    ## Select one random obs per node and tree
    for (tree in 1:num.trees){
      idx <- sample(1:n, n)
      result$random.node.values[terminal.nodes[idx, tree], tree] <- response[idx]
    }
    
    ## Prepare out-of-bag quantile regression
    if(!is.null(result$inbag.counts)) {
      inbag.counts <- simplify2array(result$inbag.counts)
      random.node.values.oob <- 0 * terminal.nodes
      random.node.values.oob[inbag.counts > 0] <- NA
      
      ## For each tree and observation select one random obs in the same node (not the same obs)
      for (tree in 1:num.trees){
        is.oob <- inbag.counts[, tree] == 0
        num.oob <- sum(is.oob)
        
        if (num.oob != 0) {
          oob.obs <- which(is.oob)
          oob.nodes <- terminal.nodes[oob.obs, tree]
          for (j in 1:num.oob) {
            idx <- terminal.nodes[, tree] == oob.nodes[j]
            idx[oob.obs[j]] <- FALSE
            random.node.values.oob[oob.obs[j], tree] <- save.sample(response[idx], size = 1)
          }
        }
      }
      
      ## Check num.trees
      minoob <- min(rowSums(inbag.counts == 0))
      if (minoob < 10) {
        stop("Error: Too few trees for out-of-bag quantile regression.")
      }
      
      ## Use the same number of values for all obs, select randomly
      result$random.node.values.oob <- t(apply(random.node.values.oob, 1, function(x) {
        sample(x[!is.na(x)], minoob)
      }))
    }
  }
  
  return(result)
}

integer.to.factor <- function(x, labels) {
  factor(x, levels = seq_along(labels), labels = labels)
}

## See help(sample)
save.sample <- function(x, ...) {
  x[sample.int(length(x), ...)]
}
