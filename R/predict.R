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
# along with blockForest. If not, see <http://www.gnu.org/licenses/>.
#
# Written by: Roman Hornung, Marvin N. Wright
# -------------------------------------------------------------------------------

predict.blockForest.forest <- function(object, data, predict.all = FALSE,
                                       num.trees = object$num.trees, 
                                       type = "response", se.method = "infjack",
                                       seed = NULL, num.threads = NULL,
                                       verbose = TRUE, inbag.counts = NULL, ...) {
  
  ## GenABEL GWA data
  if (inherits(data, "gwaa.data")) {
    snp.names <- data@gtdata@snpnames
    snp.data <- data@gtdata@gtps@.Data
    data <- data@phdata[, -1, drop = FALSE]
    gwa.mode <- TRUE
    variable.names <- c(names(data), snp.names)
  } else {
    snp.data <- as.matrix(0)
    gwa.mode <- FALSE
    variable.names <- colnames(data)
  }
  
  ## Check forest argument
  if (!inherits(object, "blockForest.forest")) {
    stop("Error: Invalid class of input object.")
  } else {
    forest <- object
  }
  if (is.null(forest$dependent.varID) || is.null(forest$num.trees) ||
      is.null(forest$child.nodeIDs) || is.null(forest$split.varIDs) ||
      is.null(forest$split.values) || is.null(forest$independent.variable.names) ||
      is.null(forest$treetype)) {
    stop("Error: Invalid forest object.")
  }
  if (forest$treetype == "Survival" && (is.null(forest$status.varID)  ||
                                        is.null(forest$chf) || is.null(forest$unique.death.times))) {
    stop("Error: Invalid forest object.")
  }
  
  ## Check for old ranger version
  if (length(forest$child.nodeIDs) != forest$num.trees || length(forest$child.nodeIDs[[1]]) != 2) {
    stop("Error: Invalid forest object. Is the forest grown in ranger version <0.3.9? Try to predict with the same version the forest was grown.")
  }
  
  ## Prediction type
  if (type == "response" || type == "se") {
    prediction.type <- 1
  } else if (type == "terminalNodes") {
    prediction.type <- 2
  } else if (type == "quantiles") {
    stop("Error: Apply predict() to the blockForest object instead of the $forest object to predict quantiles.")
  } else {
    stop("Error: Invalid value for 'type'. Use 'response', 'se', 'terminalNodes', or 'quantiles'.")
  }
  
  ## Type "se" only for certain tree types
  if (type == "se" && se.method == "jack" && forest$treetype != "Regression") {
    stop("Error: Jackknife standard error prediction currently only available for regression.")
  }
  if (type == "se" && se.method == "infjack") {
    if (forest$treetype == "Survival") {
      stop("Error: Infinitesimal jackknife standard error prediction not yet available for survival.")
    } else if (forest$treetype == "Classification") {
      stop("Error: Not a probability forest. Set probability=TRUE to use the infinitesimal jackknife standard error prediction for classification.")
    }
  }
  
  ## Type "se" requires keep.inbag=TRUE
  if (type == "se" && is.null(inbag.counts)) {
    stop("Error: No saved inbag counts in blockForest object. Please set keep.inbag=TRUE when calling blockForest.")
  }
  
  ## Set predict.all if type is "se"
  if (type == "se") {
    predict.all <- TRUE
  }
  
  ## Create final data
  if (forest$treetype == "Survival") {
    if (forest$dependent.varID > 0 && forest$status.varID > 1) {
      if (ncol(data) == length(forest$independent.variable.names)+2) {
        ## If alternative interface used and same data structure, don't subset data
        data.used <- data
      } else if (ncol(data) == length(forest$independent.variable.names)) {
        data.selected <- data[, forest$independent.variable.names, drop = FALSE]
        data.used <- cbind(0, 0, data.selected)
        variable.names <- c("time", "status", forest$independent.variable.names)
        forest$dependent.varID <- 0
        forest$status.varID <- 1
      } else {
        stop("Invalid prediction data. Include both time and status variable or none.")
      }
    } else {
      ## If formula interface used, subset data
      data.selected <- data[, forest$independent.variable.names, drop = FALSE]
      
      ## Arange data as in original data
      data.used <- cbind(0, 0, data.selected)
      variable.names <- c("time", "status", forest$independent.variable.names)
    }
    
    ## Index of no-recode variables
    idx.norecode <- c(-(forest$dependent.varID+1), -(forest$status.varID+1))
    
  } else {
    ## No survival
    if (ncol(data) == length(forest$independent.variable.names)+1 && forest$dependent.varID > 0) {
      ## If alternative interface used and same data structure, don't subset data
      data.used <- data
    } else {
      ## If formula interface used, subset data
      data.selected <- data[, forest$independent.variable.names, drop = FALSE]
      
      ## Arange data as in original data
      if (forest$dependent.varID == 0) {
        data.used <- cbind(0, data.selected)
        variable.names <- c("dependent", forest$independent.variable.names)
      } else if (forest$dependent.varID >= ncol(data)) {
        data.used <- cbind(data.selected, 0)
        variable.names <- c(forest$independent.variable.names, "dependent")
      } else {
        data.used <- cbind(data.selected[, 1:forest$dependent.varID],
                           0,
                           data.selected[, (forest$dependent.varID+1):ncol(data.selected)])
        variable.names <- c(forest$independent.variable.names[1:forest$dependent.varID],
                            "dependent",
                            forest$independent.variable.names[(forest$dependent.varID+1):length(forest$independent.variable.names)])
      }
    }
    
    ## Index of no-recode variables
    idx.norecode <- -(forest$dependent.varID+1)
  }
  
  ## Recode characters
  if (!is.matrix(data.used) && !inherits(data.used, "Matrix")) {
    char.columns <- sapply(data.used, is.character)
    data.used[char.columns] <- lapply(data.used[char.columns], factor)
  }
  
  ## Recode factors if forest grown 'order' mode
  if (!is.null(forest$covariate.levels) && !all(sapply(forest$covariate.levels, is.null))) {
    data.used[, idx.norecode] <- mapply(function(x, y) {
      if(is.null(y)) {
        x
      } else {
        new.levels <- setdiff(levels(x), y)
        factor(x, levels = c(y, new.levels))
      }
    }, data.used[, idx.norecode], forest$covariate.levels, SIMPLIFY = !is.data.frame(data.used[, idx.norecode]))
  }
  
  ## Convert to data matrix
  if (is.matrix(data.used) || inherits(data.used, "Matrix")) {
    data.final <- data.used
  } else {
    data.final <- data.matrix(data.used)
  }
  
  
  ## If gwa mode, add snp variable names
  if (gwa.mode) {
    variable.names <- c(variable.names, snp.names)
  }
  
  ## Check missing values
  if (any(is.na(data.final))) {
    offending_columns <- colnames(data.final)[colSums(is.na(data.final)) > 0]
    stop("Missing data in columns: ",
         paste0(offending_columns, collapse = ", "), ".", call. = FALSE)
  }
  
  if (sum(!(forest$independent.variable.names %in% variable.names)) > 0) {
    stop("Error: One or more independent variables not found in data.")
  }
  
  ## Num threads
  ## Default 0 -> detect from system in C++.
  if (is.null(num.threads)) {
    num.threads = 0
  } else if (!is.numeric(num.threads) || num.threads < 0) {
    stop("Error: Invalid value for num.threads")
  }
  
  ## Seed
  if (is.null(seed)) {
    seed <- runif(1 , 0, .Machine$integer.max)
  }
  
  if (forest$treetype == "Classification") {
    treetype <- 1
  } else if (forest$treetype == "Regression") {
    treetype <- 3
  } else if (forest$treetype == "Survival") {
    treetype <- 5
  } else if (forest$treetype == "Probability estimation") {
    treetype <- 9
  } else {
    stop("Error: Unknown tree type.")
  }
  
  ## Defaults for variables not needed
  dependent.variable.name <- "none"
  mtry <- 0
  importance <- 0
  min.node.size <- 0
  split.select.weights <- list(c(0, 0))
  use.split.select.weights <- FALSE
  always.split.variables <- c("0", "0")
  use.always.split.variables <- FALSE
  status.variable.name <- "status"
  prediction.mode <- TRUE
  write.forest <- FALSE
  replace <- TRUE
  probability <- FALSE
  unordered.factor.variables <- c("0", "0")
  use.unordered.factor.variables <- FALSE
  save.memory <- FALSE
  splitrule <- 1
  alpha <- 0
  minprop <- 0
  case.weights <- c(0, 0)
  use.case.weights <- FALSE
  keep.inbag <- FALSE
  sample.fraction <- 1
  holdout <- FALSE
  num.random.splits <- 1
  blocks <- list()
  block.weights <- list()
  block.method <- 1
  
  ## Use sparse matrix
  if (inherits(data.final, "dgCMatrix")) {
    sparse.data <- data.final
    data.final <- matrix(c(0, 0))
    use.sparse.data <- TRUE
  } else {
    sparse.data <- Matrix(matrix(c(0, 0)))
    use.sparse.data <- FALSE
  }
  
  ## Call Ranger
  result <- rangerCpp(treetype, dependent.variable.name, data.final, variable.names, mtry,
                      num.trees, verbose, seed, num.threads, write.forest, importance,
                      min.node.size, split.select.weights, use.split.select.weights,
                      always.split.variables, use.always.split.variables,
                      status.variable.name, prediction.mode, forest, snp.data, replace, probability,
                      unordered.factor.variables, use.unordered.factor.variables, save.memory, splitrule,
                      case.weights, use.case.weights, predict.all, keep.inbag, sample.fraction,
                      alpha, minprop, holdout, prediction.type, 
                      num.random.splits, sparse.data, use.sparse.data,
                      blocks, block.weights, block.method)
  
  if (length(result) == 0) {
    stop("User interrupt or internal error.")
  }
  
  ## Prepare results
  result$num.samples <- nrow(data.final)
  result$treetype <- forest$treetype
  
  if (predict.all) {
    if (forest$treetype %in% c("Classification", "Regression")) {
      if (is.list(result$predictions)) {
        result$predictions <- do.call(rbind, result$predictions)
      } else {
        result$predictions <- array(result$predictions, dim = c(1, length(result$predictions)))
      }
    } else {
      ## TODO: Better solution for this?
      result$predictions <- aperm(array(unlist(result$predictions), 
                                        dim = rev(c(length(result$predictions), 
                                                    length(result$predictions[[1]]), 
                                                    length(result$predictions[[1]][[1]])))))
    }
  } else {
    if (is.list(result$predictions)) {
      result$predictions <- do.call(rbind, result$predictions)
    } 
  }
  
  if (type == "response") {
    if (forest$treetype == "Classification" && !is.null(forest$levels)) {
      if (!predict.all) {
        result$predictions <- integer.to.factor(result$predictions, forest$levels)
      }
    } else if (forest$treetype == "Regression") {
      ## Empty
    } else if (forest$treetype == "Survival") {
      result$unique.death.times <- forest$unique.death.times
      result$chf <- result$predictions
      result$predictions <- NULL
      result$survival <- exp(-result$chf)
    } else if (forest$treetype == "Probability estimation" && !is.null(forest$levels)) {
      if (!predict.all) {
        if (is.vector(result$predictions)) {
          result$predictions <- matrix(result$predictions, nrow = 1)
        }
        
        ## Set colnames and sort by levels
        colnames(result$predictions) <- forest$levels[forest$class.values]
        result$predictions <- result$predictions[, forest$levels, drop = FALSE]
      }
    }
  } else if (type == "terminalNodes") {
    if (is.vector(result$predictions)) {
      result$predictions <- matrix(result$predictions, nrow = 1)
    }
  }
  
  ## Compute Jackknife
  if (type == "se") {
    ## Aggregated predictions
    if (length(dim(result$predictions)) > 2) {
      yhat <- apply(result$predictions, c(1, 2), mean)
    } else {
      yhat <- rowMeans(result$predictions)
    }
    
    ## Get inbag counts, keep only observations that are OOB at least once
    inbag.counts <- simplify2array(inbag.counts) 
    if (is.vector(inbag.counts)) {
      inbag.counts <- t(as.matrix(inbag.counts))
    }
    inbag.counts <- inbag.counts[rowSums(inbag.counts == 0) > 0, , drop = FALSE] 
    n <- nrow(inbag.counts)
    oob <- inbag.counts == 0
    if (num.trees != object$num.trees) {
      oob <- oob[, 1:num.trees]
    }
    
    if (all(!oob)) {
      stop("Error: No OOB observations found, consider increasing num.trees or reducing sample.fraction.")
    }
    
    if (se.method == "jack") {
      ## Compute Jackknife
      oob.count <- rowSums(oob)
      jack.n <- sweep(tcrossprod(result$predictions, oob), 
                      2, oob.count, "/", check.margin = FALSE)
      if (is.vector(jack.n)) {
        jack.n <- t(as.matrix(jack.n))
      }
      if (any(oob.count == 0)) {
        n <- sum(oob.count > 0)
        jack.n <- jack.n[, oob.count > 0]
      } 
      jack <- (n - 1) / n * rowSums((jack.n - yhat)^2)
      bias <- (exp(1) - 1) * n / result$num.trees^2 * rowSums((result$predictions - yhat)^2)
      jab <- pmax(jack - bias, 0)
      result$se <- sqrt(jab)
    } else if (se.method == "infjack") {
      if (forest$treetype == "Regression") {
        infjack <- rInfJack(pred = result$predictions, inbag = inbag.counts, used.trees = 1:num.trees)
        result$se <- sqrt(infjack$var.hat)
      } else if (forest$treetype == "Probability estimation") {
        infjack <- apply(result$predictions, 2, function(x) {
          rInfJack(x, inbag.counts)$var.hat
        })
        result$se <- sqrt(infjack)
      } 
    } else {
      stop("Error: Unknown standard error method (se.method).")
    }
    
    ## Response as predictions
    result$predictions <- yhat
    
    if (forest$treetype == "Probability estimation") {
      ## Set colnames and sort by levels
      colnames(result$predictions) <- forest$levels[forest$class.values]
      result$predictions <- result$predictions[, forest$levels, drop = FALSE]
    }
  }
  
  class(result) <- "blockForest.prediction"
  return(result)
}

##' This function is to be applied to the entry 'forest' of the output of
##' \code{\link{blockfor}}. See the example section for illustration.
##'
##' For \code{type = 'response'} (the default), the predicted classes (classification), predicted numeric values (regression), predicted probabilities (probability estimation) or survival probabilities (survival) are returned. 
##' For \code{type = 'se'}, the standard error of the predictions are returned (regression only). The jackknife-after-bootstrap or infinitesimal jackknife for bagging is used to estimate the standard errors based on out-of-bag predictions. See Wager et al. (2014) for details.
##' For \code{type = 'terminalNodes'}, the IDs of the terminal node in each tree for each observation in the given dataset are returned.
##' For \code{type = 'quantiles'}, the selected quantiles for each observation are estimated. See Meinshausen (2006) for details.
##' 
##' If \code{type = 'se'} is selected, the method to estimate the variances can be chosen with \code{se.method}. Set \code{se.method = 'jack'} for jackknife-after-bootstrap and \code{se.method = 'infjack'} for the infinitesimal jackknife for bagging.
##' 
##' For classification and \code{predict.all = TRUE}, a factor levels are returned as numerics.
##' To retrieve the corresponding factor levels, use \code{rf$forest$levels}, if \code{rf} is the ranger object.
##' 
##' @title Prediction using Random Forest variants for block-structured covariate data
##' @param object \code{blockForest} object.
##' @param data New test data of class \code{data.frame} or \code{gwaa.data} (GenABEL).
##' @param predict.all Return individual predictions for each tree instead of aggregated predictions for all trees. Return a matrix (sample x tree) for classification and regression, a 3d array for probability estimation (sample x class x tree) and survival (sample x time x tree).
##' @param num.trees Number of trees used for prediction. The first \code{num.trees} in the forest are used.
##' @param type Type of prediction. One of 'response', 'se', 'terminalNodes', 'quantiles' with default 'response'. See below for details.
##' @param se.method Method to compute standard errors. One of 'jack', 'infjack' with default 'infjack'. Only applicable if type = 'se'. See below for details.
##' @param quantiles Vector of quantiles for quantile prediction. Set \code{type = 'quantiles'} to use.
##' @param seed Random seed. Default is \code{NULL}, which generates the seed from \code{R}. Set to \code{0} to ignore the \code{R} seed. The seed is used in case of ties in classification mode.
##' @param num.threads Number of threads. Default is number of CPUs available.
##' @param verbose Verbose output on or off.
##' @param ... further arguments passed to or from other methods.
##' @return Object of class \code{blockForest.prediction} with elements
##'   \tabular{ll}{
##'       \code{predictions}    \tab Predicted classes/values (only for classification and regression)  \cr
##'       \code{unique.death.times} \tab Unique death times (only for survival). \cr
##'       \code{chf} \tab Estimated cumulative hazard function for each sample (only for survival). \cr
##'       \code{survival} \tab Estimated survival function for each sample (only for survival). \cr
##'       \code{num.trees}   \tab Number of trees. \cr
##'       \code{num.independent.variables} \tab Number of independent variables. \cr
##'       \code{treetype}    \tab Type of forest/tree. Classification, regression or survival. \cr
##'       \code{num.samples}     \tab Number of samples.
##'   }
##'
##' @examples 
##' # NOTE: There is no association between covariates and response for the
##' # simulated data below.
##' # Moreover, the input parameters of blockfor() are highly unrealistic
##' # (e.g., nsets = 10 is specified much too small).
##' # The purpose of the shown examples is merely to illustrate the
##' # application of predict.blockForest().
##' 
##' 
##' # Generate data:
##' ################
##' 
##' set.seed(1234)
##' 
##' # Covariate matrix:
##' X <- cbind(matrix(nrow=40, ncol=5, data=rnorm(40*5)), 
##'            matrix(nrow=40, ncol=30, data=rnorm(40*30, mean=1, sd=2)),
##'            matrix(nrow=40, ncol=100, data=rnorm(40*100, mean=2, sd=3)))
##' colnames(X) <- paste("X", 1:ncol(X), sep="")
##' 
##' # Block variable (list):
##' block <- rep(1:3, times=c(5, 30, 100))
##' block <- lapply(1:3, function(x) which(block==x))
##' 
##' # Binary outcome:
##' ybin <- factor(sample(c(0,1), size=40, replace=TRUE), levels=c(0,1))
##' 
##' # Survival outcome:
##' ysurv <- cbind(rnorm(40), sample(c(0,1), size=40, replace=TRUE))
##' 
##' 
##' 
##' # Divide in training and test data:
##' 
##' Xtrain <- X[1:30,]
##' Xtest <- X[31:40,]
##' 
##' ybintrain <- ybin[1:30]
##' ybintest <- ybin[31:40]
##' 
##' ysurvtrain <- ysurv[1:30,]
##' ysurvtest <- ysurv[31:40,]
##' 
##' 
##' 
##' 
##' # Binary outcome: Apply algorithm to training data and obtain predictions
##' # for the test data:
##' #########################################################################
##' 
##' # Apply a variant to the training data:
##' 
##' blockforobj <- blockfor(Xtrain, ybintrain, num.trees = 100, replace = TRUE, block=block,
##'                         nsets = 10, num.trees.pre = 50, splitrule="extratrees", 
##'                         block.method = "SplitWeights")
##' blockforobj$paramvalues
##' 
##' 
##' # Obtain prediction for the test data:
##' 
##' (predres <- predict(blockforobj$forest, data = Xtest, block.method = "SplitWeights"))
##' predres$predictions
##' 
##' 
##' 
##' # Survival outcome: Apply algorithm to training data and obtain predictions
##' # for the test data:
##' ###########################################################################
##' 
##' # Apply a variant to the training data:
##' 
##' blockforobj <- blockfor(Xtrain, ysurvtrain, num.trees = 100, replace = TRUE, block=block,
##'                         nsets = 10, num.trees.pre = 50, splitrule="extratrees", 
##'                         block.method = "SplitWeights")
##' blockforobj$paramvalues
##' 
##' 
##' # Obtain prediction for the test data:
##' 
##' (predres <- predict(blockforobj$forest, data = Xtest, block.method = "SplitWeights"))
##' rowSums(predres$chf)
##'
##' @references
##' \itemize{
##'   \item Wright, M. N. & Ziegler, A. (2017). ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R. J Stat Softw 77:1-17. \doi{10.18637/jss.v077.i01}.
##'   \item Wager, S., Hastie T., & Efron, B. (2014). Confidence Intervals for Random Forests: The Jackknife and the Infinitesimal Jackknife. J Mach Learn Res 15:1625-1651. \url{https://jmlr.org/papers/v15/wager14a.html}.
##'   \item Meinshausen (2006). Quantile Regression Forests. J Mach Learn Res 7:983-999. \url{https://www.jmlr.org/papers/v7/meinshausen06a.html}.  
##'   }
##' @seealso \code{\link{blockForest}}
##' @author Marvin N. Wright
##' @export
predict.blockForest <- function(object, data = NULL, predict.all = FALSE,
                                num.trees = object$num.trees,
                                type = "response", se.method = "infjack",
                                quantiles = c(0.1, 0.5, 0.9), 
                                seed = NULL, num.threads = NULL,
                                verbose = TRUE, ...) {
  forest <- object$forest
  if (is.null(forest)) {
    stop("Error: No saved forest in blockForest object. Please set write.forest to TRUE when calling blockForest.")
  }
  if (object$importance.mode %in% c("impurity_corrected", "impurity_unbiased")) {
    warning("Forest was grown with 'impurity_corrected' variable importance. For prediction it is advised to grow another forest without this importance setting.")
  }
  
  if (type == "quantiles") {
    ## Quantile prediction
    if (object$treetype != "Regression") {
      stop("Error: Quantile prediction implemented only for regression outcomes.")
    }
    if (is.null(object$random.node.values)) {
      stop("Error: Set quantreg=TRUE in blockForest(...) for quantile prediction.")
    }
    
    if (is.null(data)) {
      ## OOB prediction
      if (is.null(object$random.node.values.oob)) {
        stop("Error: Set keep.inbag=TRUE in blockForest(...) for out-of-bag quantile prediction or provide new data in predict(...).")
      }
      node.values <- object$random.node.values.oob
    } else {
      ## New data prediction
      terminal.nodes <- predict(object, data, type = "terminalNodes")$predictions
      node.values <- 0 * terminal.nodes
      for (tree in 1:num.trees) {
        node.values[, tree] <- object$random.node.values[terminal.nodes[, tree], tree]
      }
    }
    
    ## Prepare results
    result <- list(num.samples = nrow(node.values),
                   treetype = object$treetype,
                   num.independent.variables = object$num.independent.variables,
                   num.trees = num.trees)
    class(result) <- "blockForest.prediction"
    
    ## Compute quantiles of distribution
    result$predictions <- t(apply(node.values, 1, quantile, quantiles, na.rm=TRUE))
    if (nrow(result$predictions) != result$num.samples) {
      ## Fix result for single quantile
      result$predictions <- t(result$predictions)
    }
    colnames(result$predictions) <- paste("quantile=", quantiles)
    result
  } else {
    ## Non-quantile prediction
    if (is.null(data)) {
      stop("Error: Argument 'data' is required for non-quantile prediction.") 
    }
    predict(forest, data, predict.all, num.trees, type, se.method, seed, num.threads, verbose, object$inbag.counts, ...)
  }
}
