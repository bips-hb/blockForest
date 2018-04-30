##' Implements Random Forests (Breiman 2001) with emphasis on simplicity. 
##' Uses reference classes and only plain \code{R}. 
##' Not optimized for computation speed. 
##' Allows rapid prototyping of RF-type algorithms.
##' 
##' @title blockfor
##' @param X matrix. covariate matrix. observations in rows, variables in columns.
##' @param y target variable. See 'Example.R'. 
##' @param num_trees Number of trees in block forest.
##' @param mtrys Vector of length equal to the number of blocks. For each block: number of variables to possibly split at in each node. Leave unspecified.
##' @param min_node_size Minimal node size. Default 1 for classification, 5 for regression and 3 for survival.
##' @param replace Sample with replacement. Default TRUE.
##' @param probability Grow a probability forest. Default FALSE.
##' @param splitrule Splitrule to use in trees. Default "Gini" for classification forests, "Variance" for regression and probability forests and "Logrank" for survival forests.
##' @param block list. A list of length equal to the number M of blocks considered. Each
##' entry contains the vector of column indices in 'X' of the covariates in one of the M blocks.
##' @param num_treesorder B_order in Supplementary Data
##' @param nsets N_sets in Supplementary Data
##' @param num_treesoptim B_optim in Supplementary Data
##' @examples 
##' \donttest{
##' library(blockForest)
##' 
##' # Classification
##' blockfor(Species ~ ., iris)
##' 
##' # Prediction
##' train_idx <- sample(nrow(iris), 2/3 * nrow(iris))
##' iris_train <- iris[train_idx, ]
##' iris_test <- iris[-train_idx, ]
##' rf_iris <- blockfor(Species ~ ., data = iris_train)
##' pred_iris <- rf_iris$predict(iris_test)
##' table(iris_test$Species, pred_iris)
##' }
##' 
##' @author Marvin N. Wright
##' @references
##' Breiman, L. (2001). Random forests. Mach Learn, 45(1), 5-32. \cr
##' @export
blockfor <- 
  function(X, y, num_trees = 50, mtrys = NULL,
           min_node_size = NULL, replace = TRUE, probability = FALSE, 
           splitrule = NULL, block, num_treesorder = 500, nsets = 100, num_treesoptim = 100) {
    
    ##require("ranger")
    
    if(class(y)=="matrix") {
      if(ncol(y)==2) {
        require("survival")
        y <- Surv(time=y[,1], event=y[,2])
        model.data <- data.frame(y, X)
      }
    }
    else
      model.data <- data.frame(y, X)
    
    if (class(model.data[, 1]) == "factor") {
      if (probability) {
        treetype <- "Probability" 
      } else {
        treetype <- "Classification"
      }
    } else if (class(model.data[, 1]) == "numeric") {
      treetype <- "Regression"
    } else if (class(model.data[, 1]) == "Surv") {
      treetype <- "Survival"
    } else {
      stop("Unkown response type.")
    }
    
    if(missing(block))                         
      stop("Argument 'block' must be provided.")
    
    ## Check parameters
    if (is.null(mtrys)) {
      mtrys <- sapply(block, function(x) sqrt(length(x)))
    } ##else if (mtry > ncol(model.data)-1) {
    ##stop("Mtry cannot be larger than number of independent variables.")
    ##}
    if (is.null(min_node_size)) {
      if (treetype == "Classification") {
        min_node_size <- 1
      } else if (treetype == "Probability") {
        min_node_size <- 1
      } else if (treetype == "Regression") {
        min_node_size <- 5
      } else if (treetype == "Survival") {
        min_node_size <- 3
      }
    }
    
    ## Splitrule
    if (is.null(splitrule)) {
      if (treetype == "Classification") {
        splitrule <- "gini"
      } else if (treetype == "Probability") {
        splitrule <- "Variance"
      } else if (treetype == "Regression") {
        splitrule <- "variance"
      } else if (treetype == "Survival") {
        splitrule <- "logrank"
      }
    }
    
    ## Factors to numeric
    model.data[, -1] <- sapply(model.data[, -1] , as.numeric)
    
    
    # Hier cvalues optimieren.
    
    
    ## Create forest object
    if (treetype == "Classification") {
      
      cvalueoptim <- CvalueOptimizerClassification$new(num_treesorder = as.integer(num_treesorder), nsets = as.integer(nsets), num_treesoptim = as.integer(num_treesoptim),
                                                       mtrys = as.integer(mtrys), 
                                                       min_node_size = as.integer(min_node_size), 
                                                       replace = replace, splitrule = splitrule,
                                                       data = model.data, 
                                                       response_levels = levels(model.data[, 1]), 
                                                       block = block)
      
      cvalues <- cvalueoptim$optimizeCvalues()
      
      forest <- blockForest::blockForest(y ~ ., data = model.data, num.trees = num_trees,
                       blocks = block,
                       block.weights = cvalues,
                       mtry = mtrys, keep.inbag = TRUE, splitrule=splitrule)
    } else if (treetype == "Probability") {
      forest <- ForestProbability$new(num_trees = as.integer(num_trees), mtrys = as.integer(mtrys), 
                                      min_node_size = as.integer(min_node_size), 
                                      replace = replace, splitrule = splitrule,
                                      data = Data$new(data = model.data), 
                                      response_levels = levels(model.data[, 1]))
    } else if (treetype == "Regression") {
      
      cvalueoptim <- CvalueOptimizerRegression$new(num_treesorder = as.integer(num_treesorder), 
                                                   nsets = as.integer(nsets), num_treesoptim = as.integer(num_treesoptim), mtrys = as.integer(mtrys), 
                                                   min_node_size = as.integer(min_node_size), 
                                                   replace = replace, splitrule = splitrule,
                                                   data = model.data,
                                                   block = block)
      
      cvalues <- cvalueoptim$optimizeCvalues()

      forest <- blockForest::blockForest(y ~ ., data = model.data, num.trees = num_trees,
                       blocks = block,
                       block.weights = cvalues,
                       mtry = mtrys, keep.inbag = TRUE, splitrule=splitrule)
									 
    } else if (treetype == "Survival") {
      
      cvalueoptim <- CvalueOptimizerSurvival$new(num_treesorder = as.integer(num_treesorder), nsets = as.integer(nsets), num_treesoptim = as.integer(num_treesoptim),
                                                 mtrys = as.integer(mtrys), 
                                                 min_node_size = as.integer(min_node_size), 
                                                 replace = replace, splitrule = splitrule,
                                                 data = model.data, block = block)
      
      cvalues <- cvalueoptim$optimizeCvalues()
      
            forest <- blockForest::blockForest(y ~ ., data = model.data, num.trees = num_trees,
                       blocks = block,
                       block.weights = cvalues,
                       mtry = mtrys, keep.inbag = TRUE, splitrule=splitrule)
    } else {
      stop("Unkown tree type.")
    }
    
    res <- list(forest=forest, cvalues=cvalues, prederror=forest$prediction.error)	
    
    ## Return forest
    return(res) 
  }
