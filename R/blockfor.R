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

##' Random Forest variants for block-structured covariate data
##'
##' Implements five Random Forest variants for prediction
##' of binary, survival and metric outcomes using block-structured covariate
##' data, for example, clinical covariates plus measurements of a certain omics data type
##' or multi-omics data, that is, data for which measurements of different types of omics data
##' and/or clinical data for each patient exist. For example, for the task of predicting
##' survival for each patient there might be available
##' clinical covariates, gene expression measurements, mutation data,
##' and copy number variation measurements. \cr
##' The group of covariates corresponding to one specific data type is denoted as a 'block'. \cr
##' NOTE: We strongly recommend using the variant "BlockForest" (or "block forest")
##' in applications. The other four variants performed worse than "BlockForest"
##' in the analysis by Hornung & Wright (2019). Using 20 real multi-omics data sets Hornung & Wright (2019) compared all
##' five variants with each other and with alternatives, in particular with Random Survival Forest as existing
##' reference method. The ranking of the performances of the five variants was as follows
##' in the comparison study by Hornung & Wright (2019): 1) "BlockForest", 2) "RandomBlock",
##' 3) "BlockVarSel", 4) "VarProb", 5) "SplitWeights". \cr
##' Each of the five variants uses a different split selection algorithm.
##' For details, see Hornung & Wright (2019). \cr
##' Note that this R package is a fork of the R package ranger. \cr
##' NOTE ALSO: Including the clinical block mandatorily in the split point selection can considerably improve the prediction performance.
##' Whether or not this is the case, depends on the level of predictive information contained in the clinical block.
##' We recommend trying out including the clinical block mandatorily to see, whether this improves prediction
##' performance in the particular application. Note that in the case of including the clinical block mandatorily
##' and having more than only one omics block, "RandomBlock" performed (slightly) better than "BlockForest" in the comparison study by Hornung & Wright (2019). 
##' Including the clinical block mandatorily can be performed by setting the function argument 'always.select.block'
##' of 'blockfor()' to the index of the clinical block (e.g., if the clinical block would be the second block in order, we would 
##' set always.select.block=2).
##' 
##' @param X Covariate matrix. observations in rows, variables in columns.
##' @param y Target variable. If the outcome is binary, this is a factor with
##' two levels. If the outcome is metric, this is a numeric vector. If the outcome
##' is a survival outcome, this is a matrix with two columns, where the first column
##' contains the vector of survival/censoring times (one for each observation) and the second column contains
##' the status variable, that has the value '1' if the corresponding time is
##' a survival time and '0' if that time is a censoring time.
##' @param blocks A list of length equal to the number M of blocks considered. Each
##' entry contains the vector of column indices in 'X' of the covariates in one of the M blocks.
##' @param block.method Forest variant to use. One of the following: "BlockForest" (default), "RandomBlock", "BlockVarSel", "VarProb", "SplitWeights".
##' The latter listing is ordered according to the performances of these variants in the comparison study by Hornung & Wright (2019),
##' with the best variant being listed first.
##' @param num.trees Number of trees in the forest.
##' @param mtry This is either a number specifying the number of variables sampled for each
##' split from all variables (for variants "VarProb" and "SplitWeights")
##' or a vector of length equal to the number of blocks, where the m-th entry of the
##' vector gives the number of variables to sample from block m (for variants "BlockForest", "RandomBlock", and "BlockVarSel").
##' The default values are sqrt(p_1) + sqrt(p_2) + ... sqrt(p_M) and (sqrt(p_1), sqrt(p_2), ..., sqrt(p_M)), respectively,
##' where p_m denotes the number of variables in the m-th block (m = 1, ..., M) and sqrt() denoted the square root function.
##' @param nsets Number of sets of tuning parameter values generated randomly in the optimization of the tuning parameters.
##' Each variant has a tuning parameter for each block, that is, there are M tuning parameters for each variant.
##' These tuning parameters are optimized in the following way: 1. Generate random sets of tuning parameter values
##' and measure there adequateness: For j = 1,..., nsets: a) Generate a random set of tuning parameter values;
##' b) Construct a forest (with num.trees.pre trees) using the set of tuning parameter values generated in a);
##' c) Record the out-of-bag (OOB) estimated prediction error of the forest constructed in b); 2. Use the set of tuning 
##' parameter values generated in 1. that is associated with the smallest OOB estimated prediction error.
##' @param num.trees.pre Number of trees in each forest constructed during the optimization of the tuning
##' parameter values, see 'nsets' for details.
##' @param splitrule Splitting rule. Default "extratrees" (for computational efficiency). For other options see \code{\link{blockForest}}.
##' @param always.select.block Number of block to make always available for splitting (e.g. clinical covariates).
##' @param ... Parameters passed to \code{blockForest}, such as \code{num.threads}, etc. See \code{\link{blockForest}} for details.
##'
##' @return
##' \code{blockfor} returns a list containing the following components: 
##' \item{forest}{ object of class \code{"blockForest"}. Constructed forest.  }
##' \item{paramvalues}{ vector of length M. Optimized tuning parameter value for each block. }
##' \item{biased_oob_error_donotuse}{ numeric. OOB estimated prediction error. NOTE: This estimate should not be used, because it is (highly) optimistic (i.e, too small), because the data set was used twice - for optimizing the tuning parameter values and for estimating the prediction error. Instead, cross-validation should be used to estimate the prediction error. }
##'
##' @examples 
##' # NOTE: There is no association between covariates and response for the
##' # simulated data below.
##' # Moreover, the input parameters of blockfor() are highly unrealistic
##' # (e.g., nsets = 10 is specified much too small).
##' # The purpose of the shown examples is merely to illustrate the
##' # application of blockfor().
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
##' 
##' # Block variable (list):
##' blocks <- rep(1:3, times=c(5, 30, 100))
##' blocks <- lapply(1:3, function(x) which(blocks==x))
##' 
##' # Binary outcome:
##' ybin <- factor(sample(c(0,1), size=40, replace=TRUE), levels=c(0,1))
##' 
##' # Survival outcome:
##' ysurv <- cbind(rnorm(40), sample(c(0,1), size=40, replace=TRUE))
##' 
##' # Application to binary outcome:
##' ################################
##' 
##' blockforobj <- blockfor(X, ybin, num.trees = 100, replace = TRUE, blocks=blocks,
##'                         nsets = 10, num.trees.pre = 50, splitrule="extratrees", 
##'                         block.method = "BlockForest")
##' # Tuning parameter estimates (see Hornung & Wright (2019)):
##' blockforobj$paramvalues
##' 
##' # Application to survival outcome:
##' ##################################
##' 
##' blockforobj <- blockfor(X, ysurv, num.trees = 100, replace = TRUE, blocks=blocks,
##'                         nsets = 10, num.trees.pre = 50, splitrule="extratrees", 
##'                         block.method = "BlockForest")
##' blockforobj$paramvalues
##' 
##' @author Roman Hornung, Marvin N. Wright
##' @references
##' \itemize{
##'   \item Hornung, R. & Wright, M. N. (2019) Block Forests: random forests for blocks of clinical and omics covariate data. To appear, original version available as Technical Report (Department of Statistics, University of Munich) at: \url{http://epub.ub.uni-muenchen.de/59631}.
##'   \item Breiman, L. (2001). Random forests. Mach Learn, 45(1), 5-32. \url{http://dx.doi.org/10.1023/A:1010933404324}. 
##'   \item Wright, M. N. & Ziegler, A. (2017). ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R. J Stat Softw 77:1-17. \url{http://dx.doi.org/10.18637/jss.v077.i01}.
##' }
##' @import survival
##' @export
blockfor <- 
  function(X, y, blocks, block.method = "BlockForest", num.trees = 2000, mtry = NULL, 
           nsets = 300, num.trees.pre = 1500, splitrule = "extratrees",
           always.select.block = 0, ...) {
    
    if(class(y)=="matrix") {
      if(ncol(y)==2) {
        y <- Surv(time=y[,1], event=y[,2])
        model.data <- data.frame(y, X)
      }
    }
    else
      model.data <- data.frame(y, X)
    
    if (class(model.data[, 1]) == "factor") {
        treetype <- "Classification"
    } else if (class(model.data[, 1]) == "numeric") {
      treetype <- "Regression"
    } else if (class(model.data[, 1]) == "Surv") {
      treetype <- "Survival"
    } else {
      stop("Unkown response type.")
    }
    
    if(missing(blocks))                         
      stop("Argument 'blocks' must be provided.")
    
    ## Check parameters
    if (is.null(mtry)) {
      if(block.method %in% c("SplitWeights", "VarProb"))
        mtry <- sum(sqrt(sapply(blocks, length)))
      else
        mtry <- sapply(blocks, function(x) sqrt(length(x)))
    }
    
    ## Factors to numeric
    model.data[, -1] <- sapply(model.data[, -1] , as.numeric)
    
    ## Set always.split.variables if SplitWeights/RandomBlock and always.select.block set
    if (always.select.block > 0 & block.method %in% c("SplitWeights", "RandomBlock")) {
      always.split.variables <- colnames(model.data[, -1])[blocks[[always.select.block]]]
    } else {
      always.split.variables <- NULL
    }
    
    ## Set mtry of block to maximum if BlockVarSel and always.select.block set
    if (always.select.block > 0 & block.method %in% c("BlockVarSel", "BlockForest")) {
      mtry[always.select.block] <- length(blocks[[always.select.block]])
    } 
    
    ## Convert survival data
    if (treetype == "Survival") {
      model.data <- data.frame(time = model.data[, 1][, 1], status = model.data[, 1][, 2], model.data[, -1])
    }
    
    ## Create forest object
    if (treetype == "Classification") {

      cvalueoptim <- CvalueOptimizerClassification$new(nsets = as.integer(nsets), num.trees.pre = as.integer(num.trees.pre),
                                                       mtry = as.integer(mtry), 
                                                       data = model.data, 
                                                       response_levels = levels(model.data[, 1]), 
                                                       blocks = blocks, block.method=block.method, 
                                                       splitrule = splitrule, 
                                                       always.select.block = as.integer(always.select.block))
      
      paramvalues <- cvalueoptim$optimizeCvalues(always.split.variables = always.split.variables, ...)

								        if(block.method!="VarProb")
        forest <- blockForest(dependent.variable.name = "y", data = model.data, num.trees = num.trees, 
                              blocks = blocks, block.weights = paramvalues,
                              mtry = mtry, keep.inbag = TRUE, 
                              block.method=block.method, splitrule = splitrule, 
                              always.split.variables = always.split.variables, ...)
      else {
        
        pm <- sapply(blocks, length)
        splitweights <- rep(NA, sum(pm))
        for(blocki in seq(along=blocks))
          splitweights[blocks[[blocki]]] <- paramvalues[blocki]

        forest <- blockForest(dependent.variable.name = "y", data = model.data, num.trees = num.trees, 
                              split.select.weights = splitweights,
                              mtry = mtry, keep.inbag = TRUE, 
                              block.method=block.method, splitrule = splitrule, ...)
							  
	        }
							
							
    } else if (treetype == "Regression") {
      
      cvalueoptim <- CvalueOptimizerRegression$new(nsets = as.integer(nsets), num.trees.pre = as.integer(num.trees.pre), mtry = as.integer(mtry), 
                                                   data = model.data,
                                                   blocks = blocks, block.method=block.method, 
                                                   splitrule = splitrule, 
                                                   always.select.block = as.integer(always.select.block))
      
      paramvalues <- cvalueoptim$optimizeCvalues(always.split.variables = always.split.variables, ...)

	        if(block.method!="VarProb")
        forest <- blockForest(dependent.variable.name = "y", data = model.data, num.trees = num.trees,
                              blocks = blocks, block.weights = paramvalues,
                              mtry = mtry, keep.inbag = TRUE, 
                              block.method=block.method, splitrule = splitrule, 
                              always.split.variables = always.split.variables, ...)
      else {
        
        pm <- sapply(blocks, length)
        splitweights <- rep(NA, sum(pm))
        for(blocki in seq(along=blocks))
          splitweights[blocks[[blocki]]] <- paramvalues[blocki]

        forest <- blockForest(dependent.variable.name = "y", data = model.data, num.trees = num.trees,
                              split.select.weights = splitweights,
                              mtry = mtry, keep.inbag = TRUE, 
                              block.method=block.method, splitrule = splitrule, ...)
							  
	        }
      
    } else if (treetype == "Survival") {
      
      cvalueoptim <- CvalueOptimizerSurvival$new(nsets = as.integer(nsets), num.trees.pre = as.integer(num.trees.pre),
                                                 mtry = as.integer(mtry), 
                                                 data = model.data, blocks = blocks, block.method=block.method, 
                                                 splitrule = splitrule, 
                                                 always.select.block = as.integer(always.select.block))
      
      paramvalues <- cvalueoptim$optimizeCvalues(always.split.variables = always.split.variables, ...)
      
      if(block.method!="VarProb")
        forest <- blockForest(dependent.variable.name = "time", status.variable.name = "status", 
                              data = model.data, num.trees = num.trees, 
                              blocks = blocks, block.weights = paramvalues,
                              mtry = mtry, keep.inbag = TRUE, 
                              block.method=block.method, splitrule = splitrule, 
                              always.split.variables = always.split.variables, ...)
      else {
        
        pm <- sapply(blocks, length)
        splitweights <- rep(NA, sum(pm))
        for(blocki in seq(along=blocks))
          splitweights[blocks[[blocki]]] <- paramvalues[blocki]

        forest <- blockForest(dependent.variable.name = "time", status.variable.name = "status", 
                              data = model.data, num.trees = num.trees, 
                              split.select.weights = splitweights,
                              mtry = mtry, keep.inbag = TRUE, 
                              block.method=block.method, splitrule = splitrule, ...)
        
      }
      
    } else {
      stop("Unkown tree type.")
    }
    
    res <- list(forest=forest, paramvalues=paramvalues, biased_oob_error_donotuse=forest$prediction.error)	
    
    ## Return forest
    return(res) 
  }
