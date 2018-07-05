
##' @title CvalueOptimizer class
##' @description Virtual class for Random forest. 
##' Contains all fields and methods used in all Forest subclasses.
##' @importFrom parallel mclapply
##' @import methods
CvalueOptimizer <- 
  setRefClass("CvalueOptimizer", 
              fields = list(
                nsets = "integer",
                num_treesoptim = "integer",
                mtry = "integer", 
                min_node_size = "integer", 
                splitrule = "character",
                data = "data.frame",
                trees = "list",
                treetype = "character",
                replace = "logical",
				sample.fraction = "numeric",
                block = "list",
				block.method = "character",
				num.threads = "integer"),
              methods = list(
                
                optimizeCvalues = function() {
                  ## Empty virtual function
                })
              
  )
