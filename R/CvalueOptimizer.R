
##' @title CvalueOptimizer class
##' @description Virtual class for Random forest. 
##' Contains all fields and methods used in all Forest subclasses.
##' @importFrom parallel mclapply
##' @import methods
CvalueOptimizer <- 
  setRefClass("CvalueOptimizer", 
              fields = list(
                num_treesorder = "integer",
                nsets = "integer",
                num_treesoptim = "integer",
                mtrys = "integer", 
                min_node_size = "integer", 
                splitrule = "character",
                data = "data.frame",
                trees = "list",
                treetype = "character",
                replace = "logical",
                block = "list"),
              methods = list(
                
                optimizeCvalues = function() {
                  ## Empty virtual function
                })
              
  )
