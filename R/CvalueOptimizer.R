##' @importFrom parallel mclapply
##' @import methods
CvalueOptimizer <- 
  setRefClass("CvalueOptimizer", 
              fields = list(
                nsets = "integer",
                num.trees.pre = "integer",
                mtry = "integer", 
                data = "data.frame",
                trees = "list",
                treetype = "character",
                blocks = "list",
                block.method = "character", 
                splitrule = "character"),
              methods = list(
                
                optimizeCvalues = function(...) {
                  ## Empty virtual function
                })
              
  )
