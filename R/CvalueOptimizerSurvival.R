
##' @title Classification forest class
##' @description Subclass for classification forest. 
##' Contains all fields and methods used special for classification forests.
CvalueOptimizerSurvival <- 
  setRefClass("CvalueOptimizerSurvival", 
              contains = "CvalueOptimizer", 
              fields = list(), 
              methods = list(
                
                optimizeCvalues = function() {

                  # Number of blocks:
                  M <- length(block)

                  # Simulate 'nsets' vectors of c values with the ordering
                  # given by morder,  for each of these construct a forest,
                  # and calculate the corresponding OOB error.
                  # Then use that vector of c values out of the ones generated
                  # for which the corresponding forest featured the smallest
                  # OOB error.
                  
                  errs <- 0
                  cvaluesall <- list()
				                    
                  for(l in 1:nsets) {
                    
                    cvalues <- sample(c(sort(runif(M-1)), 1))
                    cvaluesall[[l]] <- cvalues  

                    forest <- blockForest::blockForest(y ~ ., data = data, num.trees = num_treesoptim,
                                     blocks = block,
                                     block.weights = cvalues,
                                     mtry = mtrys, keep.inbag = TRUE, splitrule=splitrule)
                    
                    errs[l] <- forest$prediction.error
                    
                  }
				  
				  #mycvaluesall <<- cvaluesall
				  #myerrs <<- errs
                  
				  #for(i in seq(along=cvaluesall))
				  #cat(paste("cvalues: ", paste(cvaluesall[[i]], collapse=","), ", err: ", errs[i], sep=""), "\n")
				  
                  # Optimized vector of c values:
                  cvalues <- cvaluesall[[which.max.random(-errs)]]
                  
                  return(cvalues)
                  
                })
              
  )
