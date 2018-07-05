
##' @title Classification forest class
##' @description Subclass for classification forest. 
##' Contains all fields and methods used special for classification forests.
CvalueOptimizerSurvival <- 
  setRefClass("CvalueOptimizerSurvival", 
              contains = "CvalueOptimizer", 
              fields = list(), 
              methods = list(
                
                optimizeCvalues = function() {
                  
                  if(length(num.threads)==0)
                    num.threads <- NULL
                  
                  if(block.method=="block_forest") {
                    
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
                      
                      forest <- blockForest(y ~ ., data = data, num.trees = num_treesoptim, replace = replace, sample.fraction = sample.fraction,
                                            blocks = block,
                                            block.weights = cvalues,
                                            mtry = mtry, keep.inbag = TRUE, splitrule=splitrule, block.method=block.method, num.threads=num.threads)
                      
                      errs[l] <- forest$prediction.error
                      
                    }
                    
                    # Optimized vector of c values:
                    cvalues <- cvaluesall[[which.max.random(-errs)]]
                    
                    return(cvalues)
                    
                  }
                  
                  if(block.method=="block_select_weights") {
                    
                    pm <- sapply(block, length)
                    
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
                      
                      cvalues <- sapply(pm, function(x) sample(c(runif(1, 0, sqrt(x)/x), runif(1, sqrt(x)/x, 1)), size=1))
                      cvaluesall[[l]] <- cvalues  # split.select.weights
                      
                      splitweights <- rep(NA, sum(pm))
                      for(blocki in seq(along=block))
                        splitweights[block[[blocki]]] <- cvalues[blocki]
                      
                      forest <- blockForest(y ~ ., data = data, num.trees = num_treesoptim, replace = replace, sample.fraction = sample.fraction,
                                            split.select.weights = splitweights,
                                            mtry = mtry, keep.inbag = TRUE, splitrule=splitrule, block.method=block.method, num.threads=num.threads)
                      
                      errs[l] <- forest$prediction.error
                      
                    }
                    
                    # Optimized vector of c values:
                    cvalues <- cvaluesall[[which.max.random(-errs)]]
                    
                    return(cvalues)
                    
                  }
                  
                  if(block.method=="weights_only") {
                    
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
                      
                      forest <- blockForest(y ~ ., data = data, num.trees = num_treesoptim, replace = replace, sample.fraction = sample.fraction,
                                            blocks = block,
                                            block.weights = cvalues,
                                            mtry = mtry, keep.inbag = TRUE, splitrule=splitrule, block.method=block.method, num.threads=num.threads)
                      
                      errs[l] <- forest$prediction.error
                      
                    }
                    
                    # Optimized vector of c values:
                    cvalues <- cvaluesall[[which.max.random(-errs)]]
                    
                    return(cvalues)
                    
                  }
                  
                  
                  if(block.method=="sample_from_blocks") {
                    
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
                      
                      forest <- blockForest(y ~ ., data = data, num.trees = num_treesoptim, replace = replace, sample.fraction = sample.fraction,
                                            blocks = block,
                                            block.weights = cvalues,
                                            mtry = mtry, keep.inbag = TRUE, splitrule=splitrule, block.method=block.method, num.threads=num.threads)
                      
                      errs[l] <- forest$prediction.error
                      
                    }
                    
                    # Optimized vector of c values:
                    cvalues <- cvaluesall[[which.max.random(-errs)]]
                    
                    return(cvalues)
                    
                  }
                  
                  
                  
                  if(block.method=="one_block_per_split") {
                    
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
                      
                      cvalues <- diff(c(0, sort(runif(M-1)), 1))
                      cvaluesall[[l]] <- cvalues  
                      
                      forest <- blockForest(y ~ ., data = data, num.trees = num_treesoptim, replace = replace, sample.fraction = sample.fraction,
                                            blocks = block,
                                            block.weights = cvalues,
                                            mtry = mtry, keep.inbag = TRUE, splitrule=splitrule, block.method=block.method, num.threads=num.threads)
                      
                      errs[l] <- forest$prediction.error
                      
                    }
                    
                    # Optimized vector of c values:
                    cvalues <- cvaluesall[[which.max.random(-errs)]]
                    
                    return(cvalues)
                    
                  }
                  
                })
              
  )
