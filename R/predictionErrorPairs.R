predictionErrorPairs = function(forest, pairsunknown, cvaluesb, data) {
    
  errors <- list()
  
  for(i in 1:nrow(pairsunknown)) {
    
    firstsmaller <- sapply(cvaluesb, function(x) x[pairsunknown[i,1]] <= x[pairsunknown[i,2]])
    
    error1 <- oob_error(forest, x = data[,names(data)!="y"], y = data$y, trees = which(firstsmaller)) 
    error2 <- oob_error(forest, x = data[,names(data)!="y"], y = data$y, trees = which(!firstsmaller)) 
    
    errors[[i]] <- c(error1, error2)
    
  }	  
  
  return(errors)
  
}
