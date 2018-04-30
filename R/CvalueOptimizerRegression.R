
##' @title Classification forest class
##' @description Subclass for classification forest. 
##' Contains all fields and methods used special for classification forests.
CvalueOptimizerRegression <- 
  setRefClass("CvalueOptimizerRegression", 
              contains = "CvalueOptimizer", 
              fields = list(), 
              methods = list(
                
                optimizeCvalues = function() {
                  
                  
                  
                  # Number of blocks:
                  M <- length(block)
                  
                  
                  # All possible pairs of blocks:
                  allpairs <- t(combn(M, 2))
                  
                  
                  # Matrix in which, at each iteration, all pairs will be stored
                  # for which the ordering
                  # of the corresponding c values has already
                  # been determined:
                  pairsgiven <- matrix(nrow=0, ncol=2)
                  
                  
                  # Matrix in which, at each iteration, all pairs will be stored for which
                  # the ordering of the corresponding c values 
                  # is already known (different to 'pairsgiven', because
                  # if e.g. c_3 < c_1 and c_1 < c_5 have been determined
                  # one also knows that c_3 < c_5. c_3 < c_5 would
                  # not be contained in 'pairsgiven' because it has not
                  # been tested, but it is contained in 'allpairsgiven'
                  # because it is known):
                  allpairsgiven <- matrix(nrow=0, ncol=2)
                  
                  
                  # 'pairsunknown' will contain all pairs for which the ordering is
                  # not yet known:
                  if(nrow(pairsgiven)==0) {
                    pairsunknown <- allpairs
                  } else {
                    pairssort <- t(apply(allpairsgiven, 1, sort))
                    
                    keepind <- which(apply(allpairs, 1, function(x) sum(apply(pairssort, 1, function(y) (x[1]==y[1]) & (x[2]==y[2])))==0))
                    pairsunknown <- allpairs[keepind,,drop=FALSE]
                    
                  }
                  
                  
                  # Construct first forest without any restrictions on the ordering of
                  # the c values:
                  # Generate a random vector of c values for each tree:
                  cvaluesb <- replicate(num_treesorder, sample(c(sort(runif(M-1, 0, 1)), 1)))
                  cvaluesb <- split(cvaluesb, rep(1:ncol(cvaluesb), each = nrow(cvaluesb)))
                  names(cvaluesb) <- NULL
                  
                  
                  # Construct, forest using cvaluesb[b,] as c value vector
                  # for the bth tree:
                  forest <- blockForest::blockForest(y ~ ., data = data, num.trees = num_treesorder,
                                   blocks = block,
                                   block.weights = cvaluesb,
                                   mtry = mtrys, keep.inbag = TRUE, splitrule=splitrule)
                  
                  # For each pair of blocks, calculate the OOB error of the subset of trees for
                  # which the c value for the first block of the pair is larger
                  # and the OOB error of the subset of trees for which the c value
                  # for the second block of the pair is larger:
                  errs <- predictionErrorPairs(forest, pairsunknown=pairsunknown, cvaluesb=cvaluesb, data = data)
                  
                  # Determine that pair for which the absolute difference between
                  # the OOB errors is largest:
                  newpairind <- which.max(sapply(errs, function(x) abs(x[1]-x[2])))
                  
                  # Determine the correct ordering of the optimal
                  # pair determined above:
                  if(errs[[newpairind]][1] <= errs[[newpairind]][2])
                    newpair <- pairsunknown[newpairind,]
                  else
                    newpair <- pairsunknown[newpairind,2:1]
                  
                  
                  
                  # Add first determined pair to 'pairsgiven' (this pair was
                  # associated with the largest error difference):
                  
                  pairsgiven <- rbind(pairsgiven, newpair)
                  
                  
                  # Set of possible permutations:
                  if(M > 2) {
                    require("Deducer")
                    possibleperms <- t(apply(perm(c(setdiff(1:M, newpair), 99, 99)), 1, function(x) {
                      x[x==99] <- newpair
                      x
                    }))
                  }
                  else
                    possibleperms <- pairsgiven
                  
                  
                  
                  
                  # Determine the optimal ordering of the blocks with respect to their
                  # c values:
                  
                  while(nrow(possibleperms) > 1 & M > 2) {
                    
                    # Hier werden alle (Gruppen von) Tupeln ermittelt für die man die Ordnung schon kennt. 
                    # Die Gruppen von Tupeln sind überschneidungsfrei hinsichtlich der Blöcke darin.
                    # Bspw. befaenden sich "1,3" und "1,2" in derselben Tupelgruppe, aber "5,4" waere in
                    # einer anderen Tupelgruppe.
                    # Die Gruppen von Tupeln enthalten Tupel, die irgendwie zusammenhaengen und aus der
                    # mehr als ein möglicher Tupel zusammengebaut werden können (wird später gemacht),
                    # z.B. könnte "1,3" und "1,2" zu "1,3,2" zusammengebaut werden, aber auch zu "1,2,3".
                    
                    # Konkret wird in diesem Schritt wie folgt vorgegangen:
                    
                    # 1.) Wie oben gesagt enthält 'pairsgiven' alle Paare von Bloecken fuer die die Ordnung schon
                    # evaluiert wurde. In diesem Schritt wird ein Vektor 'tupleid' erzeugt der die Zugehoerigkeiten der
                    # Paare in 'pairsgiven' zu den einzelnen Tupel(gruppe)n enthaelt.
                    # 2.) Die einzelnen Tupelindizes ('unique(tupleid)') werden durchlaufen
                    # und die aktuellen Tupelgruppen aus den zugehörigen Paaren zusammengesetzt.
                    
                    tupleid <- rep(NA, nrow(pairsgiven))
                    
                    count <- 1
                    
                    tupleid[1] <- 1
                    currpair <- pairsgiven[1,]
                    
                    if(nrow(pairsgiven) > 1) {
                      
                      connected <- setdiff(1:nrow(pairsgiven), 1)[apply(pairsgiven[-1,,drop=FALSE], 1, function(x) any(currpair %in% x))]
                      
                      tupleid[connected] <- 1
                      
                      for(i in 2:nrow(pairsgiven)) {
                        
                        currpair <- pairsgiven[i,]
                        connected <- setdiff(1:nrow(pairsgiven), i)[apply(pairsgiven[-i,,drop=FALSE], 1, function(x) any(currpair %in% x))]
                        
                        if(any(!is.na(tupleid[connected])))
                          tupleid[i] <- tupleid[connected][!is.na(tupleid[connected])][1]
                        else {
                          count <- count+1
                          tupleid[i] <- count
                        }
                        
                      }
                      
                    }
                    
                    
                    tuplelonglist <- list()
                    
                    tupleids <- sort(unique(tupleid))
                    
                    for(k in seq(along=tupleids)) {
                      
                      tupletemp <- pairsgiven[tupleid==tupleids[k],,drop=FALSE]
                      
                      tuplelong <- as.list(as.data.frame(t(tupletemp)))
                      names(tuplelong) <- NULL
                      
                      
                      if(length(tuplelong) > 1) {
                        
                        stopnow <- FALSE
                        
                        while(!stopnow) {
                          
                          lastentry <- sapply(tuplelong, function(x) x[length(x)])
                          firstentry <- sapply(tuplelong, function(x) x[1])
                          
                          pairind <- sapply(lastentry, function(x) which(firstentry==x))
                          names(pairind) <- 1:length(lastentry)
                          
                          pairind <- pairind[sapply(pairind, function(x) length(x) >0)]
                          
                          if(length(pairind)==0) {
                            stopnow <- TRUE 
                          } else {
                            
                            pairind <- pairind[sapply(pairind, function(x) all(!(x %in% names(pairind))))]
                            
                            tuplelongsafe <- tuplelong
                            tuplelong <- list()
                            counttemp <- 1
                            
                            for(i in seq(along=pairind)) {
                              
                              for(j in seq(along=pairind[[i]])) {
                                tuplelong[[counttemp]] <- c(tuplelongsafe[[as.numeric(names(pairind)[i])]][1], tuplelongsafe[[pairind[[i]][j]]]) 
                                counttemp <- counttemp+1
                              }
                              
                            }
                            
                            restind <- setdiff(1:length(tuplelongsafe), c(as.numeric(names(pairind)), unlist(pairind)))
                            
                            for(i in seq(along=restind)) {
                              tuplelong[[counttemp]] <- tuplelongsafe[[restind[i]]]
                              counttemp <- counttemp+1
                            }
                            
                          }
                          
                        }
                        
                      }
                      
                      tuplelonglist[[k]] <- tuplelong
                      
                    }
                    
                    
                    
                    
                    # Liste der Paare updaten für die man die
                    # Ordnung schon kennt:
                    
                    # library("rlist")
                    tempobj <- rlist::list.flatten(tuplelonglist)
                    
                    
                    allpairsgiven <- matrix(nrow=0, ncol=2)
                    for(i in seq(along=tempobj)) {
                      
                      for(j in 1:(length(tempobj[[i]])-1)) {
                        for(k in (j+1):length(tempobj[[i]])) {
                          allpairsgiven <- rbind(tempobj[[i]][c(j,k)], allpairsgiven)
                        }
                      }
                      
                    }
                    allpairsgiven <- unique(allpairsgiven)
                    
                    
                    
                    # Paare 'pairsunknown' updaten für die man die Ordnung noch nicht kennt:
                    
                    if(nrow(pairsgiven)==0) {
                      pairsunknown <- allpairs
                    } else {
                      pairssort <- t(apply(allpairsgiven, 1, sort))
                      
                      keepind <- which(apply(allpairs, 1, function(x) sum(apply(pairssort, 1, function(y) (x[1]==y[1]) & (x[2]==y[2])))==0))
                      pairsunknown <- allpairs[keepind,,drop=FALSE]
                      
                    }
                    
                    
                    
                    
                    # Construct forest with restrictions given by 'allpairsgiven'
                    # on the ordering of the c values:
                    
                    # Generate a random vector of c values for each tree, where
                    # the vectors of c values adher to the restrictions on the
                    # ordering given by 'allpairsgiven':
                    
                    cvaluesb <- replicate(num_treesorder, {
                      as <- c(sort(runif(M-1)), 1)
                      cvaluestemp <- as[order(possibleperms[sample(1:nrow(possibleperms), size=1),])]
                      cvaluestemp  
                    })
                    cvaluesb <- split(cvaluesb, rep(1:ncol(cvaluesb), each = nrow(cvaluesb)))
                    names(cvaluesb) <- NULL
                    
                    
                    # Construct, forest using cvaluesb[b,] as c value vector
                    # for the bth tree:
                    
                    forest <- blockForest::blockForest(y ~ ., data = data, num.trees = num_treesorder,
                                     blocks = block,
                                     block.weights = cvaluesb,
                                     mtry = mtrys, keep.inbag = TRUE, splitrule=splitrule)
                    
                    # For each pair of blocks for which the ordering is not yet determined,
                    # calculate the OOB error of the subset of trees for
                    # which the c value for the first block of the pair is larger
                    # and the OOB error of the subset of trees for which the c value
                    # for the second block of the pair is larger:
                    
                    errs <- predictionErrorPairs(forest, pairsunknown=pairsunknown, cvaluesb=cvaluesb, data = data)
                    
                    # Determine that pair for which the absolute difference between
                    # the OOB errors is largest:
                    
                    newpairind <- which.max(sapply(errs, function(x) abs(x[1]-x[2])))
                    
                    # Determine the correct ordering of the optimal
                    # pair determined above:
                    
                    if(errs[[newpairind]][1] <= errs[[newpairind]][2])
                      newpair <- pairsunknown[newpairind,]
                    else
                      newpair <- pairsunknown[newpairind,2:1]
                    
                    
                    
                    # Exclude from the set of possible orderings of the 
                    # c values those for which the ordering determined
                    # for the new pair is not the one determined:
                    
                    toexcl <- apply(possibleperms, 1, function(x) which(x==newpair[1]) >  which(x==newpair[2]))
                    possibleperms <- possibleperms[!toexcl,,drop=FALSE]
                    
                    
                    # Add new pair to 'pairsgiven':
                    
                    pairsgiven <- rbind(pairsgiven, newpair)
                    
                  }
                  
                  
                  
                  # Determined order:
                  
                  morder <- possibleperms[1,]
                  
                  
                  # Simulate 'nsets' vectors of c values with the ordering
                  # given by morder,  for each of these construct a forest,
                  # and calculate the corresponding OOB error.
                  # Then use that vector of c values out of the ones generated
                  # for which the corresponding forest featured the smallest
                  # OOB error.
                  
                  errs <- 0
                  cvaluesall <- list()
                  
                  for(l in 1:nsets) {
                    
                    as <- c(sort(runif(M-1)), 1)
                    cvalues <- as[order(morder)]
                    cvaluesall[[l]] <- cvalues  
                    
                    forest <- blockForest::blockForest(y ~ ., data = data, num.trees = num_treesoptim,
                                     blocks = block,
                                     block.weights = cvalues,
                                     mtry = mtrys, keep.inbag = TRUE, splitrule=splitrule)
                    
                    errs[l] <- forest$prediction.error
                    
                  }
                  
                  # Optimized vector of c values:
                  cvalues <- cvaluesall[[which.max.random(-errs)]]
                  
                  return(cvalues)
                  
                })
              
  )
