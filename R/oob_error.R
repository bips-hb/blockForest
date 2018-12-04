oob_error = function(rf, x, y, trees) {
  pred <- predict(rf, x, predict.all = TRUE)
  
  if (is.null(rf$inbag.counts)) {
    stop("Set 'keep.inbag = TRUE' when growing the RF.")
  }
  
  inbag <- simplify2array(rf$inbag.counts)
  oob <- inbag == 0
  
  majority_vote <- function(x) {
    xx <- table(x)
    as.numeric(names(which(rank(xx, ties.method = "random", na.last = FALSE) == length(xx))))
  } 
  
  oob_pred <- sapply(1:rf$num.samples, function(i) {
    if (any(oob[i, trees])) {
      if (rf$treetype == "Classification") {
        # Majority vote
        majority_vote(pred$predictions[i, trees][oob[i, trees]])
      } else if (rf$treetype == "Regression") {
        # Mean
        mean(pred$predictions[i, trees][oob[i, trees]])
      } else if (rf$treetype == "Survival") {
        # Sum CHF
        mean(colSums(pred$chf[i, , trees, drop = FALSE][1, , oob[i, trees], drop = FALSE]))
      } else {
        stop("Unknown treetype.")
      }
    } else {
      NA
    }
  })
  
  if (rf$treetype == "Classification") {
    # Missclassification
    res <- mean(oob_pred != as.numeric(y), na.rm = TRUE)
  } else if (rf$treetype == "Regression") {
    # MSE
    res <- mean((oob_pred - as.numeric(y))^2, na.rm = TRUE)
  } else if (rf$treetype == "Survival") {
    # 1 - C-index
    res <- 1 - survival::survConcordance(y ~ oob_pred)$concordance
  } else {
    stop("Unknown treetype.")
  }
  names(res) <- NULL
  res
}
