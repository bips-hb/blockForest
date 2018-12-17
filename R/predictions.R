# -------------------------------------------------------------------------------
#   This file is part of blockForest.
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
# along with blockForest. If not, see <http://www.gnu.org/licenses/>.
#
# Written by: Roman Hornung, Marvin N. Wright
# -------------------------------------------------------------------------------

##' @export
predictions <- function(x, ...)  UseMethod("predictions")

##' Extract predictions of blockForest prediction object.
##'
##'
##' @title blockForest predictions
##' @param x blockForest prediction object.
##' @param ... Further arguments passed to or from other methods.
##' @return Predictions: Classes for Classification forests, Numerical values for Regressions forests and the estimated survival functions for all individuals for Survival forests.
##' @seealso \code{\link{blockForest}}
##' @author Marvin N. Wright
##' @aliases predictions
##' @export
predictions.blockForest.prediction <- function(x, ...) {
  if (class(x) != "blockForest.prediction") {
    stop("Object ist no blockForest.prediction object.")
  }
  if (x$treetype == "Classification" || x$treetype == "Regression" || x$treetype == "Probability estimation") {
    if (is.null(x$predictions)) {
      stop("No predictions found.")
    } else {
      return(x$predictions)
    }
  } else if (x$treetype == "Survival") {
    if (is.null(x$survival)) {
      stop("No predictions found.")
    } else {
      return(x$survival)
    }
  } else {
    stop("Unknown tree type.")
  }
}

##' Extract training data predictions of blockForest object.
##'
##'
##' @title blockForest predictions
##' @param x blockForest object.
##' @param ... Further arguments passed to or from other methods.
##' @return Predictions: Classes for Classification forests, Numerical values for Regressions forests and the estimated survival functions for all individuals for Survival forests.
##' @seealso \code{\link{blockForest}}
##' @author Marvin N. Wright
##' @export
predictions.blockForest<- function(x, ...) {
  if (class(x) != "blockForest") {
    stop("Object ist no blockForest object.")
  }
  if (x$treetype == "Classification" || x$treetype == "Regression" || x$treetype == "Probability estimation") {
    if (is.null(x$predictions)) {
      stop("No predictions found.")
    } else {
      return(x$predictions)
    }
  } else if (x$treetype == "Survival") {
    if (is.null(x$survival)) {
      stop("No predictions found.")
    } else {
      return(x$survival)
    }
  } else {
    stop("Unknown tree type.")
  }
}
