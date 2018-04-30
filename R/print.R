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
# Written by:
#
#   Marvin N. Wright
# Institut fuer Medizinische Biometrie und Statistik
# Universitaet zu Luebeck
# Ratzeburger Allee 160
# 23562 Luebeck
# Germany
#
# http://www.imbs-luebeck.de
# -------------------------------------------------------------------------------

##' Print contents of blockForest object.
##'
##'
##' @title Print blockForest
##' @param x Object of class 'blockForest'.
##' @param ... Further arguments passed to or from other methods.
##' @seealso \code{\link{blockForest}}
##' @author Marvin N. Wright
##' @export
print.blockForest <- function(x, ...) {
  cat("blockForest result\n\n")
  cat("Call:\n", deparse(x$call), "\n\n")
  cat("Type:                            ", x$treetype, "\n")
  cat("Number of trees:                 ", x$num.trees, "\n")
  cat("Sample size:                     ", x$num.samples, "\n")
  cat("Number of independent variables: ", x$num.independent.variables, "\n")
  cat("Mtry:                            ", x$mtry, "\n")
  cat("Target node size:                ", x$min.node.size, "\n")
  cat("Variable importance mode:        ", x$importance.mode, "\n")
  if (x$treetype == "Survival") {
    cat("Splitrule:                       ", x$splitrule, "\n")
    cat("Number of unique death times:    ", length(x$unique.death.times), "\n")
  }
  if (x$treetype == "Classification") {
    cat("OOB prediction error:            ", sprintf("%1.2f %%", 100*x$prediction.error), "\n")
  } else if (x$treetype == "Regression") {
    cat("OOB prediction error (MSE):      ", x$prediction.error, "\n")
  } else {
    cat("OOB prediction error:            ", x$prediction.error, "\n")
  }
  if (x$treetype == "Regression") {
    cat("R squared (OOB):                 ", x$r.squared, "\n")
  }
}

##' Print contents of blockForest forest object.
##'
##'
##' @title Print blockForest forest
##' @param x Object of class 'blockForest.forest'.
##' @param ... further arguments passed to or from other methods.
##' @author Marvin N. Wright
##' @export
print.blockForest.forest <- function(x, ...) {
  cat("blockForest forest object\n\n")
  cat("Type:                         ", x$treetype, "\n")
  cat("Number of trees:              ", x$num.trees, "\n")
  if (x$treetype == "Survival") {
    cat("Number of unique death times: ", length(x$unique.death.times), "\n")
  }
}

##' Print contents of blockForest prediction object.
##'
##'
##' @title Print blockForest prediction
##' @param x Object of class 'blockForest.prediction'.
##' @param ... further arguments passed to or from other methods.
##' @author Marvin N. Wright
##' @export
print.blockForest.prediction <- function(x, ...) {
  cat("blockForest prediction\n\n")
  cat("Type:                            ", x$treetype, "\n")
  cat("Sample size:                     ", x$num.samples, "\n")
  cat("Number of independent variables: ", x$num.independent.variables, "\n")
  if (x$treetype == "Survival") {
    cat("Number of unique death times:    ", length(x$unique.death.times), "\n")
  }
}

str.blockForest.forest <- function(object, max.level = 2, ...) {
  class(object) <- "list"
  str(object, max.level = max.level, ...)
}

str.blockForest <- function(object, max.level = 2, ...) {
  class(object) <- "list"
  str(object, max.level = max.level, ...)
}
