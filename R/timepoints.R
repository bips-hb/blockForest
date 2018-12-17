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
timepoints <- function(x, ...)  UseMethod("timepoints")

##' Extract unique death times of blockForest Survival prediction object.
##'
##'
##' @title blockForest timepoints
##' @param x blockForest Survival prediction object.
##' @param ... Further arguments passed to or from other methods.
##' @return Unique death times
##' @seealso \code{\link{blockForest}}
##' @author Marvin N. Wright
##' @export
timepoints.blockForest.prediction <- function(x, ...) {
  if (class(x) != "blockForest.prediction") {
    stop("Object ist no blockForest.prediction object.")
  }
  if (x$treetype != "Survival") {
    stop("No timepoints found. Object is no Survival prediction object.")
  }
  if (is.null(x$unique.death.times)) {
    stop("No timepoints found.")
  }
  return(x$unique.death.times)
}

##' Extract unique death times of blockForest Survival forest
##'
##'
##' @title blockForest timepoints
##' @param x blockForest Survival forest object.
##' @param ... Further arguments passed to or from other methods.
##' @return Unique death times
##' @seealso \code{\link{blockForest}}
##' @author Marvin N. Wright
##' @aliases timepoints
##' @export
timepoints.blockForest <- function(x, ...) {
  if (class(x) != "blockForest") {
    stop("Object ist no blockForest object.")
  }
  if (x$treetype != "Survival") {
    stop("No timepoints found. Object is no Survival forest.")
  }
  if (is.null(x$unique.death.times)) {
    stop("No timepoints found.")
  }
  return(x$unique.death.times)
}
