
wrapit <- function(text, width = 20) {
  wtext <- paste(strwrap(text, width = width), collapse=" \n ")
  return(wtext)
}

#' @title Efficient 2d binnary summaries.
#'
#' @description
#' \code{condense2} creates a two dimensional binnary summary with bigvis
#' package.
#'
#' @details
#' \code{condense2} is compatible with dplyr.
#'
#' @param x the first variable on data to be binned.
#' @param y the second variable on date to be binned.
#' @param var varible to be summarised on x, y bins.
#' @param data data.frame where the variables exist.
#' @param width vector of bin widths for \code{\link{bin}} function.
#' @param origin vector for \code{\link{bin}} function.
#' @param ... additional options passed to \code{\link{condense}} function.
#'
#' @return condense object.
#'
#' @author Erick A. Chacon-Montalvan
#'
#' @export
condense2 <- function (x, y, var = NULL, data, width, origin, ...) {
  if (!require(bigvis)) {
   stop("package bigvis is required.")
  }
  xbin <- substitute(bin(x, width[1], origin[1]), list(x = substitute(x)))
  xbin <- eval(xbin, data)
  ybin <- substitute(bin(y, width[2], origin[2]), list(y = substitute(y)))
  ybin <- eval(ybin, data)
  if (!is.null(substitute(var))) {
    zbin <- substitute(condense(xbin, ybin, z = var, ...),
                       list(var = substitute(var)))
  } else {
    zbin <- substitute(condense(xbin, ybin, ...))
  }
  return(eval(zbin, data))
}
