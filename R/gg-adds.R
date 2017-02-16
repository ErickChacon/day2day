#' @title Wrap text for ggplots.
#'
#' @description
#' \code{wrapit} wrap long texts to be used with ggplot package.
#'
#' @param text long text to be wrapped
#' @param width width of the wrap
#'
#' @return wrapped text
#'
#' @author Erick A. Chacon-Montalvan
#'
#' @export
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

#' @title Creates sequence for continuous scale .
#'
#' @description
#' \code{ggscale_seq} Creates a function that can be provide to compute
#' break for a continuous scale axis.
#'
#' @details
#' details.
#'
#' @param by desired step of the sequence as in \code{\link{seq}}.
#'
#' @return a function that can be provided to \code{\link{scale_x_continuos}}
#' or similar ones.
#'
#' @author Erick A. Chacon-Montalvan
#'
#' @export
ggscale_seq <- function (by = 1) {
  fun <- function (x) seq(ceiling(min(x)), floor(max(x)), by = by)
  return(fun)
}

###########################################################################
## Title: PLOT GAM EFFECTS MODEL
## Desc: From http://stackoverflow.com/questions/19735149/is-it-possible-to-plot-the-smooth-components-of-a-gam-fit-with-ggplot2
## Input: input
## Output: output
## Status: running
## Author: Erick Albacharro Chacon-Montalvan
## Date: 09 Feb 2017
###########################################################################

ggplot.model <- function(model, type = "conditional", res = FALSE,
                         col.line = "#7fc97f", col.point = "#beaed4",
                         size.line=1, size.point=1) {
  require(visreg)
  require(dplyr)
  plotdata <- visreg(model, type = type, plot = FALSE)
  if (is.data.frame(plotdata[[1]])) plotdata <- list(plotdata)
  smooths <- ldply(plotdata, function(part)
    data.frame(Variable = part$meta$x,
             x=part$fit[[part$meta$x]],
             smooth=part$fit$visregFit,
             lower=part$fit$visregLwr,
             upper=part$fit$visregUpr))
  residuals <- ldply(plotdata, function(part)
    data.frame(Variable = part$meta$x,
               x=part$res[[part$meta$x]],
               y=part$res$visregRes))
  if (res)
    ggplot(smooths, aes(x, smooth)) + geom_line(col=col.line, size=size.line) +
      geom_line(aes(y=lower), linetype="dashed", col=col.line, size=size.line) +
      geom_line(aes(y=upper), linetype="dashed", col=col.line, size=size.line) +
      geom_point(data = residuals, aes(x, y), col=col.point, size=size.point) +
      facet_grid(. ~ Variable, scales = "free_x")
  else
    ggplot(smooths, aes(x, smooth)) + geom_line(col=col.line, size=size.line) +
      geom_line(aes(y=lower), linetype="dashed", col=col.line, size=size.line) +
      geom_line(aes(y=upper), linetype="dashed", col=col.line, size=size.line) +
      facet_grid(. ~ Variable, scales = "free_x")
  }

#' @title Customized theme for ggplot.
#'
#' @description
#' \code{ggtheme} creates a customized theme for ggplot.
#'
#' @details
#' details.
#'
#' @param color
#'
#' @return theme for being used with ggplot objects.
#'
#' @author Erick A. Chacon-Montalvan
#'
#' @export
ggtheme <- function (color = "#990000") {
  out <- theme(
    axis.title = element_text(face = "bold", colour = chicol, size = 8),
    axis.text = element_text(size = 7),
    plot.title = element_text(size = rel(1), hjust = 0.5))
}
