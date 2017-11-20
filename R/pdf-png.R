#' @title PNG custom.
#'
#' @description
#' \code{png2} is a customized version of \code{png}.
#'
#' @param filename File name for the graph.
#' @param width Width of the png device.
#' @param ratio Ratio between height and width (height/width).
#' @param ... Additional parameters to be passed to \code{png}.
#'
#' @return return.
#'
#' @author Erick A. Chacon-Montalvan
#'
#' @importFrom grDevices png
#'
#' @export
png2 <- function (filename = "Rplot%03d.png", width = 1000, ratio = 1, ...) {
  png(filename, width = 1000, height = width * ratio, ...)
}
