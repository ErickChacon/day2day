#' @title PNG custom.
#'
#' @description
#' \code{png2} is a customized version of \code{png}.
#'
#' @details
#' details.
#'
#' @param par.
#'
#' @return return.
#'
#' @author Erick A. Chacon-Montalvan
#'
#' @export
png2 <- function (filename = "Rplot%03d.png", width = 1000, ratio = 1, ...) {
  png(filename, width = 1000, height = width * ratio, ...)
}
