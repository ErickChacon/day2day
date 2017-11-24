#' @title Obtain list of objects by memory usage.
#'
#' @description
#' \code{ls_size} obtain the list of objects with their memory usage.
#'
#' @param units Unit to be printed as with \code{object.size} function.
#' @param ... Additional parameters passed to \code{ls}.
#'
#' @return vector of objects size sorted by memory usage.
#'
#' @author Erick A. Chacon-Montalvan
#'
#' @examples
#'
#' x <- c("1", "b")
#' ls_size(units = "KB")
#'
#' @importFrom utils object.size
#'
#' @export

ls_size <- function (units = "MB", ...) {
  obs <- ls(envir = .GlobalEnv, ...)
  index <- order(sapply(obs, function(x) {object.size(get(x))}))
  size <- sapply(obs, function(x) {format(object.size(get(x)), units = units)})
  return(size[index])
}
