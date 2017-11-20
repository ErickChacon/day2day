#' @title Obtain list of objects by memory usage.
#'
#' @description
#' \code{ls_size} obtain the list of objects with their memory usage.
#'
#' @param units unit to be printed as with \code{object.size} function.
#'
#' @return list of objects sorted by memory usage.
#'
#' @author Erick A. Chacon-Montalvan
#'
#' @importFrom utils object.size
#'
#' @export

ls_size <- function (units = "MB") {
  obs <- ls(envir = .GlobalEnv)
  index <- order(sapply(obs, function(x) {object.size(get(x))}))
  size <- sapply(obs, function(x) {format(object.size(get(x)), units = units)})
  return(size[index])
}
