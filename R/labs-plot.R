#' @title Create labels with units.
#'
#' @description
#' \code{units_tx} creates labels for considering singular or plural values.
#'
#' @details
#' details.
#'
#' @param vec Vector of values.
#' @param unit Name of unit in plural
#' @param singular Name of the unit in singular.
#'
#' @return return.
#'
#' @author Erick A. Chacon-Montalvan
#'
#' @examples
#'
#' x <- sample(1:3, size = 10, replace = TRUE)
#' units_tx(x)
#' units_tx(x, unit = "grams", singular = "grams")
#'
#' @export
units_tx <- function (vec, unit = "weeks", singular = NULL) {
  if (is.null(singular)) singular <- substr(unit, 1, nchar(unit) - 1)
  vec <- as.character(vec)
  is_sing <- (vec == "1") + 1
  add_unit <- c(unit, singular)[is_sing]
  paste(vec, add_unit)
}
