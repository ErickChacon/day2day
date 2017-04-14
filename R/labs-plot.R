#' @title Create labels with units.
#'
#' @description
#' \code{units_tx} creates labels for considering singular or plural values.
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
units_tx <- function (vec, unit = "weeks", singular = "week") {
  vec <- as.character(vec)
  is_sing <- (vec == "1") + 1
  add_unit <- c(unit, singular)[is_sing]
  paste(vec, add_unit)
}
