#' @title Sum vector with NA values.
#'
#' @description
#' \code{sum_na} Sum vector with NA values.
#'
#' @details
#' details.
#'
#' @param x
#' @param ...
#'
#' @author Erick A. Chacon-Montalvan
#'
#' @export
sum_na <- function (x, ...) {
  sum(x, na.rm = TRUE, ...)
}


#' @title Sum vector with NA values.
#'
#' @description
#' \code{mean_na} Mean vector with NA values.
#'
#' @details
#' details.
#'
#' @param x
#' @param ...
#'
#' @author Erick A. Chacon-Montalvan
#'
#' @export
mean_na <- function (x, ...) {
  mean(x, na.rm = TRUE, ...)
}


#' @title Cumsum vector with NA values.
#'
#' @description
#' \code{cumsum_na} Cumsum vector with NA values.
#'
#' @details
#' details.
#'
#' @param x
#'
#' @author Erick A. Chacon-Montalvan
#'
#' @export
cumsum_na <- function (x, ...) {
  x[is.na(x)] <- 0
  cumsum(x, ...)
}
