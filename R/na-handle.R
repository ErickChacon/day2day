#' @title Sum vector with NA values.
#'
#' @description
#' \code{sum_na} Sum vector with NA values.
#'
#' @details
#' details.
#'
#' @param x vector
#' @param ... Parameters passed to \code{sum}
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
#' @param x vector
#' @param ... Parameters passed to \code{mean}
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
#' \code{cumsum_na} Cumulative sum for vectors with NA values.
#'
#' @details
#' When missing values are present, \code{cumsum_na} replace missing values for 0 and
#' compute the usual \code{cumsum}.
#'
#' @param x The vector for which the cumulative sum is desired.
#' @param ... Additional arguments for the \code{cumsum} function.
#'
#' @author Erick A. Chacon-Montalvan
#'
#' @export
cumsum_na <- function (x, ...) {
  x[is.na(x)] <- 0
  cumsum(x, ...)
}
