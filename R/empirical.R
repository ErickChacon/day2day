
#' @title Compute empirical cumulative density function.
#'
#' @description
#' \code{ecdf_val} Computes the empirical quantile using \code{ecdf} funcion.
#'
#' @details
#' details.
#'
#' @param x Vector of realizations.
#'
#' @return vector of quantiles.
#'
#' @author Erick A. Chacon-Montalvan
#'
#' @importFrom stats ecdf
#'
#' @export
ecdf_val <- function (x) {
  ecdff <- ecdf(x)
  return(ecdff(x))
}
