#' @title Convert Numeric to Factor with custom condition
#'
#' @description
#' \code{cut_ext} converts a numeric object to factor given the breaks and
#' condition applied to the high threshold of each interval.
#'
#' @details
#' details.
#'
#' @param x numeric vector to be converted to factor.
#'        breaks numeric vector of breaks to be included.
#'        condition condition to be applied to the high threshold of interval.
#'
#' @return The new factor is returned.
#'
#' @author Erick A. Chacon-Montalvan
#'
#' @export
cut_ext <- function (x, breaks, condition = NULL) {
  # Initialize variables.
  n <- length(breaks)
  if (is.null(condition)) condition <- rep('<', n)
  con_fun <- function (i) {
    out <- eval(parse(text = paste("x", condition[i], breaks[i])))
    out <- !out
  }
  # Transform to vector.
  out <- apply(sapply(1:n, con_fun), 1, sum)
  high_lab <- c(')', ']')[match(condition, c('<', '<='))]
  high_lab <- c(high_lab, ')')
  low_lab <- c('[', '(')[match(condition, c('<', '<='))]
  low_lab <- c('(', low_lab)
  breaks <- c(-Inf, breaks, Inf)
  out <- factor(out, labels = paste0(low_lab, breaks[1:(n+1)], ',',
                                     breaks[2:(n+2)], high_lab))
  return(out)
}
