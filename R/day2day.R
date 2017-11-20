#' @title \code{day2day} package
#'
#' @description
#' Functions that can be useful in the day-to-day data analysis. It comprehends
#' functions to find paths for projects, make summaries of databases inside
#' folder and so on.
#'
#' @docType package
#' @name day2day
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
# if(getRversion() >= "2.15.1") utils::globalVariables(c("."))
if(getRversion() >= "2.15.1") utils::globalVariables(
  c(".",
    "varname", "value", "number", "id",
    "weeks", "data0", "GA", "pzero", "cdf_gamma", "spi"
    )
  )

data0 <- NULL
