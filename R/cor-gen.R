
corr_gen <- function(x, y) {
  cl <- c(class(x), class(y))
  if (cl[1] == "factor") {
    if (cl[2] == "factor") {
      k1 <- length(table(x)) - 1
      r1 <- length(table(y)) - 1
      n <- sum(table(x, y))
      # denom <- min(length(table(x)), length(table(y))) - 1
      # return(sqrt(chisq.test(x, y)$statistic / length(x) / denom))
      # denom <- min(sum(!is.na(x)), sum(!is.na(y))) - 1
      # cramers v with biased correction
      adj <- max(0, chisq.test(x, y)$statistic / n - k1 * r1 / (n - 1))
      return(sqrt(adj / min(k1, r1)))
      # return(chisq.test(x, y)$p.value)
    } else {
      mo <- summary(lm( y ~ x))
      return(sqrt(max(0, mo$adj.r.squared)))
      # return(pf(mo$fstatistic[1], mo$fstatistic[2], mo$fstatistic[3],
                # lower.tail = FALSE))
    }
  } else if (cl[2] != "factor") {
    mo <- gam(y ~ s(x))
    # mo <- gam(y ~ s(x, bs="ts", k = 3))
    return(sqrt(max(0, summary(mo)$r.sq)))
    # return(summary(mo)$s.table[, 4])
  } else {
    mo <- summary(lm( x ~ y))
    return(sqrt(max(0, mo$adj.r.squared)))
    # return(pf(mo$fstatistic[1], mo$fstatistic[2], mo$fstatistic[3],
    #           lower.tail = FALSE))
  }
} 
#' @title title.
#' 
#' @description
#' \code{function} description. 
#' 
#' @details
#' details.
#' 
#' @param par.
#' 
#' @return return.
#' 
#' @author Erick Albacharro Chacon-Montalvan
#'
#' @export
corr <- function(data) {
  metadata <- as.data.frame(expand.grid(1:ncol(data), 1:ncol(data)))
  metadata$corr <- apply(metadata, 1,
                         function(x) corr_gen(data[, x[1]], data[, x[2]]))
  corr <- matrix(metadata$corr, ncol(data), ncol(data))
  # diag(corr) <- 1
  rownames(corr) <- colnames(corr) <- names(data)
  return(corr)
}
