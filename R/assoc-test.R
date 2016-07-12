
test_assoc <- function(x, y) {
  cl <- c(class(x), class(y))
  if (cl[1] == "factor") {
    if (cl[2] == "factor") {
      return(chisq.test(x, y)$p.value)
    } else {
      mo <- summary(lm( y ~ x))
      return(pf(mo$fstatistic[1], mo$fstatistic[2], mo$fstatistic[3],
                lower.tail = FALSE))
    }
  } else if (cl[2] != "factor") {
    mo <- gam(y ~ s(x))
    # mo <- gam(y ~ s(x, bs="ts", k = 3))
    return(summary(mo)$s.table[, 4])
  } else {
    mo <- summary(lm( x ~ y))
    return(pf(mo$fstatistic[1], mo$fstatistic[2], mo$fstatistic[3],
              lower.tail = FALSE))
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
assoc <- function(data) {
  metadata <- as.data.frame(expand.grid(1:ncol(data), 1:ncol(data)))
  metadata$pvalue <- apply(metadata, 1,
                           function(x) test_assoc(data[, x[1]], data[, x[2]]))
  corr <- 1 - matrix(metadata$pvalue, ncol(data), ncol(data))
  diag(corr) <- 1
  rownames(corr) <- colnames(corr) <- names(data)
  return(corr)
}
