
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
test_assoc_re <- function(x, y, z) {
  index <- !is.na(x) & !is.na(y) & !is.na(z)
  y <- y[index]
  x <- x[index]
  z <- z[index]
  cl <- c(class(x), class(y))
  if (cl[1] == "factor") {
    if (cl[2] == "factor") {
      return(chisq.test(x, y)$p.value)
    } else {
      lmer1 <- lmer(y ~ (1 | z), REML = F)
      lmer2 <- lmer(y ~ x + (1 | z), REML = F)
      return(anova(lmer1, lmer2)$"Pr(>Chisq)"[2])
      # mo <- summary(lm( y ~ x))
      # return(pf(mo$fstatistic[1], mo$fstatistic[2], mo$fstatistic[3],
                # lower.tail = FALSE))
    }
  } else if (cl[2] != "factor") {
    mo <- gam(y ~ s(x))
    # mo <- gam(y ~ s(x, bs="ts", k = 3))
    return(summary(mo)$s.table[, 4])
  } else {
    lmer1 <- lmer(x ~ (1 | z), REML = F)
    lmer2 <- lmer(x ~ y + (1 | z), REML = F)
    return(anova(lmer1, lmer2)$"Pr(>Chisq)"[2])
    # mo <- summary(lm( x ~ y))
    # return(pf(mo$fstatistic[1], mo$fstatistic[2], mo$fstatistic[3],
              # lower.tail = FALSE))
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
assoc <- function(data, re = NULL) {
  metadata <- expand.grid(x = 1:ncol(data), y = 1:ncol(data))
  metadata$pvalue <- NA
  index <- metadata$y > metadata$x
  if (is.null(re)) {
    metadata$pvalue[index] <-
      apply(subset(metadata, index), 1,
            function(x) test_assoc(data[, x[1]], data[, x[2]]))
  } else {
    metadata$pvalue[index] <-
      apply(subset(metadata, index), 1,
            function(x) test_assoc_re(data[, x[1]], data[, x[2]], re))
  }
  pvalue <- 1 - matrix(metadata$pvalue, ncol(data), ncol(data))
  pvalue[is.na(pvalue)] <- t(pvalue)[is.na(pvalue)]
  diag(pvalue) <- 1
  rownames(pvalue) <- colnames(pvalue) <- names(data)
  return(pvalue)
}
