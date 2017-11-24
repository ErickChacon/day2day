
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
      lmer1 <- lme4::lmer(y ~ (1 | z), REML = F)
      lmer2 <- lme4::lmer(y ~ x + (1 | z), REML = F)
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

gen_assoc <- function(x, y) {
  cl <- c(class(x), class(y))
  if (cl[1] == "factor") {
    if (cl[2] == "factor") {
      tab <- table(x, y)
      return(vcd::assocstats(tab)$cramer)
    } else {
      return(summary(lm(y ~ x))$r.squared)
    }
  } else if (cl[2] != "factor") {
    mo1 <- summary(gam(y ~ s(x)))$dev.expl
    mo2 <- summary(gam(x ~ s(y)))$dev.expl
    return((mo1 + mo2) / 2)
  } else {
    mo <- summary(lm( x ~ y))
    return(summary(lm(x ~ y))$r.squared)
  }
}


#' @title Matrix of General Association
#'
#' @description
#' \code{assoc} computes a matrix of general association between categorical and
#' quantitative variables.
#'
#' @details
#' Given a \code{data.frame}, it will compute a matrix of association between all the
#' variables.
#'
#' @param data Data frame with covariates
#' @param re Logical values, indicating if random effects should be included or not.
#'
#' @return A matrix of the pairwise association.
#'
#' @author Erick Albacharro Chacon-Montalvan
#'
#' @examples
#' library(lattice)
#' assoc_mat <- assoc(iris)
#' levelplot(assoc_mat)
#'
#' @importFrom vcd assocstats
#' @importFrom stats lm
#' @importFrom lme4 lmer
#'
#' @export
assoc <- function(data, re = NULL) {
  metadata <- expand.grid(x = 1:ncol(data), y = 1:ncol(data))
  metadata$assoc_ind <- NA
  index <- metadata$y > metadata$x
  if (is.null(re)) {
    metadata$assoc_ind[index] <-
      apply(subset(metadata, index), 1,
            function(x) gen_assoc(data[, x[1]], data[, x[2]]))
  } else {
    metadata$assoc_ind[index] <-
      apply(subset(metadata, index), 1,
            function(x) test_assoc_re(data[, x[1]], data[, x[2]], re))
  }
  assoc_ind <- matrix(metadata$assoc_ind, ncol(data), ncol(data))
  assoc_ind[is.na(assoc_ind)] <- t(assoc_ind)[is.na(assoc_ind)]
  diag(assoc_ind) <- 1
  rownames(assoc_ind) <- colnames(assoc_ind) <- names(data)
  return(assoc_ind)
}
