
#' @title Simulate databases based in models
#'
#' @description
#' \code{sim_model} simulate a database based on common models. The structure
#' used to create the data is similar as the \code{bamlss.formula}.
#'
#' @param formula List of the parameters, indicating how they should be computed.
#' similar to formula for \code{lm}, \code{glm}, \code{bamlss}, with the difference
#' that it included the coefficients and link function explicitly.
#'
#' @param generator Function to generate the response variables given the parameters
#'
#' @param n Number of observations to be simulated
#'
#' @param seed Seed to be defined with function \code{set.seed} to obtain reproducible
#' results
#'
#' @return a \code{tibble} containing the simulated predictors, parameters and response
#' variable
#'
#' @author Erick Albacharro Chacon-Montalvan
#'
#' @examples
#'f <- list(
#'  mean ~ 5 + 0.5 * x1 + 0.1 * x2 + 0.7 * id1,
#'  sd ~ exp(x1)
#')
#'data <- sim_model(f, rnorm, 100)
#'
#' @export
#'

sim_model <- function (formula = list(mean ~ 1 + 2 * x1, sd ~ 1),
                       generator = rnorm, n = 1000, seed = NULL) {

  if (!is.null(seed))
    set.seed(seed)

  params <- purrr::map_chr(formula, ~ all.vars(.)[1])
  predictors <- purrr::map(formula, ~ all.vars(.)[-1]) %>%
    purrr::reduce(c) %>% unique()
  p <- length(predictors)

  data <- matrix(rnorm(n * p), nrow = n) %>%
    tibble::as_tibble() %>%
    setNames(predictors)

  data[params] <- purrr::map(formula, ~ .[[3]]) %>%
    purrr::map(~ eval(., data))
  data$y <- do.call(generator, c(n = n, data[params]))

  return(data)
}



exp_cor <- function (dis, phi) {
  exp(-dis/phi)
}
exp_cov <- function (dis, phi, sigma2) {
  sigma2 * exp(-dis/phi)
}

#' @title Simulate a Gaussian process
#'
#' @description
#' \code{gp} Simulate a spatial Gaussian process given a certain covariance function.
#'
#' @details
#' details.
#'
#' @param s1 First coordinate
#'
#' @param s2 Second coordinate
#'
#' @param cov.model A character or function indicating the covariance function that
#' Should be used to compute the variance-covariance matrix
#'
#' @param cov.params A list of the parameters required by the \code{cov.model} function.
#'
#' @return A vector of the realization of the Gaussian Process
#'
#' @author Erick A. Chacon-Montalvan
#'
#' @examples
#' # Generate coordinates
#' N <- 1000
#' s1 <- 2 * runif(N)
#' s2 <- 2 * runif(N)
#' # Simulate and plot the realization of a Gaussian process
#' y <- gp(s1, s2, "exp_cov", params)
#' plot(x, y, cex = y)
#'
#' @export
gp <- function (s1, s2, cov.model = NULL, cov.params = NULL) {
  coords <- cbind(s1, s2)
  n <- nrow(coords)
  distance <- as.matrix(dist(coords))
  varcov <- do.call(cov.model, c(list(dis = distance), cov.params))
  right <- chol(varcov)
  output <- as.numeric(crossprod(right, rnorm(n)))
}
