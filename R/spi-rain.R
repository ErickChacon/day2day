
###########################################################################
## Title: Compute Standardized Precipitation Index
## Desc: Function to compute the spi index with gamlss models.
## It can also with rain series containing NA, 0 or only non-zero values.
## Input: vector of weekly rain (running mean computed by the user).
## Output: vector of weekly spi
## Status: working, workaround for gamlss package
## Author: Erick Albacharro Chacon-Montalvan
## Date: 12 Apr 2017
###########################################################################

spi_week <- function(rain, period = 365 / 7, package = "gamlss", plot = FALSE) {

  require(mgcv)

  # Create dataframe.
  data <- data.frame(rain, weeks = 1:length(rain))
  data <- transform(data, no_rain = (rain == 0) * 1, rain_level = rain)
  data <- within(data, rain_level[rain == 0] <- NA)
  data <- transform(data, weeks2 = 1 + weeks %% (365 / 7)) # for seasonal trend

  # Modelling probability of no rain.
  gam0 <- gam(no_rain ~ s(weeks2, bs = "cc"), binomial("logit"), data)
  data$pzero <- predict(gam0, data, type = "response")

  if (package == "gamlss") {
    require(gamlss)
    form_mu <- rain_level ~ pbc(weeks2)
    form_sg <- ~ pbc(weeks2)
    data0 <<- na.omit(data)
    lss <- gamlss(form_mu, sigma.formula = form_sg, data = data0, family = GA,
                  trace = FALSE)
    mu <- predict(lss, what = "mu", newdata = data, type = "response")
    sigma <- predict(lss, what = "sigma", newdata = data, type = "response")
    data$mu <- mu
    data$sigma <- 1 / sigma ^ 2
    # Notes:
    # 1) predict only works if data0 exists in the global env.
    # 2) stepGAIC only works if form_mu and form_sg exits in the global env.
    # Solution: use <<- instead of <- for global assignment.
  } else {
    require(bamlss)
    # Modelling meanrain to select harmonic terms.
    lm0 <- lm(paste0("rain_level ~ 1", terms), data)
    lm1 <- step(lm0, trace = 0)
    # lm1 <- step(lm0, trace = 0, k = log(nrow(data)))

    # Distributional model for rain_level.
    # form_lss <- list(update(formula(lm1), rain_level ~ .),
    #                  update(formula(lm1), sigma ~ .))
    form0 <- as.character(formula(lm1))
    form0 <- as.formula(paste(form0[2], form0[1], form0[3]))
    # form0 <- formula(lm1)
    form_lss <- list(update(form0, rain_level ~ .),
                     update(form0, sigma ~ .))

    # form_lss <- list(rain_level ~ 1,
    #                  sigma ~ 1)
    lss <- bamlss(form_lss, family = "gamma", data = data)
    data[c("mu", "sigma")] <- predict(lss, data, type = "parameter")
  }

  # Compute the spi and additional variables.
  data <- transform(data, cdf_gamma = pgamma(rain, sigma, sigma / mu))
  data <- transform(data, cdf_mix = pzero + (1-pzero) * cdf_gamma)
  data <- transform(data, spi = qnorm(data$cdf_mix),
                    q025 = qgamma((0.025 - pzero) / (1-pzero), sigma,
                                  sigma / mu),
                    q975 = qgamma((0.975 - pzero) / (1-pzero), sigma,
                                  sigma / mu))
  data <- within(data, {q025[is.na(q025)] <- 0;
                        q975[is.na(q975)] <- 0})



  if (plot) {
    opar <- par(mfrow = c(1, 2))
    plot(data$weeks, data$spi)
    points(data$weeks[abs(data$spi) > qnorm(0.975)],
           data$spi[abs(data$spi) > qnorm(0.975)], col = 2, lwd = 2)
    plot(data$weeks, data$rain)
    points(data$weeks[abs(data$spi) > qnorm(0.975)],
           data$rain[abs(data$spi) > qnorm(0.975)], col = 2, lwd = 2)
    lines(data$weeks, data$mu)
    lines(data$weeks, data$q025)
    lines(data$weeks, data$q975)
    par(opar)
  }

return(subset(data, select = c(spi, mu, sigma, pzero)))
}
