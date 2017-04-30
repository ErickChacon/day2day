
#' @title Compute the Standardized Precipitation Index.
#'
#' @description
#' \code{spi_week} computes the weekly spi index with gamlss models.
#' It can also with work with precipitation series containing NA, 0 or
#' only non-zero values.
#'
#' @details
#' details.
#'
#' @param rain Precipitation level.
#' @param tscale Time-scale to compute the SPI in week units.
#' @param period Period (e.g. 53 weeks) defined to model the seasonal effect.
#' @param package Package to use. \code{gamlss} for backfitting and \code{bamlss} for baysian gamlss models.
#'
#' @return dataframe consisting of spi, mu, sigma and pzero.
#'
#' @author Erick A. Chacon-Montalvan
#'
#' @export
spi_week <- function(rain, tscale = 1, period = 365 / 7, package = "gamlss", plot = FALSE) {

  require(mgcv)

  # Compute moving average of rain based on time-scale.
  rain <- runmean(rain, tscale)

  # Create dataframe.
  data <- data.frame(rain, weeks = 1:length(rain))
  data <- transform(data, no_rain = (rain == 0) * 1, rain_level = rain)
  data <- within(data, rain_level[rain == 0] <- NA)
  data <- transform(data, weeks2 = weeks %% period) # for seasonal trend

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


#' @title Identify floods and droughts based on SPI.
#'
#' @description
#' \code{find_flood_drought} description.
#'
#' @details
#' details.
#'
#' @param par.
#'
#' @return return.
#'
#' @author Erick A. Chacon-Montalvan
#'
#' @export
find_flood_drought <- function (spi) {
  # Convert spi to string to detect extreme events.
  spi <- (spi > -1) + (spi > 0) + (spi > 1)
  spi[is.na(spi)] <- 9
  spichar <- spi %>% paste(collapse = "")
  # Detect flood and drought.
  flood_exp <- gregexpr("[2-3]*3+[2-3]*", spichar)
  drought_exp <- gregexpr("[0-1]*0+[0-1]*", spichar)
  flood <- function(n) strrep("8", n)
  drought <- function(n) strrep("7", n)
  regmatches(spichar, flood_exp) <-
    Map(flood, lapply(regmatches(spichar, flood_exp), nchar))
  regmatches(spichar, drought_exp) <-
    Map(drought, lapply(regmatches(spichar, drought_exp), nchar))
  # Convert to vector.
  spichar <- substring(spichar, 1:nchar(spichar), 1:(nchar(spichar)))
  spi <- as.numeric(spichar)
  spi[spi == 9] <- NA
  spi[!(spi %in% c(7:8, NA))] <- 0
  spi <- factor(spi, c(0, 7, 8), c("normal", "drought", "flood"))
  return(spi)
}
