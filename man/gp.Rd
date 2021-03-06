% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulation.R
\name{gp}
\alias{gp}
\title{Simulate a Gaussian process}
\usage{
gp(s1, s2, cov.model = NULL, cov.params = NULL)
}
\arguments{
\item{s1}{First coordinate}

\item{s2}{Second coordinate}

\item{cov.model}{A character or function indicating the covariance function that
Should be used to compute the variance-covariance matrix}

\item{cov.params}{A list of the parameters required by the \code{cov.model} function.}
}
\value{
A vector of the realization of the Gaussian Process
}
\description{
\code{gp} Simulate a spatial Gaussian process given a certain covariance function.
}
\details{
details.
}
\examples{
# Generate coordinates
N <- 1000
s1 <- 2 * runif(N)
s2 <- 2 * runif(N)
# Simulate and plot the realization of a Gaussian process
y <- gp(s1, s2, "exp_cov", list(phi = 0.05, sigma2 = 1))
plot(s1, s2, cex = y)
# Plot with ggplot
# ggplot(data.frame(s1, s2, y), aes(s1, s2, col = y)) +
#  geom_point(size = 3)

}
\author{
Erick A. Chacon-Montalvan
}
