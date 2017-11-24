
#include <Rcpp.h>
using namespace Rcpp;

//' @title Moving average.
//'
//' @description
//' \code{runmean} computes the moving average of a time series.
//'
//' @details
//' details.
//'
//' @param a Vector of values representing the original time series.
//' @param width The number of values to consider for the average.
//'
//' @return A moving average of the time series.
//'
//' @author Erick A. Chacon-Montalvan
//'
//' @examples
//'
//' x <- 1:10
//' runmean(x, width = 1)
//' runmean(x, width = 2)
//' runmean(x, width = 3)
//'
//' @importFrom Rcpp evalCpp
//'
//' @useDynLib day2day
//'
//' @export
// [[Rcpp::export]]
NumericVector runmean(NumericVector a, int width) {
  int n = a.size();
  NumericVector out(n); // by default is initialized with 0

  for (int i = 0; i < width - 1; ++i)
    out[i] = NA_REAL;

  for (int i = width - 1; i < n; i++)
    for (int j = 0; j < width; j++)
      out[i] += a[i-j] / width;
  return out;
}
