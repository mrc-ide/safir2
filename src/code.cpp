#include <Rcpp.h>

//' @title test c++
//' @param x an integer
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector testcpp(const int x) {
  Rcpp::NumericVector out(x);
  for (auto i = 0u; i < x; ++i) {
    out[i] = 1.;
  }
  return out;
}
