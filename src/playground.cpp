#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::string timesTwo(int64_t input) {
  Rcout << input << "\n";
  return std::to_string(input);
}

/*** R

timesTwo(2^54)
*/
