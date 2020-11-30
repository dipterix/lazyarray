#include <Rcpp.h>
#include "utils.h"
using namespace Rcpp;



/*** R
a = 100000
b = 1024
bench::mark(
  mod2(a, b),
  mod3(a, b),
  mod4(a, b)
)

*/
