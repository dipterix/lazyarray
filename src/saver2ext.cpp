#include "saver2ext.h"

#include "common.h"
#include "utils.h"
#include "fstWrapper.h"
#include "indexConvert.h"
using namespace Rcpp;

// returns: -1 all partitions affected, otherwise the indices of partitions


/*** R
x <- lazyarray:::as.lazyarray(array(rep(1,27), rep(3,3)))
unlink(x$get_partition_fpath(3))
res <- lazyarray:::parseAndScheduleBlocks(list(lazyarray:::get_missing_value(),1,1:3), dim(x))
writeFstPartition_double(1.1, x$get_partition_fpath(), dim(x), res, 50L, TRUE)
x[]
x[,1,]
*/
