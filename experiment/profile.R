devtools::load_all()
library(lazyarray)

x <- as.lazyarray(array(1:27,c(3,3,3)), storage_format = 'double')
lazyarray:::setLazyBlockSize(2)
x[,,1]
loc2idx2(list(c(NA,1,0), c(2,NA,1), 1), dim(x))

lazyarray:::setLazyBlockSize(-1)
path <- "~/Desktop/lazyarray_data/"
path <- "~/Desktop/junk/lazyarray_test2/"
dimension <- c(287, 200, 601, 84)
x <- lazyarray(path, storage_format = "double", dim = dimension)
x
a=x[,,,1:10]

subf <- function(i, ...){
  .Call(lazyarray:::`_lazyarray_subsetFST`, x$get_partition_fpath(), environment(), dim(x),
        getSexpType(x$`@sample_data`()), reshape=NULL, drop=FALSE)
}
subf <- function(i, ...){
  lazyarray:::subsetFST(x$get_partition_fpath(), environment(), dim(x),
                         getSexpType(x$`@sample_data`()), reshape=NULL, drop=FALSE)
}

(subf(,,,1))
lazyarray:::setLazyBlockSize(-1)
system.time(subf(,,,1:10))
lazyarray:::setLazyBlockSize(31250000)
system.time(subf(,,,1:10))
system.time(x$`@get_data`(1:287, 1:200, 1, 1:10))

lazyarray::set_lazy_threads(4, reset_after_fork = T)
