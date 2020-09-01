# Installation (dev version)
remotes::install_github('dipterix/dipsaus')
remotes::install_github('dipterix/lazyarray')

library(bigmemory)
library(biganalytics)
library(dipsaus)
library(lazyarray)
library(future)

path <- '~/Desktop/junk/'
f <- 'bigmemory.testfile'
# unlink(file.path(path, f))
dir.create(path)
ncol = 5
x <- big.matrix(99072112, ncol = ncol, type = 'double', backingfile = f, backingpath = path)
for(ii in 1:ncol){
  x[,ii] <- rnorm(99072112)
}

system.time({
  x[100:20000000,]
})
system.time({
  dipsaus::lapply_async2(1:ncol, function(i){
    max(x[,i])
  }, plan = FALSE)
})

profvis::profvis(max(x))

# The challenge is calculation of X^T*X and X^T*y
{
  options(bigmemory.allow.dimnames=TRUE)
  colnames(x) <- c('movie', 'customer', 'rating', 'year', 'month')
  a <- proc.time()
  res <- biglm.big.matrix(rating ~ movie + customer + year + month - 1, data = x)
  b <- proc.time(); b - a
}
# Time difference of 26.069 secs


path <- '~/Desktop/junk/lazyarray_test2'
arr <- lazyarray::lazyarray(path, dim = c(99072112, 5), storage_format = 'double')
# for(ii in 1:5){
#   arr[,ii] <- x[,ii]
# }
system.time({
  arr[1:20000000,1]
})

{
  fstcore::threads_fstlib(1L)  # change this to > 1 to enable OpenMP
  future::plan('sequential')  # change this to multisession or multicore to enable future parallels
  a = Sys.time()
  amat <- as.lazymatrix(arr)
  amatt <- amat$transpose()
  re <- amatt$multiply_mat(amat, plan = FALSE)
  coef <- solve(re[c(1,2,4,5),c(1,2,4,5)]) %*% re[c(1,2,4,5), 3]
  b <- Sys.time(); b - a
}

# sequential, openmp thread = 1
# Time difference of 16.67987 secs

# sequential, openmp thread = 4
# Time difference of 16.65988 secs

# multisession
# Time difference of 8.292826 secs

# multicore
# Time difference of 7.512503 secs


# Make sure the coef are correct
# 1e-17 range
coefficients(res) - coef









test_file <- '~/Desktop/junk/lazyarray_test'

arr <- lazyarray::lazyarray(test_file, dim = c(100,100,100,1000), storage_format = 'double')
dim(arr)

# dipsaus::lapply_async2(1:1000, function(i){
#   arr[,,,i] <- rnorm(1e6)
#   NULL
# }, plan = 'multisession')

# options('lazyarray.chunk_memory' = 80)


future::plan('sequential')
future::plan('multisession')

system.time({
  lazyarray::set_lazy_threads(4)
  max(arr)
})

system.time({
  lazyarray::set_lazy_threads(1)
  future::plan('multisession', workers = 2)
  tmp <- arr$`@partition_map`(partitions = 1:1000, method = 'others', map_function = function(x){
    dim(x) <- c(100,100,100)
    dipsaus::collapse(x, c(3,2), average = TRUE)
  }, split = FALSE, na_rm = FALSE, missing = NA)
})

system.time({
  lazyarray:::lapply2(1:1000, function(i){
    max(arr[,,,i])
  })
})

future::plan('sequential')
profvis::profvis(max(arr, max_nchunks = 10))

system.time({
  max(arr, max_nchunks = 10)
})
system.time({
  range(fst::read_fst(arr$get_partition_fpath(1), as.data.table = T))
})


lazyarray:::make_chunks(max_nchunks = )

