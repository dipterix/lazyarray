# Installation (dev version)
remotes::install_github('dipterix/dipsaus')
remotes::install_github('dipterix/lazyarray')

library(bigmemory)
library(biganalytics)
library(dipsaus)
library(lazyarray)
library(future)


path <- '~/Desktop/junk/lazyarray_test2'
arr <- lazyarray::lazyarray(path, dim = c(99072112, 5), storage_format = 'double')
arr <- as.lazymatrix(arr)
colnames(arr) <- c('movie', 'customer', 'rating', 'year', 'month')
# for(ii in 1:5){
#   arr[,ii] <- x[,ii]
# }
system.time({
  arr[,1]
})


path <- '~/Desktop/junk/'
f <- 'bigmemory.testfile'
# unlink(file.path(path, f))
dir.create(path)
ncol = 5
x <- bigmemory::attach.resource(file.path(path, 'bigmemory.testfile.desc'))
# x <- big.matrix(99072112, ncol = ncol, type = 'double', backingfile = f, backingpath = path)
# for(ii in 1:ncol){
#   x[,ii] <- arr[,ii]
# }

# The challenge is calculation of X^T*X and X^T*y
{
  options(bigmemory.allow.dimnames=TRUE)
  colnames(x) <- c('movie', 'customer', 'rating', 'year', 'month')
  a <- proc.time()
  res <- biglm.big.matrix(rating ~ movie + customer + year + month - 1, data = x)
  b <- proc.time(); b - a
}
# Time difference of 26.069 secs


future::plan('multisession')
{
  a <- proc.time()
  ll <- lazy_lm_simple(arr, yidx = 3, intercept = FALSE)
  b <- proc.time(); b - a
}

coefficients(ll) / coefficients(res)

future::plan('sequential')
{
  a <- proc.time()
  lazyres <- lazy_lm(rating ~ movie + customer + year + month - 1, data = arr)
  b <- proc.time(); b - a
}
coefficients(lazyres) / coefficients(res)
