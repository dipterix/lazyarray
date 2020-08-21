# Installation (dev version)
remotes::install_github('dipterix/dipsaus')
remotes::install_github('dipterix/lazyarray')

library(bigmemory)
library(biganalytics)
library(dipsaus)
library(lazyarray)

path <- tempfile()
f <- 'bigmemory.testfile'
dir.create(path)
x <- big.matrix(99072112, ncol = 5, type = 'double', backingfile = f, backingpath = path)
options(bigmemory.allow.dimnames=TRUE)
colnames(x) <- c("movie", "customer", "rating", "year", "month")
for(ii in 1:5){
  x[,ii] <- rnorm(99072112)
}

pryr::object_size(x)

# The challenge is calculation of X^T*X and X^T*y
{
  a = Sys.time()
  res <- biglm.big.matrix(rating ~ movie + customer + year + month - 1, data = x)
  b <- Sys.time(); b - a
}
# Time difference of 26.069 secs


path <- tempfile()
arr <- lazyarray::lazyarray(path, dim = c(99072112, 5), storage_format = 'double')
for(ii in 1:5){
  arr[,ii] <- x[,ii]
}

{
  a = Sys.time()
  amat <- as.lazymatrix(arr)
  amatt <- amat$transpose()
  re <- amatt$multiply_mat(amat, plan = TRUE)
  coef <- solve(re[c(1,2,4,5),c(1,2,4,5)]) %*% re[c(1,2,4,5), 3]
  b <- Sys.time(); b - a
}

# sequential 
# Time difference of 21.62714 secs

# multisession
# Time difference of 12.19716 secs

# multicore (plan = TRUE)
# Time difference of 7.512503 secs


# Make sure the coef are correct
# 1e-17 range
coefficients(res) - coef
