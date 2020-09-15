
require(testthat)

context("Loader2 no sub-blocks")

dim <- c(10,20,50) 

f <- normalizePath(tempfile(), mustWork = FALSE)
on.exit({
  setLazyBlockSize(0)
  unlink(f)
})

x = array(1:prod(dim), dim)
a = as.lazyarray(x, path = f)
loader_f <- function(..., samp, reshape, drop){
  lazyarray:::subsetFST(a$storage_path, environment(), dim, getSexpType(samp), reshape, drop)
}

lazy_test_unit <- function(samp_data, x_alt){
  loader_double = function(..., reshape = NULL, drop = FALSE){ loader_f(samp = samp_data, ..., reshape = reshape, drop = drop) }
  if(missing(x_alt)){
    x <- x; storage.mode(x) <- storage.mode(samp_data)
  } else {
    x <- x_alt; storage.mode(x) <- storage.mode(samp_data)
  }
  
  # 1. a()
  re <- loader_double()
  expect_equal(storage.mode(re), storage.mode(samp_data))
  expect_equivalent(re, x)
  expect_equivalent(dim(re), dim(x))
  
  # 2. a(i)
  idx <- sample(length(x), size = 200, replace = TRUE)
  idx[sample(200, 20)] = NA
  re <- loader_double(idx)
  cp <- x[idx]
  expect_equivalent(re, cp)
  
  # 2. a(-i)
  idx <- -sample(length(x), size = 200, replace = TRUE)
  idx[sample(200, 20)] = NA
  re <- loader_double(idx)
  cp <- x[idx[!is.na(idx)]]
  expect_equivalent(re, cp)
  
  # 2.1 large indices
  re <- loader_double(10000000)
  cp <- x[10000000]
  expect_equivalent(re, cp)
  expect_true(is.na(re))
  re <- loader_double(-10000000)
  cp <- x[-10000000]
  expect_equal(re, cp)
  expect_equal(dim(re), NULL)
  
  
  
  # 3. a(-i:i)  
  expect_error(loader_double(-1:1))
  
  # 4. a(i,j,k)
  idx <- lapply(dim, sample, replace = TRUE, size = 6)
  re <- do.call(loader_double, idx)
  cp <- eval(as.call(c(list(quote(`[`), quote(x)), idx)))
  expect_equivalent(re, cp)
  expect_equivalent(dim(re), dim(cp))
  
  # 5. negative subscripts
  ii <- sample(2)[[1]] + 1
  idx[[ii]] <- -idx[[ii]]
  re <- do.call(loader_double, idx)
  cp <- eval(as.call(c(list(quote(`[`), quote(x)), idx)))
  expect_equivalent(re, cp)
  expect_equivalent(dim(re), dim(cp))
  ii <- 1
  idx[[ii]] <- -idx[[ii]]
  re <- do.call(loader_double, idx)
  cp <- eval(as.call(c(list(quote(`[`), quote(x)), idx)))
  expect_equivalent(re, cp)
  expect_equivalent(dim(re), dim(cp))
  
  # 6. With missing
  re <- loader_double(,c(NA,1:0),c(2,NA,1))
  cp <- x[,c(NA,1:0),c(2,NA,1),drop = FALSE]
  expect_equivalent(re, cp)
  expect_equivalent(dim(re), dim(cp))
  
  re <- loader_double(c(NA,1:0),c(2,NA,1),)
  cp <- x[c(NA,1:0),c(2,NA,1),,drop = FALSE]
  expect_equivalent(re, cp)
  expect_equivalent(dim(re), dim(cp))
  
  re <- loader_double(c(NA,1:0),,c(2,NA,1))
  cp <- x[c(NA,1:0),,c(2,NA,1),drop = FALSE]
  expect_equivalent(re, cp)
  expect_equivalent(dim(re), dim(cp))
  
  re <- loader_double(,,)
  cp <- x[,,,drop = FALSE]
  expect_equivalent(re, cp)
  expect_equivalent(dim(re), dim(cp))
  
  # 7. drop
  re <- loader_double(,c(NA,1:0),c(2,NA,1), drop = TRUE)
  cp <- x[,c(NA,1:0),c(2,NA,1),drop = TRUE]
  expect_equivalent(re, cp)
  expect_equivalent(dim(re), dim(cp))
  re <- loader_double(,c(NA,1:0),c(3), drop = TRUE)
  cp <- x[,c(NA,1:0),c(3),drop = TRUE]
  expect_equivalent(re, cp)
  expect_equivalent(dim(re), dim(cp))
  
  # 8. reshape
  re <- loader_double(,c(NA,1:0),c(2,NA,1), reshape = c(20, 3))
  cp <- x[,c(NA,1:0),c(2,NA,1),drop = TRUE]
  expect_equivalent(as.vector(re), as.vector(cp))
  expect_equivalent(dim(re), c(20, 3))
  re <- loader_double(,c(NA,1:0),c(2,NA,1), reshape = c(60))
  expect_equivalent(dim(re), NULL)
  
  # 9. Negative with missing
  re <- loader_double(,-c(NA,1:0),c(2,NA,1))
  cp <- x[,-c(1:0),c(2,NA,1),drop = FALSE]
  expect_equivalent(re, cp)
  expect_equivalent(dim(re), dim(cp))
  
  re <- loader_double(-c(NA,1:0),c(2,NA,1),)
  cp <- x[-c(1:0),c(2,NA,1),,drop = FALSE]
  expect_equivalent(re, cp)
  expect_equivalent(dim(re), dim(cp))
  
  re <- loader_double(c(NA,1:0),,-c(2,NA,1))
  cp <- x[c(NA,1:0),,-c(2,1),drop = FALSE]
  expect_equivalent(re, cp)
  expect_equivalent(dim(re), dim(cp))
  
  re <- loader_double(,,-1000)
  cp <- x[,,-1000,drop = FALSE]
  expect_equivalent(re, cp)
  expect_equivalent(dim(re), dim(cp))
  
  re <- loader_double(,-1000,1)
  cp <- x[,-1000,1,drop = FALSE]
  expect_equivalent(re, cp)
  expect_equivalent(dim(re), dim(cp))
  
  re <- loader_double(-1000,,)
  cp <- x[-1000,,,drop = FALSE]
  expect_equivalent(re, cp)
  expect_equivalent(dim(re), dim(cp))
  
  
  # Wrong usages
  expect_error(loader_double(100,,))
  expect_error(loader_double(1,1,1,1))
  expect_error(loader_double(1,1))
  expect_error(loader_double(,1,1,))
  expect_error(loader_double(,))
  expect_error(loader_double(,1000,))
  expect_error(loader_double(,,1000))
  expect_error(loader_double(1000,,1000))
  expect_error(loader_double(1000,1000,))
  expect_error(loader_double(,1000,1000))
  
}

test_that("Loader2 no sub-blocks", {
  lazy_test_unit(0.1)
  lazy_test_unit(1L)
  lazy_test_unit("")
})


context("Loader2 sub-blocks")

test_that("Loader2 sub-blocks", {
  setLazyBlockSize(1)
  lazy_test_unit(0.1)
  lazy_test_unit(1L)
  lazy_test_unit("")
  
  setLazyBlockSize(11)
  lazy_test_unit(0.1)
  lazy_test_unit(1L)
  lazy_test_unit("")
  
  setLazyBlockSize(201)
  lazy_test_unit(0.1)
  lazy_test_unit(1L)
  lazy_test_unit("")
  
  setLazyBlockSize(0)
})

context("Loader2 with NAs")

test_that("Loader2 with NAs", {
  unlink(a$get_partition_fpath(2))
  x[,,2] <- NA
  expect_equal(x[], a[])
  lazy_test_unit(0.1, x)
  lazy_test_unit(1L, x)
  lazy_test_unit("", x)
})


context("Loader2 with complex data")

x = array(rnorm(prod(dim)), dim) + 1i * array(rnorm(prod(dim)), dim)
unlink(f, recursive = TRUE)
a = as.lazyarray(x, path = f)

test_that("Loader2 complex", {
  setLazyBlockSize(0)
  lazy_test_unit(x[[1]])
  
  setLazyBlockSize(1)
  lazy_test_unit(x[[1]])
  
  setLazyBlockSize(11)
  lazy_test_unit(x[[1]])
  
  setLazyBlockSize(201)
  lazy_test_unit(x[[1]])
  
  setLazyBlockSize(0)
  unlink(a$get_partition_fpath(2))
  x[,,2] <- NA
  expect_equal(x[], a[])
  lazy_test_unit(x[[1]], x)
})

context("Loader2 with matrix")
test_that("Loader2 with matrix", {
  x <- matrix(1:16,4)
  a <- as.lazyarray(x)
  loader_f <- function(...){
    lazyarray:::subsetFST(a$storage_path, environment(), dim(x), getSexpType(1L), NULL, FALSE)
  }
  
  expect_equivalent(loader_f(), x)
  expect_equivalent(loader_f(,1), x[,1,drop=TRUE])
  expect_equivalent(loader_f(1,), x[1,,drop=TRUE])
  
})
