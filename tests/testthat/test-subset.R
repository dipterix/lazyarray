test_that("Subset lazyarray & lazymatrix", {
  a <- array(rnorm(80), c(2,4,2,5))
  x <- as.lazyarray(a)
  
  expect_equal(x[], a)
  
  idx <- sample(length(x), length(x) * 10, replace = TRUE)
  expect_equal(x[idx], a[idx])
  
  # matrix
  x <- as.lazymatrix(a)
  b <- x[]
  
  expect_equivalent(as.integer(dim(b)), c(16,5))
  
  expect_equal(x[], b)
  
  idx <- sample(length(x), length(x) * 10, replace = TRUE)
  expect_equal(x[idx], b[idx])
  
  expect_equivalent(x[,1], b[,1])
  expect_equivalent(x[,1,drop=FALSE], b[,1,drop=FALSE])

  x <- t(x)  
  b <- t(b)
  
  expect_equal(dim(x), c(5,16))
  
  expect_equal(x[], b)
  
  idx <- sample(length(x), length(x) * 10, replace = TRUE)
  expect_equal(x[idx], b[idx])
  
  expect_equal(x[,1], b[,1])
  expect_equal(x[,1,drop=FALSE], b[,1,drop=FALSE])
  
})
