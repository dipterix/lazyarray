test_that("multiplication works", {
  on.exit({options('lazyarray.chunk_memory' = 80)
    lazyarray::set_lazy_threads(0)})
  
  lazyarray::set_lazy_threads(1)
  
  
  options('lazyarray.chunk_memory' = 0.00001)
  
  orig <- rnorm(1e4)
  
  orig1 <- orig; dim(orig1) <- c(10, 1000)
  
  weights <- rnorm(1000)
  
  # 0.001 
  system.time({
    xy0 <- orig1 %*% (t(orig1) * weights)
  })
  
  # x <- as.lazymatrix(orig1)
  # y <- as.lazymatrix(t(orig1))
  
  
  # # 0.130
  # system.time({
  #   xy <- lazy_matmul(x, y, weights =  weights, hybrid_limit = 0)
  # })
  # expect_equal(range(xy[] - xy0), c(0,0))
  # 
  # y <- t(as.lazymatrix(orig1))
  # 
  # # 0.188 
  # a <- system.time({
  #   xy <- lazy_matmul(x, y, weights =  weights, hybrid_limit = 0)
  # })
  # expect_equal(range(xy[] - xy0), c(0,0))
  # 
  # x <- t(as.lazymatrix(t(orig1)))
  # 
  # # 0.098 
  # system.time({
  #   xy <- lazy_matmul(x, y, weights =  weights, hybrid_limit = 0)
  # })
  # expect_equal(range(xy[] - xy0), c(0,0))
  
  y <- as.lazymatrix(t(orig1))
  
  # # 0.017 
  # system.time({
  #   xy <- lazy_matmul(x, y, weights =  weights, hybrid_limit = 0)
  # })
  # expect_equal(range(xy[] - xy0), c(0,0))
  
  x <- as.lazymatrix(t(orig1))
  
  # 0.002
  system.time({
    xy <- crossprod(x, weights = weights)
  })
  expect_equal(range(xy[] - xy0), c(0,0))
  
  # x <- t(as.lazymatrix(orig1))
  # 
  # # 0.162 
  # system.time({
  #   xy <- lazy_crossprod(x, weights = weights)
  # })
  # expect_equal(range(xy[] - xy0), c(0,0))
  # 
  # # bench::mark({
  # #   lazy_matmul(x, y)
  # # }, {
  # #   x[] %*% y[]
  # # }, check = F, iterations = 1)
  # # 
})



# test_that("multiplication works", {
#   skip_on_cran()
#   skip_on_travis()
#   skip_on_appveyor()
#   skip_on_bioc()
#   skip_on_ci()
#   skip_on_covr()
#   skip_if_not(dir.exists('~/Desktop/lazyarray_data/'))
# 
#   self <- lazyarray('~/Desktop/lazyarray_data/')
#   self <- as.lazymatrix(self)
# 
#   future::plan('multisession')
#   
#   # pryr::mem_used()
#   system.time({
#     a <- lazy_crossprod(self)
#   })
# 
# })
