
require(testthat)

context("cpp - IO base")


dim <- c(10,20,50); 

f <- normalizePath(tempfile(), mustWork = FALSE)

test_that("IO - double, NumericVector", {
  
  x <- rnorm(10000); dim(x) <- dim
  unlink(f)
  test_data <- x; dim(test_data) <- c(200, 50); test_data <- as.data.frame(test_data)
  .Call(fstcore:::`_fstcore_fststore`, f, test_data, 100L, TRUE)
  expect_true(file.exists(f), label = "fstcore can write to file")
  
  unlink(f)
  # write to a file
  file.create(f)
  f <- normalizePath(f, mustWork = FALSE)
  unlink(f)
  lazyarray:::cpp_create_lazyarray(x, dim, f, 100L, TRUE);
  
  expect_true(file.exists(f), label = "cpp_create_lazyarray can write to file")
  
  
  # Make sure we have invalid indices
  idx_loc <- list(
    as.integer(sample(12) - 1),
    as.integer(sample(22)-1),
    as.integer(sample(52)-1)
  )
  target_dim = sapply(idx_loc, length)
  y1 <- lazyarray:::lazyLoadOld(f, idx_loc, dim, 0.1)
  
  a = idx_loc[[1]]; a[(a<1) | (a >10)] = NA
  b = idx_loc[[2]]; b[b<1 | b > 20] = NA
  c = idx_loc[[3]]; c[c<1 | c > 50] = NA
  y2 <- x[a,b,c]
  
  expect_equal(sum(abs(is.na(y2) - is.na(y1))), 0, label = 'lazyarray subset (double) vs base -> index')
  expect_equal(range(y2 - y1, na.rm = TRUE), c(0,0), label = 'lazyarray subset (double) vs base -> value')
  
  # trying other loader
  y3 <- lazyarray:::lazyLoadOld(f, idx_loc, dim, 0L)
  expect_equal(range(as.integer(y2) - y3, na.rm = TRUE), c(0,0), label = 'lazyarray stored: double -> loader: int')
  
  y3 <- lazyarray:::lazyLoadOld(f, idx_loc, dim, '')
  expect_equal(local({y2 <- as.character(y2); dim(y2) = dim(y3); y2}), y3, label = 'lazyarray stored: double -> loader: char')
  
})


test_that("IO - double, IntegerVector", {
  x <- as.integer(sample(10000)); dim(x) <- dim
  unlink(f)
  lazyarray:::cpp_create_lazyarray(x, dim, f, 100L, TRUE);
  
  expect_true(file.exists(f), label = "cpp_create_lazyarray can write to file")
  
  idx_loc <- list(
    as.integer(sample(12) - 1),
    as.integer(sample(22)-1),
    as.integer(sample(52)-1)
  )
  y1 <- lazyarray:::lazyLoadOld(f, idx_loc, dim, 9L)
  
  a = idx_loc[[1]]; a[(a<1) | (a >10)] = NA
  b = idx_loc[[2]]; b[b<1 | b > 20] = NA
  c = idx_loc[[3]]; c[c<1 | c > 50] = NA
  y2 <- x[a,b,c]
  
  expect_equal(sum(abs(is.na(y2) - is.na(y1))), 0, label = 'lazyarray subset (int) vs base -> index')
  expect_equal(range(y2 - y1, na.rm = TRUE), c(0,0), label = 'lazyarray subset (int) vs base -> value')
  expect_true(is.integer(y1), label = "check if it's integer")
  
  # trying other loaders
  y3 <- lazyarray:::lazyLoadOld(f, idx_loc, dim, 0.0)
  expect_equal(range(y2 - y3, na.rm = TRUE), c(0,0), label = 'lazyarray stored: int -> loader: double')
  
  y3 <- lazyarray:::lazyLoadOld(f, idx_loc, dim, '')
  expect_equal(local({y2 <- as.character(y2); dim(y2) = dim(y3); y2}), y3, label = 'lazyarray stored: int -> loader: char')
  
})



test_that("IO - double, CharacterVector", {
  
  x <- paste(sample(LETTERS, 10000, replace = TRUE), " asdasd"); dim(x) <- dim
  unlink(f)
  lazyarray:::cpp_create_lazyarray(x, dim, f, 100L, TRUE);
  
  expect_true(file.exists(f), label = "cpp_create_lazyarray can write to file")
  
  idx_loc <- list(
    as.integer(sample(12) - 1),
    as.integer(sample(22)-1),
    as.integer(sample(52)-1)
  )
  y1 <- lazyarray:::lazyLoadOld(f, idx_loc, dim, '')
  
  a = idx_loc[[1]]; a[(a<1) | (a >10)] = NA
  b = idx_loc[[2]]; b[b<1 | b > 20] = NA
  c = idx_loc[[3]]; c[c<1 | c > 50] = NA
  y2 <- x[a,b,c]
  
  expect_equal(sum(abs(is.na(y2) - is.na(y1))), 0, label = 'lazyarray subset (char) vs base -> index')
  expect_equal(y1, y2, label = 'lazyarray subset (char) vs base -> value')
  expect_true(is.character(y1), label = "check if it's integer")
  
  # trying other loaders
  expect_error(lazyarray:::lazyLoadOld(f, idx_loc, dim, 0.0))
  
  expect_error(lazyarray:::lazyLoadOld(f, idx_loc, dim, 0L))
  
  x <- paste0(sample(0:9, 10000, replace = TRUE), ""); dim(x) <- dim
  unlink(f)
  lazyarray:::cpp_create_lazyarray(x, dim, f, 100L, TRUE);
  
  expect_error(lazyarray:::lazyLoadOld(f, idx_loc, dim, 0.0))
  
  expect_error(lazyarray:::lazyLoadOld(f, idx_loc, dim, 0L))
  
})
