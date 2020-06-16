
require(testthat)

context("cpp - Single Unit IO base")


dim <- c(10,20,50) 

f <- normalizePath(tempfile(), mustWork = FALSE)
on.exit({
  unlink(f)
})

test_that("IO - NumericVector", {
  
  x <- rnorm(10000); dim(x) <- dim
  x[sample(10000, 2000)] <- NA
  unlink(f)
  test_data <- x; dim(test_data) <- c(200, 50); test_data <- as.data.frame(test_data)
  .Call(fstcore:::`_fstcore_fststore`, f, test_data, 100L, TRUE)
  expect_true(file.exists(f), label = "fstcore can write to file")
  
  unlink(f)
  # write to a file
  file.create(f)
  f <- normalizePath(f, mustWork = FALSE)
  unlink(f)
  lazyarray:::cpp_create_lazyarray(x, dim, f, 100L, TRUE)
  
  expect_true(file.exists(f), label = "cpp_create_lazyarray can write to file")
  
  
  # Make sure we have invalid indices
  idx_loc <- list(
    as.integer(sample(12) - 1),
    as.integer(sample(22)-1),
    as.integer(sample(52)-1)
  )
  target_dim <- sapply(idx_loc, length)
  y1 <- lazyarray:::cpp_load_lazyarray(f, idx_loc, dim, length(dim), 0.1)
  
  a <- idx_loc[[1]]; a[(a<1) | (a >10)] <-  NA
  b <- idx_loc[[2]]; b[b<1 | b > 20] <-  NA
  c <- idx_loc[[3]]; c[c<1 | c > 50] <-  NA
  y2 <- x[a,b,c]
  
  expect_equal(sum(abs(is.na(y2) - is.na(y1))), 0, label = 'lazyarray subset (double) vs base -> index')
  expect_equal(range(y2 - y1, na.rm = TRUE), c(0,0), label = 'lazyarray subset (double) vs base -> value')
  
  # trying other loader
  y3 <- lazyarray:::cpp_load_lazyarray(f, idx_loc, dim, length(dim), 0L)
  expect_equal(range(as.integer(y2) - y3, na.rm = TRUE), c(0,0), label = 'lazyarray stored: double -> loader: int')
  
  y3 <- lazyarray:::cpp_load_lazyarray(f, idx_loc, dim, length(dim), '')
  expect_equal(local({y2 <- as.character(y2); dim(y2) <- dim(y3); y2}), y3, label = 'lazyarray stored: double -> loader: char')
  
})


test_that("IO - IntegerVector", {
  x <- as.integer(sample(10000)); dim(x) <- dim
  x[sample(10000, 2000)] <- NA
  unlink(f)
  lazyarray:::cpp_create_lazyarray(x, dim, f, 100L, TRUE)
  
  expect_true(file.exists(f), label = "cpp_create_lazyarray can write to file")
  
  idx_loc <- list(
    as.integer(sample(12) - 1),
    as.integer(sample(22)-1),
    as.integer(sample(52)-1)
  )
  y1 <- lazyarray:::cpp_load_lazyarray(f, idx_loc, dim, length(dim), 9L)
  
  a <- idx_loc[[1]]; a[(a<1) | (a >10)] <-  NA
  b <- idx_loc[[2]]; b[b<1 | b > 20] <- NA
  c <- idx_loc[[3]]; c[c<1 | c > 50] <- NA
  y2 <- x[a,b,c]
  
  expect_equal(sum(abs(is.na(y2) - is.na(y1))), 0, label = 'lazyarray subset (int) vs base -> index')
  expect_equal(range(y2 - y1, na.rm = TRUE), c(0,0), label = 'lazyarray subset (int) vs base -> value')
  expect_true(is.integer(y1), label = "check if it's integer")
  
  # trying other loaders
  y3 <- lazyarray:::cpp_load_lazyarray(f, idx_loc, dim, length(dim), 0.0)
  expect_equal(range(y2 - y3, na.rm = TRUE), c(0,0), label = 'lazyarray stored: int -> loader: double')
  
  y3 <- lazyarray:::cpp_load_lazyarray(f, idx_loc, dim, length(dim), '')
  expect_equal(local({y2 <- as.character(y2); dim(y2) <- dim(y3); y2}), y3, label = 'lazyarray stored: int -> loader: char')
  
})


test_that("IO - LogicalVector", {
  x <- (sample(10000) > 5000); dim(x) <- dim
  x[sample(10000, 2000)] <- NA
  unlink(f)
  lazyarray:::cpp_create_lazyarray(x, dim, f, 100L, TRUE)
  
  expect_true(file.exists(f), label = "cpp_create_lazyarray can write to file")
  
  idx_loc <- list(
    as.integer(sample(12) - 1),
    as.integer(sample(22)-1),
    as.integer(sample(52)-1)
  )
  y1 <- lazyarray:::cpp_load_lazyarray(f, idx_loc, dim, length(dim), TRUE)
  
  a <- idx_loc[[1]]; a[(a<1) | (a >10)] <- NA
  b <- idx_loc[[2]]; b[b<1 | b > 20] <- NA
  c <- idx_loc[[3]]; c[c<1 | c > 50] <- NA
  y2 <- x[a,b,c]
  
  expect_equal(sum(abs(is.na(y2) - is.na(y1))), 0, label = 'lazyarray subset (logical) vs base -> index')
  expect_equal(range(y2 - y1, na.rm = TRUE), c(0,0), label = 'lazyarray subset (logical) vs base -> value')
  expect_true(is.integer(y1), label = "check if it's integer")
  
  # trying other loaders
  y3 <- lazyarray:::cpp_load_lazyarray(f, idx_loc, dim, length(dim), 0.0)
  expect_equal(range(y2 - y3, na.rm = TRUE), c(0,0), label = 'lazyarray stored: logical -> loader: double')
  
  y3 <- lazyarray:::cpp_load_lazyarray(f, idx_loc, dim, length(dim), '')
  expect_equal(local({y2 <- as.character(y2); dim(y2) <- dim(y3); y2}), y3, label = 'lazyarray stored: logical -> loader: char')
  
})



test_that("IO - CharacterVector", {
  
  x <- paste(sample(LETTERS, 10000, replace = TRUE), " asdasd"); dim(x) <- dim
  x[sample(10000, 2000)] <- NA
  unlink(f)
  lazyarray:::cpp_create_lazyarray(x, dim, f, 100L, TRUE)
  
  expect_true(file.exists(f), label = "cpp_create_lazyarray can write to file")
  
  idx_loc <- list(
    as.integer(sample(12) - 1),
    as.integer(sample(22)-1),
    as.integer(sample(52)-1)
  )
  y1 <- lazyarray:::cpp_load_lazyarray(f, idx_loc, dim, length(dim), '')
  
  a <- idx_loc[[1]]; a[(a<1) | (a >10)] <- NA
  b <- idx_loc[[2]]; b[b<1 | b > 20] <- NA
  c <- idx_loc[[3]]; c[c<1 | c > 50] <- NA
  y2 <- x[a,b,c]
  
  expect_equal(sum(abs(is.na(y2) - is.na(y1))), 0, label = 'lazyarray subset (char) vs base -> index')
  expect_equal(y1, y2, label = 'lazyarray subset (char) vs base -> value')
  expect_true(is.character(y1), label = "check if it's integer")
  
  # trying other loaders
  expect_error(lazyarray:::cpp_load_lazyarray(f, idx_loc, dim, length(dim), 0.0))
  
  expect_error(lazyarray:::cpp_load_lazyarray(f, idx_loc, dim, length(dim), 0L))
  
  x <- paste0(sample(0:9, 10000, replace = TRUE), ""); dim(x) <- dim
  unlink(f)
  lazyarray:::cpp_create_lazyarray(x, dim, f, 100L, TRUE)
  
  expect_error(lazyarray:::cpp_load_lazyarray(f, idx_loc, dim, length(dim), 0.0))
  
  expect_error(lazyarray:::cpp_load_lazyarray(f, idx_loc, dim, length(dim), 0L))
  
})


test_that("IO - ComplexVector", {
  x <- rnorm(10000) + 1i * rnorm(10000); dim(x) <- dim
  x[sample(10000, 2000)] <- NA
  unlink(f)
  lazyarray:::cpp_create_lazyarray(x, dim, f, 100L, TRUE)
  
  expect_true(file.exists(f), label = "cpp_create_lazyarray can write to file")
  
  expect_true(setequal(names(fst::fst(f)), paste0('V', 1:50, rep(c('R','I'), each = 50))))
  idx_loc <- list(
    as.integer(sample(12) - 1),
    as.integer(sample(22)-1),
    as.integer(sample(52)-1)
  )
  # .Call(fstcore:::`_fstcore_fstretrieve`, f, c('V1R', 'V1I'), 1L, 2L)
  y1 <- lazyarray:::cpp_load_lazyarray(f, idx_loc, dim, length(dim), 1i)

  a <- idx_loc[[1]]; a[(a<1) | (a >10)] <- NA
  b <- idx_loc[[2]]; b[b<1 | b > 20] <- NA
  c <- idx_loc[[3]]; c[c<1 | c > 50] <- NA
  y2 <- x[a,b,c]
  

  expect_equal(sum(abs(is.na(y2) - is.na(y1))), 0, label = 'lazyarray subset (logical) vs base -> index')
  expect_equal(y1, y2, label = 'lazyarray subset (logical) vs base -> value')
  # 
  # # trying other loaders
  # y3 <- lazyarray:::cpp_load_lazyarray(f, idx_loc, dim, length(dim), 0.0)
  # expect_equal(range(y2 - y3, na.rm = TRUE), c(0,0), label = 'lazyarray stored: logical -> loader: double')
  # 
  # y3 <- lazyarray:::cpp_load_lazyarray(f, idx_loc, dim, length(dim), '')
  # expect_equal(local({y2 <- as.character(y2); dim(y2) = dim(y3); y2}), y3, label = 'lazyarray stored: logical -> loader: char')
  
})


test_that("IO - Wrong Column Names", {
  # test with irregular column names
  x <- (sample(10000) > 5000); dim(x) <- c(200,50)
  x[sample(10000, 2000)] <- NA
  unlink(f)
  x <- as.data.frame(x)
  names(x) <- sprintf('Col')
  fst::write_fst(x, f)
  
  
  idx_loc <- list(
    as.integer(sample(12) - 1),
    as.integer(sample(22)-1),
    as.integer(sample(52)-1)
  )
  expect_error(lazyarray:::cpp_load_lazyarray(f, idx_loc, dim, length(dim), 0.0))
  
})


test_that("IO - Simgle dimension", {
  x <- as.integer(sample(100))
  unlink(f)
  
  # will be store as 100x1 dataframe
  lazyarray:::cpp_create_lazyarray(x, 100L, f, 100L, TRUE)
  
  expect_true(file.exists(f), label = "cpp_create_lazyarray can write to file")
  
  idx_loc <- list(
    as.integer(sample(12) - 1),
    as.integer(c(0, 1))
  )
  
  # Must use 100x1 dimension, even though the data is supposed to be 100 vector
  y1 <- lazyarray:::cpp_load_lazyarray(f, idx_loc, c(100L, 1L), 2L, 9L)
  
  expect_error(lazyarray:::cpp_load_lazyarray(f, list(1:10), 100L, 2L, 9L))
  
  lazyarray:::cpp_load_lazyarray(f, list(1:10, 1L), c(100L, 1L), 2L, 9L)
  
})










context("cpp - Multipart IO base")

test_that('2-D array', {
  fs <- replicate(10, normalizePath(tempfile(), mustWork = FALSE))
  on.exit({
    lapply(fs, unlink)
  })
  
  x <- matrix(rnorm(1000), c(100, 10))
  for(ii in 1:10){
    lazyarray:::cpp_create_lazyarray(x[,ii], 100L, fileName = fs[[ii]], compression = 100L, uniformEncoding = TRUE)
  }
  
  y1 <- lazyarray:::cpp_load_lazyarray(files = c('', rev(fs)), partition_locations = list(
    0:3,
    1L
  ), partition_dim = c(100L, 1L), ndim = 2L, 0.1)
  
  y2 <- cbind(NA, rbind(NA, x[1:3, 10:1]))
  
  expect_equal(is.na(y1), is.na(y2))
  expect_equal(range(y1-y2, na.rm = TRUE), c(0,0))
  
})


test_that('>=3-D array mode = 1', {
  fs <- replicate(10, normalizePath(tempfile(), mustWork = FALSE))
  on.exit({
    lapply(fs, unlink)
  })
  
  x <- array(rnorm(5000), c(25, 20, 10))
  for(ii in 1:10){
    lazyarray:::cpp_create_lazyarray(x[,,ii], c(25L, 20L, 1L), 
                                     fileName = fs[[ii]], compression = 100L, uniformEncoding = TRUE)
  }
  
  y1 <- lazyarray:::cpp_load_lazyarray(files = c('', rev(fs)), partition_locations = list(
    0:3,
    c(20L,100L),
    1L
  ), partition_dim = c(25L, 20L, 1L), ndim = 3L, 0.1)
  
  y2 <- x[c(NA,1:3), c(20, NA), c(NA, 10:1)]
  
  expect_equal(is.na(y1), is.na(y2))
  expect_equal(range(y1-y2, na.rm = TRUE), c(0,0))
  
})


test_that('>=3-D array mode = 2', {
  fs <- replicate(10, normalizePath(tempfile(), mustWork = FALSE))
  on.exit({
    lapply(fs, unlink)
  })
  
  x <- array(rnorm(5000), c(25, 20, 10))
  for(ii in 1:10){
    lazyarray:::cpp_create_lazyarray(x[,,ii], c(25L, 20L), 
                                     fileName = fs[[ii]], compression = 100L, uniformEncoding = TRUE)
  }
  
  y1 <- lazyarray:::cpp_load_lazyarray(files = c('', rev(fs)), partition_locations = list(
    0:3,
    c(20L,100L)
  ), partition_dim = c(25L, 20L), ndim = 3L, 0.1)
  
  y2 <- x[c(NA,1:3), c(20, NA), c(NA, 10:1)]
  
  expect_equal(is.na(y1), is.na(y2))
  expect_equal(range(y1-y2, na.rm = TRUE), c(0,0))
  
})












