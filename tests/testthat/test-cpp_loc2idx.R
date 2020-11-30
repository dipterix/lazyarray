test_that("Test loc2idx3 - validate", {
  # Case 1: small dset with NAs
  x = array(1:8,c(2,2,2))
  dim = dim(x)
  locs = list(0:1L,1L, 3:1)
  target_dim = sapply(locs, length)
  tmp = loc2idx3(locs, dim);# tmp
  tmp[tmp < -9e18] <- NA
  # validate in R
  mfactor <- c(1, cumprod(dim))[seq_along(dim)]
  scaled_loc <- lapply(seq_along(locs), function(ii){
    x <- as.integer(locs[[ii]])
    d <- dim[[ii]]
    x[x < 1 | x > d] <- NA 
    (x - 1) * mfactor[[ii]]
  })
  t1 <- Reduce(function(a, b){ outer(a, b, '+') }, scaled_loc) + 1
  expect_equal(sum(is.na(t1)) - sum(is.na(tmp)), 0)
  expect_equal(sum(xor(is.na(t1), is.na(tmp))), 0)
  expect_equal(range(t1 - tmp, na.rm = TRUE), c(0,0))
  
  # Case 2: small dset with 0 length
  x = array(1:8,c(2,2,2))
  dim = dim(x)
  locs = list(0:1L,integer(0), 3:1)
  target_dim = sapply(locs, length)
  tmp = loc2idx3(locs, dim);# tmp
  tmp[tmp < -9e18] <- NA
  # validate in R
  mfactor <- c(1, cumprod(dim))[seq_along(dim)]
  scaled_loc <- lapply(seq_along(locs), function(ii){
    x <- as.integer(locs[[ii]])
    d <- dim[[ii]]
    x[x < 1 | x > d] <- NA 
    (x - 1) * mfactor[[ii]]
  })
  t1 <- Reduce(function(a, b){ outer(a, b, '+') }, scaled_loc) + 1
  expect_equal(sum(is.na(t1)) - sum(is.na(tmp)), 0)
  expect_equal(sum(xor(is.na(t1), is.na(tmp))), 0)
  expect_length(t1, 0)
  expect_length(tmp, 0)
  
  # case 3: random set
  x = (1:5000000)
  # arr2df(x, c(0L, 25L, 50L), 9L)
  dim = c(50000L, 10L, 10L)
  locs = list(
    as.integer(sample(100000L, 300) - 2000),
    as.integer(sample(12)-1),
    as.integer(sample(12)-1)
  )
  target_dim = sapply(locs, length)
  tmp = loc2idx3(locs, dim);# tmp
  tmp[tmp < -9e18] <- NA
  dim(tmp) = target_dim
  
  # validate in R
  mfactor <- c(1, cumprod(dim))[seq_along(dim)]
  scaled_loc <- lapply(seq_along(locs), function(ii){
    x <- as.integer(locs[[ii]])
    d <- dim[[ii]]
    x[x < 1 | x > d] <- NA 
    (x - 1) * mfactor[[ii]]
  })
  t1 <- Reduce(function(a, b){ outer(a, b, '+') }, scaled_loc) + 1
  expect_equal(sum(is.na(t1)) - sum(is.na(tmp)), 0)
  expect_equal(sum(xor(is.na(t1), is.na(tmp))), 0)
  expect_equal(range(t1 - tmp, na.rm = TRUE), c(0,0))
  
  dim(x) = c(50000L, 10L, 10L)
  a = locs[[1]]; a[(a<1) | (a >50000)] = NA
  b = locs[[2]]; b[b<1 | b > 10] = NA
  c = locs[[3]]; c[c<1 | c > 10] = NA
  y2 <- x[a,b,c]
  expect_equal(range(y2 - tmp, na.rm = TRUE), c(0,0))
  
  y3 = tmp
  expect_equal(sum(abs(is.na(y2) - is.na(y3))), 0)
  expect_length(which(abs(is.na(y2) - is.na(y3)) > 0, arr.ind = TRUE), 0)
  expect_equal(range(y2 - y3, na.rm = TRUE), c(0,0))
  
})

test_that("Test loc2idx3 - validate", {
  # Case 1: small dset with NAs
  x = array(1:8,c(2,2,2))
  dim = dim(x)
  locs = list(0:1L,1L, 3:1)
  target_dim = sapply(locs, length)
  tmp = loc2idx3(locs, dim);# tmp
  tmp[tmp < -9e18] <- NA
  # validate in R
  mfactor <- c(1, cumprod(dim))[seq_along(dim)]
  scaled_loc <- lapply(seq_along(locs), function(ii){
    x <- as.integer(locs[[ii]])
    d <- dim[[ii]]
    x[x < 1 | x > d] <- NA 
    (x - 1) * mfactor[[ii]]
  })
  t1 <- Reduce(function(a, b){ outer(a, b, '+') }, scaled_loc) + 1
  expect_equal(sum(is.na(t1)) - sum(is.na(tmp)), 0)
  expect_equal(sum(xor(is.na(t1), is.na(tmp))), 0)
  expect_equal(range(t1 - tmp, na.rm = TRUE), c(0,0))
  
  # Case 2: small dset with 0 length
  x = array(1:8,c(2,2,2))
  dim = dim(x)
  locs = list(0:1L,integer(0), 3:1)
  target_dim = sapply(locs, length)
  tmp = loc2idx3(locs, dim);# tmp
  tmp[tmp < -9e18] <- NA
  # validate in R
  mfactor <- c(1, cumprod(dim))[seq_along(dim)]
  scaled_loc <- lapply(seq_along(locs), function(ii){
    x <- as.integer(locs[[ii]])
    d <- dim[[ii]]
    x[x < 1 | x > d] <- NA 
    (x - 1) * mfactor[[ii]]
  })
  t1 <- Reduce(function(a, b){ outer(a, b, '+') }, scaled_loc) + 1
  expect_equal(sum(is.na(t1)) - sum(is.na(tmp)), 0)
  expect_equal(sum(xor(is.na(t1), is.na(tmp))), 0)
  expect_length(t1, 0)
  expect_length(tmp, 0)
  
  # case 3: random set
  x = (1:5000000)
  # arr2df(x, c(0L, 25L, 50L), 9L)
  dim = c(50000L, 10L, 10L)
  locs = list(
    as.integer(sample(100000L, 300) - 2000),
    as.integer(sample(12)-1),
    as.integer(sample(12)-1)
  )
  target_dim = sapply(locs, length)
  tmp = loc2idx3(locs, dim);# tmp
  tmp[tmp < -9e18] <- NA
  dim(tmp) = target_dim
  
  # validate in R
  mfactor <- c(1, cumprod(dim))[seq_along(dim)]
  scaled_loc <- lapply(seq_along(locs), function(ii){
    x <- as.integer(locs[[ii]])
    d <- dim[[ii]]
    x[x < 1 | x > d] <- NA 
    (x - 1) * mfactor[[ii]]
  })
  t1 <- Reduce(function(a, b){ outer(a, b, '+') }, scaled_loc) + 1
  expect_equal(sum(is.na(t1)) - sum(is.na(tmp)), 0)
  expect_equal(sum(xor(is.na(t1), is.na(tmp))), 0)
  expect_equal(range(t1 - tmp, na.rm = TRUE), c(0,0))
  
  dim(x) = c(50000L, 10L, 10L)
  a = locs[[1]]; a[(a<1) | (a >50000)] = NA
  b = locs[[2]]; b[b<1 | b > 10] = NA
  c = locs[[3]]; c[c<1 | c > 10] = NA
  y2 <- x[a,b,c]
  expect_equal(range(y2 - tmp, na.rm = TRUE), c(0,0))
  
  y3 = tmp
  expect_equal(sum(abs(is.na(y2) - is.na(y3))), 0)
  expect_length(which(abs(is.na(y2) - is.na(y3)) > 0, arr.ind = TRUE), 0)
  expect_equal(range(y2 - y3, na.rm = TRUE), c(0,0))
  
})
