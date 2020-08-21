# lazyarray (col major)
path <- tempfile()
arr <- lazyarray::lazyarray(path, dim = c(99072112, 5), storage_format = 'double')
x <- as.lazymatrix(arr)
y <- x$transpose()
for(ii in 1:5){
  arr[,ii] <- rnorm(99072112)
}

{
  a = Sys.time()
  # fstcore::threads_fstlib(4)
  # dipsaus::make_forked_clusters(4)
  future::plan(future::multisession)
  nrows <- 99072112
  # xtx <- dipsaus::async_works(1:500, function(ii, ...){
  #   arr <- lazyarray::lazyarray(path, dim = c(99072112, 5), storage_format = 'double')
  #   idx <- seq.int((ii-1) * 200000 + 1, ii * 200000)
  #   idx <- idx[idx <= nrows]
  #   if(!length(idx)){ return(list(
  #     xtx = matrix(0, 5,5),
  #     xty = rep(0, 5)
  #   )) }
  #   
  #   tmp <- cbind(1, arr[idx, c(1,2,4,5)])
  #   
  #   list(
  #     xtx = t(tmp) %*% tmp,
  #     xty = t(tmp) %*% arr[idx, 3]
  #   )
  #   
  # }, .globals =  list(nrows = nrows, path = path), .nworkers = 4)
  
  xtx <- dipsaus::lapply_async2(1:500, function(ii, ...){
    arr <- lazyarray::lazyarray(path, dim = c(99072112, 5), storage_format = 'double')
    idx <- seq.int((ii-1) * 200000 + 1, ii * 200000)
    idx <- idx[idx <= nrows]
    if(!length(idx)){ return(list(
      xtx = matrix(0, 5,5),
      xty = rep(0, 5)
    )) }
    
    tmp <- cbind(1, arr[idx, c(1,2,4,5)])
    
    list(
      xtx = t(tmp) %*% tmp,
      xty = t(tmp) %*% arr[idx, 3]
    )
    
  })
  
  xty <- Reduce('+', lapply(1:500, function(ii){
    xtx[[ii]]$xty
  }))
  xtx <- Reduce('+', lapply(1:500, function(ii){
    xtx[[ii]]$xtx
  }))
  coef <- solve(xtx) %*% xty
  b <- Sys.time(); b - a
}

# single thread
# Time difference of 15.4888 secs

# future, 4 cores, no openmp
# Time difference of 6.66791 secs

# openmp, 4 cores, one process
# Time difference of 18.39945 secs

# ---------------------------------------------------------------------
# lazyarray (row major)
path <- tempfile()
arr <- lazyarray::lazyarray(path, dim = c(200000, 500, 5), storage_format = 'double', multipart_mode = 2)
for(ii in 1:5){
  print(ii)
  arr[,,ii] <- rnorm(200000*500)
}

{
  # dipsaus::make_forked_clusters(4)
  future::plan(future::multisession)
  a = Sys.time()
  
  nrows <- 99072112
  xtx <- dipsaus::lapply_async2(1:500, function(ii, nrows, arr){
    idx <- seq.int((ii-1) * 200000 + 1, ii * 200000)
    idx <- idx <= nrows
    if(!any(idx)){ return(list(
      xtx = matrix(0, 5,5),
      xty = rep(0, 5)
    )) }
    tmp <- cbind(1, arr[idx, ii, c(1,2,4,5), drop = TRUE])
    list(
      xtx = t(tmp) %*% tmp,
      xty = t(tmp) %*% arr[idx, ii, 3, drop = TRUE]
    )
  }, plan = FALSE, FUN.args = list(nrows = nrows, arr = arr))#, .globals = list(nrows = nrows, arr = arr), .rs = TRUE)
  
  xty <- Reduce('+', lapply(1:500, function(ii){
    xtx[[ii]]$xty
  }))
  xtx <- Reduce('+', lapply(1:500, function(ii){
    xtx[[ii]]$xtx
  }))
  
  coef <- solve(xtx) %*% xty
  b <- Sys.time(); b - a
}

# Time difference of 13.36511 secs

coef - coefficients(res)

