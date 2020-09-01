library(lazyarray)
library(dipsaus)
library(future)
options(future.fork.enable = TRUE)
options('width' = 75)
path <- "~/Desktop/lazyarray_data/"
dimension <- c(287, 200, 601, 84)
x <- lazyarray(path, storage_format = "double", dim = dimension)

tempf <- tempfile()

prof <- lapply(1:10, function(ii){
  print(ii)
  subarray <- x[,,,1:ii,drop=FALSE]
  
  temp_rds <- paste0(tempf, '.rds')
  temp_h5 <- paste0(tempf, '.h5')
  temp_lazy <- paste0(tempf, '.lazyarray')
  
  unlink(temp_rds)
  unlink(temp_h5)
  unlink(temp_lazy, recursive = TRUE)
  
  # pause for a while
  Sys.sleep(2)
  
  plan('sequential')
  
  rds <- bench::mark(
    {
      saveRDS(subarray, temp_rds)
    },
    {
      raveio::save_h5(subarray, file = temp_h5, name = '/data', chunk = c(100, 1, 10, 1))
    },
    {
      lazyarray::set_lazy_threads(1L)
      y <- lazyarray(temp_lazy, storage_format = 'double', dim = dim(subarray))
      y[] <- subarray
    }, 
    {
      lazyarray::set_lazy_threads(2L)
      unlink(temp_lazy, recursive = TRUE)
      y <- lazyarray(temp_lazy, storage_format = 'double', dim = dim(subarray))
      y[] <- subarray
    }, 
    {
      lazyarray::set_lazy_threads(3L)
      unlink(temp_lazy, recursive = TRUE)
      y <- lazyarray(temp_lazy, storage_format = 'double', dim = dim(subarray))
      y[] <- subarray
    }, 
    {
      lazyarray::set_lazy_threads(4L)
      unlink(temp_lazy, recursive = TRUE)
      y <- lazyarray(temp_lazy, storage_format = 'double', dim = dim(subarray))
      y[] <- subarray
    }, 
    memory = TRUE, time_unit = 's', iterations = 1, check = FALSE, filter_gc = FALSE
  )
  
  saveRDS(object = rds, file = sprintf('~/Dropbox/projects/lazyarray/experiment/data/%d.rds', ii))
  rds
})



temp_rds <- paste0(tempf, '.rds')
temp_h5 <- paste0(tempf, '.h5')
temp_lazy <- paste0(tempf, '.lazyarray')

plan('sequential')

lapply(1:10, function(ii){
  rds <- bench::mark(
    {
      raveio::load_h5(file = temp_h5, name = '/data')[,,,1:ii]
    },
    {
      lazyarray::set_lazy_threads(1L)
      y <- lazyarray(temp_lazy, storage_format = 'double', dim = dim(subarray))
      y[,,,1:ii]
    }, 
    {
      lazyarray::set_lazy_threads(2L)
      y <- lazyarray(temp_lazy, storage_format = 'double', dim = dim(subarray))
      y[,,,1:ii]
    }, 
    {
      lazyarray::set_lazy_threads(3L)
      y <- lazyarray(temp_lazy, storage_format = 'double', dim = dim(subarray))
      y[,,,1:ii]
    }, 
    {
      lazyarray::set_lazy_threads(4L)
      y <- lazyarray(temp_lazy, storage_format = 'double', dim = dim(subarray))
      y[,,,1:ii]
    }, 
    memory = TRUE, time_unit = 's', iterations = 1, check = FALSE, filter_gc = FALSE
  )
  
  saveRDS(object = rds, file = sprintf('~/Dropbox/projects/lazyarray/experiment/data/read_%d.rds', ii))
})



