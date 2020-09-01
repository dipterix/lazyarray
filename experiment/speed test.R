library(lazyarray)
library(dipsaus)
library(future)
options(future.fork.enable = TRUE)
options('width' = 75)
path <- "~/Desktop/lazyarray_data/"
dimension <- c(287, 200, 601, 84)
x <- lazyarray(path, storage_format = "double", dim = dimension)


subarray <- x[,,,1,drop=FALSE]

tempf <- tempfile()
temp_rds <- paste0(tempf, '.rds')
temp_h5 <- paste0(tempf, '.h5')
temp_lazy <- paste0(tempf, '.lazyarray')

unlink(temp_rds)
unlink(temp_h5)
unlink(temp_lazy, recursive = TRUE)

rds <- bench::mark(
  {
    saveRDS(subarray, temp_rds)
  },
  {
    raveio::save_h5(subarray, file = temp_h5, name = '/data', chunk = c(287, 1, 10, 1))
  },
  {
    y <- lazyarray(temp_lazy, storage_format = 'double', dim = dim(subarray))
    y[] <- subarray
  }, memory = TRUE, time_unit = 's', iterations = 1, check = FALSE
)


