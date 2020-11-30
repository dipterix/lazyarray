library(lazyarray)
path <- "~/Desktop/lazyarray_data/"
dimension <- c(287, 200, 601, 84)
x <- lazyarray(path, storage_format = "double", dim = dimension)
x$make_readonly()
system.time(x[,,,1])
gc()
prod(dim(x))



