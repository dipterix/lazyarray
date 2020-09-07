devtools::load_all()
arr <- lazyarray:::as.lazyarray(array(1:prod(1:4), 1:4))
a <- function(i, ...) {
  files = arr$get_partition_fpath()
  lazyarray:::lazySubset(files, environment(), dim(arr), 0.1)
}
dipsaus::lapply_async2(1:4, function(i, ...){
  a(,,,1)
  lazyarray:::getLazyThread()
})


  

a(, , , 1)
