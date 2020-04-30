.onUnload <- function (libpath) {
  library.dynam.unload("lazyarray", libpath)
}
