#' @rdname lazyarray
#' @export
lazymatrix <- function(
  path, storage_format, dim, dimnames = NULL, 
  compress_level = 50L, meta_name = 'lazyarray.meta', 
  read_only = FALSE, quiet = FALSE, ...
){
  if(file.exists(path) && !dir.exists(path)){
    stop('lazymatrix path must be a directory path, but a file was found.')
  }
  
  if(!dir.exists(path)){
    if(length(dim) != 2){
      stop('dim must have length of 2 for matrix')
    }
    # not exists, create a new one
    arr <- create_lazyarray(
      path = path, storage_format = storage_format, dim = dim,
      dimnames = dimnames,  compress_level = compress_level, meta_name = meta_name)
    return(as.lazymatrix(arr, read_only = read_only))
  }
  
  # path exists, locate meta_name
  if(file.exists(file.path(path, meta_name))){
    path <- normalizePath(path, mustWork = TRUE)
    arr <- ClassLazyMatrix$new(path = path, read_only = read_only, meta_name = meta_name)
    return(arr)
  }
  
  if(!quiet){
    message('meta file not found, create one with existing files')
  }
  
  
  # Otherwise meta_name does not exist
  if(length(dim) != 2){
    stop('dim must have length of 2 for matrix')
  }
  
  lazyarray(
    path = path, storage_format = storage_format, dim = dim, dimnames = dimnames, 
    compress_level = compress_level, meta_name = meta_name, 
    read_only = read_only, quiet = quiet
  )
  
  ClassLazyMatrix$new(path = path, read_only = read_only, meta_name = meta_name)
}



#' @rdname lazyarray
#' @export
as.lazymatrix <- function(x, read_only = FALSE, ...){
  UseMethod('as.lazymatrix')
}

#' @rdname lazyarray
#' @export
as.lazymatrix.default <- function(x, read_only = FALSE, storage_format, path = tempfile(), ...){
  x <- unlist(x)
  call <- match.call()
  call[[1]] <- quote(as.lazymatrix.array)
  eval(call)
}

#' @rdname lazyarray
#' @export
as.lazymatrix.array <- function(x, read_only = FALSE, storage_format, path = tempfile(), ...){
  if(dir.exists(path)){
    stop("path exists, please specify a different path")
  }
  if(missing(storage_format)){
    storage_format <- storage.mode(x)
  }
  
  dm <- dim(x)
  if(length(dm) < 2){
    dm <- c(length(x), 1)
  } else {
    dm <- c( prod(dm) / dm[[length(dm)]], dm[[length(dm)]] )
  }
  dim(x) <- dm
  
  if(dm[[1]] < dm[[2]]){
    dm = rev(dm)
    re <- lazyarray(path = path, storage_format = storage_format, dim = dm)
    
    ii <- 1
    for(ii in seq_len(dm[[2]])){
      re[,ii] <- x[ii,]
    }
    re <- as.lazymatrix.LazyArray(re, read_only = read_only, ...)
    re <- t(re)
  } else {
    re <- lazyarray(path = path, storage_format = storage_format, dim = dm)
    
    ii <- 1
    for(ii in seq_len(dm[[2]])){
      re[,ii] <- x[,ii]
    }
    
    re <- as.lazymatrix.LazyArray(re, read_only = read_only, ...)
  }
  re
  
}

#' @rdname lazyarray
#' @export
as.lazymatrix.LazyArray <- function(x, read_only = FALSE, storage_format, ...){
  path <- dirname(x$storage_path)
  meta_name <- x$meta_name
  if(is.na(read_only) || !is.logical(read_only)){
    read_only <- !x$can_write
  }
  
  if(missing(storage_format)){
    storage_format <- x$get_storage_format()
  }
  
  stopifnot(storage_format %in% x$storage_formats_avail)
  
  meta_name <- sprintf("%s_version-%s", storage_format, x$meta_name)
  
  # create header
  meta <- load_yaml(x$storage_path)
  meta$storage_format <- storage_format
  meta$dim <- c(length(x) / x$npart, x$npart)
  meta$part_dimension <- c(meta$dim[[1]], 1L)
  
  if(length(meta$dimnames) == length(x$dim)){
    dn <- meta$dimnames
    ndim <- length(dn)
    dk <- names(dn)
    if(length(dk == ndim)){
      meta$dimnames <- structure(list(NULL, dn[[ndim]]), names = c('', names(dn)[[ndim]]))
    } else {
      meta$dimnames <- list(NULL, dn[[ndim]])
    }
    
  } else {
    meta$dimnames <- NULL
  }
  
  save_yaml(meta, file.path(path, meta_name))
  
  ClassLazyMatrix$new(path = path, read_only = read_only, meta_name = meta_name)
}

#' @rdname lazyarray
#' @export
as.lazymatrix.LazyMatrix <- function(x, read_only = FALSE, storage_format, ...){
  re <- as.lazymatrix.LazyArray(x, read_only, storage_format, ...)
  re$`@transposed` <- x$`@transposed`
  re
}
