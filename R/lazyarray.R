#' @export
lazyarray <- function(
  path, dim, read_only = FALSE, type = c("filearray", "fstarray"), 
  storage_format = c('double', 'integer', 'complex', 'character'), 
  meta_name = 'lazyarray.meta'){
  
  call <- match.call()
  if(!missing(type)) {
    call[["type"]] <- NULL
    has_type <- TRUE
  } else {
    has_type <- FALSE
  }
  
  type <- match.arg(type)
  storage_format <- match.arg(storage_format)
  
  if(storage_format %in% c("character", "complex")){
    if(has_type && type == 'filearray'){
      stop("FileArray does not support `character` nor `complex` data types.")
    } else {
      # automatically switch to fst format 
      type <- 'fstarray'
    }
  }
  
  switch (type,
    'fstarray' = {
      call[[1]] <- quote(fstarray)
    },
    'filearray' = {
      call[[1]] <- quote(filearray)
    }
  )
  eval(call)
}

#' @export
fstarray <- function(
  path, dim, read_only = FALSE,
  storage_format = c('double', 'integer', 'complex', 'character'),
  meta_name = 'lazyarray.meta'){
  storage_format <- match.arg(storage_format)
  if (missing(dim)) {
    re <-
      FstArray$new(
        path = path ,
        storage_format = storage_format,
        read_only = !isFALSE(read_only),
        meta_name = meta_name
      )
  } else {
    re <-
      FstArray$new(
        path = path ,
        dim = dim,
        storage_format = storage_format,
        read_only = !isFALSE(read_only),
        meta_name = meta_name
      )
  }
  if (re$can_write) {
    re$save_meta()
  }
  re
}


#' @export
filearray <- function(
  path, dim, read_only = FALSE,
  storage_format = c('double', 'integer'),
  meta_name = 'lazyarray.meta'){
  storage_format <- match.arg(storage_format)
  if (missing(dim)) {
    re <-
      FileArray$new(
        path = path ,
        storage_format = storage_format,
        read_only = !isFALSE(read_only),
        meta_name = meta_name
      )
  } else {
    re <-
      FileArray$new(
        path = path ,
        dim = dim,
        storage_format = storage_format,
        read_only = !isFALSE(read_only),
        meta_name = meta_name
      )
  }
  if (re$can_write) {
    re$save_meta()
  }
  re
}


#' @export
as.lazymatrix <- function(x, ...){
  UseMethod("as.lazymatrix")
}

#' @export
as.lazymatrix.default <- function(x, ...){
  re <- as.lazyarray(x, ...)
  as.lazymatrix.AbstractLazyArray(re)
}

#' @export
as.lazymatrix.AbstractLazyArray <- function(x, ...){
  x <- x$clone()
  x$make_readonly()
  dim(x) <- c(x$partition_length, x$npart)
  x
}


#' @export
as.lazyarray <- function(x, path, type = "filearray", ...){
  UseMethod("as.lazyarray")
}

#' @export
as.lazyarray.default <- function(x, path, type = "filearray", dim, storage_format, ...){
  
  if(missing(path)){
    path <- tempfile()
  }
  if(missing(dim)){
    dim <- attr(x, "dim")
    if(!length(dim)){
      dim <- c(length(x), 1)
    }
  }
  stopifnot(length(dim) >= 2)
  
  if(missing(storage_format)){
    storage_format <- storage.mode(x)
  }
  
  
  re <- lazyarray(path, dim = dim, storage_format = storage_format, type = type, ...)
  dim(x) <- dim
  re[] <- x
  re
  
}

#' @export
as.lazyarray.AbstractLazyArray <- function(x, path, type = "filearray", ...){
  if(!missing(type)){
    warning("x is already a lazyarray, type will be ignored")
  }
  x
}

