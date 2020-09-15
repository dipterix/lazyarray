# Rcpp::loadModule('LazyArrayModules', TRUE)

.onUnload <- function (libpath) {
  library.dynam.unload("lazyarray", libpath)
}


.onLoad <- function(libname, pkgname){
  options('lazyarray.parallel.strategy' = FALSE)
  options('lazyarray.chunk_memory' = 80)
  ncores <- parallel::detectCores(logical = TRUE)
  options('lazyarray.nthreads' = ncores)
  set_lazy_threads(ncores, TRUE)
}


#' Schedule parallel processes for \code{LazyArray}
#' @description Enable parallel processing, need \code{dipsaus} to be installed.
#' For \code{"callr"} strategy, please also install \code{future.callr}.
#' @param enabled whether multiple-process strategy is enabled
#' @param strategy strategies to apply, see \code{\link[future]{plan}} for
#' some of the details. For \code{"callr"} plan, please install package
#' @param workers number of 'CPU' cores to use. 
#' \code{future.callr}.
#' @param ... Further passed to \code{\link[future]{plan}}
#' 
#' @export
lazy_parallel <- function(
  strategy = c(
    'multisession', 'multicore', 
    'multiprocess', 'cluster', 'remote', 'callr'),
  enabled = TRUE, workers = 'auto',
  ...
  ){
  
  options('lazyarray.parallel.strategy' = FALSE)
  strategy <- match.arg(strategy)
  if(!has_dipsaus()){
    stop('Package dipsaus not detected. Please install.packages("dipsaus")')
  }
  
  if(isTRUE(workers == 'auto')){
    # get maximum available workers
    workers <- future::availableCores()
  }
  
  if(enabled){
    
    if(strategy == 'multicore'){
      dipsaus::make_forked_clusters(..., workers = workers)
    } else if(strategy == 'callr'){
      future::plan(future.callr::callr, ..., workers = workers)
    } else {
      args <- list(...)
      tryCatch({
        future::plan(strategy, ..., workers = workers)
      }, error = function(e){
        do.call(future::plan, c(list(strategy), args))
      })
    }
    
  } else {
    future::plan('sequential')
  }
  
  invisible()
}

setOldClass(c('FstArray', 'AbstractLazyArray', 'R6'))
setOldClass(c('FileArray', 'AbstractLazyArray', 'R6'))


setGeneric("typeof")


#' Type of \code{LazyArray}
#' @param x a \code{LazyArray} or an R object
#' @return The type of data stored in the input
#' @exportMethod typeof
setMethod("typeof", signature(x="AbstractLazyArray"), function(x){
  x$storage_format
})


setGeneric("crossprod")
setGeneric("tcrossprod")

#' Matrix Crossproduct
#' @param x a \code{LazyArray} or an R matrix
#' @param y \code{NULL} or matrix
#' @param weights numeric vector used as weight
#' @param ... passed to further methods
#' @return Matrix of cross product if data is small, or \code{LazyMatrix} if
#' matrix is too large
#' @name crossprod
#' 
#' @examples 
#' 
#' x <- matrix(1:100, 50)
#' crossprod(x)
#' 
#' lazy_x <- as.lazymatrix(x)
#' crossprod(lazy_x)[]
#' 
#' weights <- (1:50)/50
#' 
#' t(x) %*% diag(weights) %*% x
#' lazy_crossprod(lazy_x, weights = weights)
#' 
#' \dontrun{
#' 
#' # large data set ~ 1.6GB
#' x <- as.lazymatrix(matrix(rnorm(2e8), ncol = 2))
#' 
#' crossprod(x)
#' }
#' 
#' 
NULL

#' @rdname crossprod
#' @exportMethod crossprod
setMethod("crossprod", signature(x="AbstractLazyArray", y = 'AbstractLazyArray'), function(x, y = NULL, weights = NULL, ...){
  lazy_crossprod(x, y, weights = weights, ...)
})

setMethod("crossprod", signature(x="AbstractLazyArray", y = 'NULL'), function(x, y = NULL, weights = NULL, ...){
  lazy_crossprod(x, NULL, weights = weights, ...)
})

setMethod("crossprod", signature(x="AbstractLazyArray", y = "missing"), function(x, y = NULL, weights = NULL, ...){
  lazy_crossprod(x, NULL, weights = weights, ...)
})

setMethod("crossprod", signature(x="AbstractLazyArray", y = 'matrix'), function(x, y = NULL, weights = NULL, ...){
  if(!is.null(weights)){
    stopifnot(length(weights) == x$partition_length)
    res <- lapply(seq_len(x$npart), function(ii){
      x$get_partition_data(ii, reshape = c(1, x$partition_length)) %*% (y * weights)
    })
  } else {
    res <- lapply(seq_len(x$npart), function(ii){
      x$get_partition_data(ii, reshape = c(1, x$partition_length)) %*% y
    })
  }
  
  do.call('rbind', res)
})


lazy_crossprod <- function(x, y = NULL, weights = NULL, ...){
  
  if(!is.null(weights)){
    stopifnot(length(weights) == x$partition_length)
  }
  
  new_x <- as.lazymatrix(x)
  new_x$make_readonly()
  if(is.null(y)){
    yisx <- TRUE
    new_y <- new_x
  } else {
    yisx <- isTRUE(x$storage_path == y$storage_path && x$get_file_format() == y$get_file_format())
    new_y <- as.lazymatrix(y)
  }
  
  if(length(weights)){
    ftile <- filematrix::fm.create(tempfile(), nrow = length(weights), ncol = 1)
    ftile[] <- weights
    on.exit(filematrix::close(ftile))
    
    chunk_map(new_x, map_fun = function(data, ii, idx_range){
      idx <- seq.int(idx_range[[1]], idx_range[[2]])
      if(yisx){
        return(crossprod(data, data * as.vector(ftile[idx,1])))
      } else {
        sub_y <- y[idx,,drop=FALSE] * as.vector(ftile[idx,1])
        return(crossprod(data, sub_y))
      }
    }, reduce = function(mapped){
      Reduce('+', mapped)
    }, ...)
    
  } else {
    chunk_map(new_x, map_fun = function(data, ii, idx_range){
      
      if(yisx){
        return(crossprod(data))
      } else {
        sub_y <- y[seq.int(idx_range[[1]], idx_range[[2]]),,drop=FALSE]
        return(crossprod(data, sub_y))
      }
    }, reduce = function(mapped){
      Reduce('+', mapped)
    }, ...)
  }
  
}
