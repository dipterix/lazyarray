# Rcpp::loadModule('LazyArrayModules', TRUE)

.onUnload <- function (libpath) {
  setLazyThread(1L, FALSE)
  library.dynam.unload("lazyarray", libpath)
}


.onLoad <- function(libname, pkgname){
  options('lazyarray.parallel.strategy' = FALSE)
  options('lazyarray.chunk_memory' = 80)
  setLazyThread(4L, FALSE)
}


#' Schedule parallel processes for \code{LazyArray}
#' @description Enable parallel processing, need \code{dipsaus} to be installed.
#' For \code{"callr"} strategy, please also install \code{future.callr}.
#' @param enabled whether multiple-process strategy is enabled
#' @param strategy strategies to apply, see \code{\link[future]{plan}} for
#' some of the details. For \code{"callr"} plan, please install package
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

setOldClass(c('LazyMatrix', 'LazyArray', 'R6'))


setGeneric("typeof")


#' Type of \code{LazyArray}
#' @param x a \code{LazyArray} or an R object
#' @return The type of data stored in the input
#' @exportMethod typeof
setMethod("typeof", signature(x="LazyArray"), function(x){
  typeof(x$`@sample_data`())
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
setMethod("crossprod", signature(x="LazyMatrix", y = 'ANY'), function(x, y = NULL){
  lazy_crossprod(x, y)
})

#' @rdname crossprod
#' @exportMethod tcrossprod
setMethod("tcrossprod", signature(x="LazyMatrix", y = 'ANY'), function(x, y = NULL){
  lazy_crossprod(t(x), y)
})



#' @rdname crossprod
#' @export
lazy_crossprod <- function(x, y = NULL, weights = NULL, ...){
  if(!is.null(y)){
    y <- as.lazymatrix(y)
    if(x$storage_path != y$storage_path || !xor(x$`@transposed`, y$`@transposed`) || x$`@transposed`){
      return(lazy_matmul(t(x), y, weights = weights, ...))
    }
  }
  x <- as.lazymatrix(x, read_only = TRUE)
  
  if(!x$`@transposed`){
    # t(x) %*% y
    
    if(!is.null(weights)){
      re <- chunk_map(x, function(chunk_x, ii, idx_range){
        crossprod(chunk_x, chunk_x * weights[seq.int(idx_range[1], idx_range[2])])
      }, reduce = function(mapped){
        Reduce('+', mapped)
      })
    } else {
      re <- chunk_map(x, function(chunk_x){
        crossprod(chunk_x)
      }, reduce = function(mapped){
        Reduce('+', mapped)
      })
    }
    return(re)
  } else {
    return(lazy_matmul(t(x), x, weights = weights, ...))
  }
  
}
