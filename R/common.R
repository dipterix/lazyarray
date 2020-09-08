#' @title Set Number of Threads for Lazy Arrays
#' @description Set number of threads used by 'OpenMP' for both \code{lazyarray}
#' and \code{fstcore} packages.
#' @param nr_of_threads number of CPU cores to use, or \code{NULL} to 
#' stay unchanged
#' @param reset_after_fork whether to reset after forked process
#' @param max whether return maximum available threads
#' @return Number of cores currently used.
#' @seealso \code{\link[fstcore]{threads_fstlib}}
#' @name lazyarray-threads
#' @export
set_lazy_threads <- function(nr_of_threads = NULL, reset_after_fork = NULL){
  if(!is.null(reset_after_fork)){
    reset_after_fork <- isTRUE(reset_after_fork)
  } 
  if(is.null(nr_of_threads) || !is.numeric(nr_of_threads) || nr_of_threads == 0){
    nr_of_threads <- max(fstcore::threads_fstlib(), getLazyThread())
  }
  nr_of_threads = max(nr_of_threads, 1)
  setLazyThread(n = nr_of_threads, reset_after_fork = reset_after_fork)
  fstcore::threads_fstlib(nr_of_threads = nr_of_threads, reset_after_fork = reset_after_fork)
  getLazyThread()
}

#' @rdname lazyarray-threads
#' @export
get_lazy_threads <- function(max = FALSE){
  getLazyThread(max = isTRUE(max))
}
