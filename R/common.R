#' @title Set Number of Threads for Lazy Arrays
#' @description A ported function from \code{\link[fstcore]{threads_fstlib}}.
#' @param nr_of_threads number of CPU cores to use, or \code{NULL} to 
#' use all cores
#' @param reset_after_fork whether to reset after forked process
#' @return Number of cores currently used.
#' @seealso \code{\link[fstcore]{threads_fstlib}}
#' @export
set_lazy_threads <- function(nr_of_threads = NULL, reset_after_fork = NULL){
  fstcore::threads_fstlib(nr_of_threads = nr_of_threads, reset_after_fork = reset_after_fork)
}
