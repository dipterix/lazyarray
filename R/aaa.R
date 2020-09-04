#' @importFrom R6 R6Class
#' @importFrom Rcpp evalCpp
#' @importFrom Rcpp sourceCpp
#' @importFrom yaml read_yaml
#' @importFrom yaml write_yaml
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom utils str
#' @importFrom utils capture.output
#' @importFrom stats sd
#' @importFrom stats model.response
#' @importFrom stats is.empty.model
#' @importFrom stats model.matrix
#' @importFrom stats contrasts
#' @importFrom methods setGeneric
#' @importFrom methods setMethod
#' @importFrom methods signature
#' @useDynLib lazyarray, .registration = TRUE
NULL

has_dipsaus <- function(){
  system.file('', package = 'dipsaus') != ''
}
