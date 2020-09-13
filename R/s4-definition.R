#' @exportClass LazyArrayBase
setClass("LazyArrayBase", slots = c(pointer = "externalptr", method_list = "list", binding_list = "list"))

#' @exportClass FstArray
setClass("FstArray", contains = "LazyArrayBase")

#' @exportClass FstMatrix
setClass("FstMatrix", contains = "FstArray")


make_methods_base <- function(x){
  if(!is.list(x@method_list)){
    x@method_list <- list()
  }
  if(!is.list(x@binding_list)){
    x@binding_list <- list()
  }
  
  x@method_list$validate <- function(stopIfError = TRUE){ .Call("LazyArrayBase__validate", x@pointer, stopIfError) }
  x@method_list$subset <- function(..., reshape = NULL, drop = TRUE){ .Call("LazyArrayBase__subset", x@pointer, environment(), reshape, isTRUE(drop)) }
  
  x@binding_list$nparts <- function(){ .Call("LazyArrayBase__nparts", x@pointer) }
  x@binding_list$read_only <- function(v){
    if(missing(v)) v <- NULL
    function(){ .Call("LazyArrayBase__readOnly", x@pointer, v) }
  }
  x@binding_list$dim <- function(){ .Call("LazyArrayBase__getDim", x@pointer) }
  x@binding_list$length_per_part <- function(){ .Call("LazyArrayBase__partLen", x@pointer) }
  x@binding_list$data_type <- function(){ .Call("LazyArrayBase__dataType", x@pointer) }
  invisible(x)
}

make_methods_lazymatrix <- function(x){
  invisible(x)
}



check_data_type <- function(dataType){
  print('lol')
  if(is.numeric(dataType)){
    dataType <- as.integer(dataType)
  } else if(is.character(dataType)){
    dataType <- list("int" = 13L, "integer" = 13L, "numeric" = 14L, "double" = 14L, "complex" = 15L, "character" = 16L, "string" = 16L)[[dataType]]
  }
  if(!isTRUE(is.integer(dataType))){
    stop("dataType must be character or integer. Choices are: integer(13), double(14), complex(15), or string(16)")
  }
  dataType
}

setMethod(
  "initialize", "LazyArrayBase",
  function(.Object, dimension, dataType = "double") {
    stopifnot(is.numeric(dimension))
    dataType <- check_data_type(dataType)
    .Object@pointer <- .Call("LazyArrayBase__new", dimension, dataType)
    .Object <- make_methods_base(.Object)
    .Object
  }
)


setMethod(
  "initialize", "FstArray",
  function(.Object, rootPath, dimension, dataType = "double", compression = 50, uniformEncoding = TRUE) {
    stopifnot(is.numeric(dimension))
    dataType <- check_data_type(dataType)
    if(!dir.exists(rootPath)){
      dir_create(rootPath)
    }
    compression <- as.integer(compression)
    uniformEncoding <- isTRUE(uniformEncoding)
    .Object@pointer <- .Call("FstArray__new", rootPath, dimension, dataType, compression, uniformEncoding)
    .Object <- make_methods_base(.Object)
    .Object
  }
)

setMethod(
  "initialize", "FstMatrix",
  function(.Object, rootPath, dimension, transposed = FALSE, dataType = "double", compression = 50, uniformEncoding = TRUE) {
    stopifnot(is.numeric(dimension))
    dataType <- check_data_type(dataType)
    if(!dir.exists(rootPath)){
      dir_create(rootPath)
    }
    compression <- as.integer(compression)
    uniformEncoding <- isTRUE(uniformEncoding)
    if( transposed ){ transposed <- TRUE } else { transposed <- FALSE }
    .Object@pointer <- .Call("FstMatrix__new", rootPath, dimension, transposed, dataType, compression, uniformEncoding)
    .Object <- make_methods_base(.Object)
    .Object <- make_methods_lazymatrix(.Object)
    .Object
  }
)



