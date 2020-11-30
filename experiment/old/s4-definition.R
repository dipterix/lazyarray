ensure_path <- function(x){
  if(!dir.exists(x)){
    dir_create(x)
  }
  x
}

#' @export
register_lazyarray <- function(x){
  x@method_list$validate <- function(stopIfError = TRUE){
    x@method_list$with_instance(function(pointer){
      .Call("LazyArrayBase__validate", pointer, stopIfError)
    })
  }
  x@method_list$subset <- function(env, reshape = NULL, drop = TRUE){ 
    stopifnot(is.environment(env) || is.list(env))
    x@method_list$with_instance(function(pointer){
      .Call("LazyArrayBase__subset", pointer, env, reshape, isTRUE(drop))
    })
  }
  x@method_list$subsetAssign <- function(env, value){ 
    stopifnot(is.environment(env) || is.list(env))
    x@method_list$with_instance(function(pointer){
      .Call("LazyArrayBase__subsetAssign", pointer, value, env)
    })
  }
  
  
  x@binding_list$nparts <- function(){
    x@method_list$with_instance(function(pointer){
      .Call("LazyArrayBase__nparts", pointer)
    })
  }
  x@binding_list$read_only <- function(v){
    if(missing(v)) v <- NULL
    x@method_list$with_instance(function(pointer){
      .Call("LazyArrayBase__readOnly", pointer, v)
    })
  }
  x@binding_list$dim <- function(){
    x@method_list$with_instance(function(pointer){
      .Call("LazyArrayBase__getDim", pointer)
    })
  }
  x@binding_list$length_per_part <- function(){
    x@method_list$with_instance(function(pointer){
      .Call("LazyArrayBase__partLen", pointer) 
    })
  }
  x@binding_list$data_type <- function(){
    x@method_list$with_instance(function(pointer){
      .Call("LazyArrayBase__dataType", pointer) 
    })
  }
  invisible(x)
}

make_methods_lazymatrix <- function(x){
  invisible(x)
}

#' @export
check_data_type <- function(dataType){
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


#' @title Base S4 class for 'LazyArray'
#' @export
setClass("LazyArrayBase", slots = c(
  classname = "character", method_list = "list", 
  binding_list = "list", dimension = "numeric", dataType = "integer"
), contains = 'oldClass')

#' @export
setClass("FstArray", contains = "LazyArrayBase", slots = c(rootPath = "character", compression = "integer", uniformEncoding = "logical"))

#' @export
setClass("FstMatrix", contains = "FstArray", slots = c(transposed = "logical"))

setMethod(
  "initialize", "LazyArrayBase",
  function(.Object, ...) {
    stop("LazyArrayBase is an abstract class. Please create from sub-classes")
  }
)


setMethod(
  "initialize", "FstArray",
  function(.Object, rootPath, dimension, dataType = "double", compression = 50, uniformEncoding = TRUE) {
    stopifnot(is.numeric(dimension))
    .Object@classname <- "FstArray"
    .Object@dimension <- dimension
    .Object@dataType <- check_data_type(dataType)[[1]]
    .Object@compression <- as.integer(compression)[[1]]
    .Object@uniformEncoding <- isTRUE(uniformEncoding)[[1]]
    .Object@rootPath <- as.character(rootPath)[[1]]
    
    if(!is.list(.Object@method_list)){
      .Object@method_list <- list()
    }
    if(!is.list(.Object@binding_list)){
      .Object@binding_list <- list()
    }
    .Object@method_list$with_instance <- function(FUN){
      ensure_path(.Object@rootPath)
      pointer <- .Call("FstArray__new", .Object@rootPath, .Object@dimension, .Object@dataType, .Object@compression, .Object@uniformEncoding)
      FUN(pointer)
    }
    .Object <- register_lazyarray(.Object)
    .Object
  }
)

setMethod(
  "initialize", "FstMatrix",
  function(.Object, rootPath, dimension, transposed = FALSE, dataType = "double", compression = 50, uniformEncoding = TRUE) {
    stopifnot(is.numeric(dimension))
    .Object@classname <- "FstMatrix"
    .Object@dimension = dimension
    .Object@dataType <- check_data_type(dataType)[[1]]
    .Object@compression <- as.integer(compression)[[1]]
    .Object@uniformEncoding <- isTRUE(uniformEncoding)[[1]]
    .Object@rootPath <- as.character(rootPath)[[1]]
    if( transposed ){ transposed <- TRUE } else { transposed <- FALSE }
    .Object@transposed <- transposed;
    
    if(!is.list(.Object@method_list)){
      .Object@method_list <- list()
    }
    if(!is.list(.Object@binding_list)){
      .Object@binding_list <- list()
    }
    .Object@method_list$with_instance <- function(FUN){
      ensure_path(.Object@rootPath)
      FUN(.Call("FstMatrix__new", .Object@rootPath, .Object@dimension, 
                .Object@transposed, .Object@dataType, .Object@compression, .Object@uniformEncoding))
    }
    .Object <- register_lazyarray(.Object)
    .Object <- make_methods_lazymatrix(.Object)
    .Object
  }
)

setOldClass(c("FstMatrix", "FstArray", "LazyArrayBase"))

#' @export
lazyarray2 <- function(dim, storage_mode = "double", ..., type = 'fstarray'){
  UseMethod("lazyarray2", structure(type, class = type))
}

#' @export
lazyarray2.fstarray <- function(path, dim, storage_mode = "double", ..., type){
  x <- new("FstArray", path, dim, storage_mode, ...)
  # new_s3class <- c(oldClass(x), "LazyArrayBase")
  # attributes(new_s3class) <- attributes(oldClass(x))
  # oldClass(x) <- new_s3class
  x
}


