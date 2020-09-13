setGeneric("$")
setMethod(
  "$", "LazyArrayBase", function(x, name){
    if(name %in% names(x@binding_list)){
      return(x@binding_list[[name]]())
    }
    return(x@method_list[[name]])
  }
)

setGeneric("names")
setMethod(
  "names", "LazyArrayBase", function(x){
    c(names(x@binding_list), names(x@method_list))
  }
)

#' @export
`[.LazyArrayBase` <- function(x, ..., reshape = NULL, drop = TRUE){
  drop <- as.logical(drop)
  if(!is.null(reshape)){
    reshape <- as.numeric(reshape)
  }
  x$subset(environment(), reshape = reshape, drop = drop)
}

#' @export
`[<-.LazyArrayBase` <- function(x, ..., value){
  x$subsetAssign(environment(), value = value)
}


