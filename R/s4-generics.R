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
