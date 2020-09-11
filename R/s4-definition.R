# S4 class of lazyarray - refactored in C++
# Plan to change from R6 to S4 as S4 allows for more generics
# especailly specials like %*% don't fully support S3


# FstArray

setClass(
  "LazyArray2",
  slots = list(
    xPtr = "ANY",
    xMethods = "character",
    xFields = "character"
  )
)


lazyarray2 <- function(paths, dim, storage_format = "double"){
  if(length(dim) < 2){
    dim <- c(prod(dim), 1)
  }
  
  # int(13), double(14), complex(15), string(16)
  sxpcode <- switch (
    storage_format,
    'integer' = 13L,
    'double' = 14L,
    'complex' = 15L,
    'character' = 16L,
    stop("Unknown `storage_format`, only 'integer', 'double', 'complex', 'character' are allows")
  )
  nparts <- dim[[length(dim)]]
  
  dir_create(paths)
  
  paths <- normalizePath(paths, mustWork = TRUE)
  paths <- file.path(paths, sprintf("%s.fst", seq_len(nparts)))
  
  xPtr <- new(FstArray, paths, dim, sxpcode)
  
  re <- new("LazyArray2", xPtr = xPtr, xMethods = '', xFields = "")
  re
}



# Generics
setGeneric("names")

setMethod("names", c("LazyArray2"), definition = function(x){
  re@xMethods
})

# names(re)
