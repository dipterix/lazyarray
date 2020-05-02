# create arrays on disk

#' @title Create a lazy-array with given format and dimension
#' @param path path to a local drive to store array data
#' @param storage_format data type, choices are \code{"double"}, 
#' \code{"integer"}, \code{"character"}, and \code{"complex"}
#' @param dim integer vector, dimension of array, see \code{\link{dim}}
#' @param dimnames list of vectors, names of each dimension, see \code{\link{dimnames}}
#' @param multipart whether split array into multiple partitions
#' @param prefix character prefix of array partition
#' @param multipart_mode 1, or 2, mode of partition, see details.
#' @param compress_level 0 to 100, level of compression. 0 means
#' no compression, 100 means maximum compression. For persistent data,
#' it's recommended to set 100. Default is 50.
#' @return A \code{LazyArray} object
#' @details Lazy array stores array into hard drive, and load them on
#' demand. It differs from other packages such as \code{"bigmemory"}
#' that the internal reading uses multi-thread, which gains significant 
#' speed boost on SSD drives. 
#' 
#' One lazy array contains two parts: data file(s) and a meta file.
#' The data files can be stored in two ways: non-partitioned and 
#' partitioned. 
#' 
#' For non-partitioned data array, the dimension is 
#' set at the creation of the array and cannot be mutable once created
#' 
#' For partitioned data array, there are also two parition modes, 
#' defined by \code{`multipart_mode`}. For mode 1, each partition 
#' has the same dimension size as the array, with the last dimension
#' to be one. For example, a data with dimension \code{c(2,3,5)} 
#' partitioned with mode 1 will have each partition dimension stored
#' with \code{c(2,3,1)}. For mode 2, the last dimension will be dropped
#' when storing each partitions.
#' 
#' @export
create_lazyarray <- function(path, storage_format, dim, dimnames = NULL, 
                                 multipart = FALSE, prefix = "",
                                 multipart_mode = 1, compress_level = 50L){
  
  if(dir.exists(path)){
    stop("Path already exists.")
  }
  
  stopifnot(compress_level <= 100 & compress_level >= 0)
  
  if(length(dim) < 1){
    stop("length(dim) must be at least 2")
  }
  
  if(!is.list(dimnames) && !is.null(dimnames)){
    stop("dimnames must be a list or NULL")
  }
  
  # check if dim matches with dimnames
  if(!is.null(dimnames)){
    dnl = sapply(dimnames, length)
    if(length(dnl) != length(dim) || !all(dnl - dim == 0)){
      stop("Invalid dimension")
    }
  }
  
  multipart = as.logical(multipart)
  if(multipart && multipart_mode == 2 && length(dim) == 2){
    warning("multipart_mode must be 1 for matrix")
  }
  
  stopifnot(storage_format %in% c('character', 'double', 'int', 'complex'))
  
  if( multipart_mode == 1 ){
    part_dimension = dim
    part_dimension[length(dim)] = 1
  } else if(multipart_mode == 2){
    part_dimension = dim[-length(dim)]
  }
  
  #####
  meta = list(
    lazyarray_version = 0,
    file_format = 'fst',
    storage_format = storage_format,
    dim = dim,
    dimnames = dimnames,
    partitioned = multipart,
    prefix = prefix,
    part_dimension = part_dimension,
    postfix = '.fst',
    compress_level = compress_level
  )
  
  dir.create(path, showWarnings = TRUE, recursive = TRUE)
  
  meta_path = file.path(path, 'lazyarray.meta')
  save_yaml(meta, meta_path)
  
  LazyArray$new(path = path, read_only = FALSE)
  
}



