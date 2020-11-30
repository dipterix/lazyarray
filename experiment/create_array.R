# create arrays on disk

#' @title Create a lazy-array with given format and dimension
#' @author Zhengjia Wang
#' @description Create a directory to store lazy-array. The path must be missing. See \code{\link{load_lazyarray}} for more details
#' @param path path to a local drive to store array data
#' @param storage_format data type, choices are \code{"double"}, 
#' \code{"integer"}, \code{"character"}, and \code{"complex"}
#' @param dim integer vector, dimension of array, see \code{\link{dim}}
#' @param dimnames list of vectors, names of each dimension, see \code{\link{dimnames}}
#' @param compress_level 0 to 100, level of compression. 0 means
#' no compression, 100 means maximum compression. For persistent data,
#' it's recommended to set 100. Default is 50.
#' @param meta_name header file name, default is \code{"lazyarray.meta"}
#' @return A \code{ClassLazyArray} instance
#' @details Lazy array stores array into hard drive, and load them on
#' demand. It uses multi-thread which gains significant 
#' speed boost on solid state drives. 
#' 
#' For examples, see \code{\link{lazyarray}}.
#' 
#' @export
create_lazyarray <- function(
  path, storage_format, dim, dimnames = NULL, compress_level = 50L, 
  meta_name = 'lazyarray.meta'){
  
  if(dir.exists(path)){
    stop("Path already exists.")
  }
  multipart <- TRUE
  multipart_mode <- 1L
  
  stopifnot(compress_level <= 100 & compress_level >= 0)
  
  if(length(dim) < 1){
    stop("length(dim) must be at least 2")
  }
  
  if(!is.list(dimnames) && !is.null(dimnames)){
    stop("dimnames must be a list or NULL")
  }
  
  # check if dim matches with dimnames
  if(!is.null(dimnames)){
    dnl <- sapply(dimnames, length)
    if(length(dnl) != length(dim) || !all(dnl - dim == 0)){
      stop("Invalid dimension")
    }
  }
  
  stopifnot(storage_format %in% c('character', 'double', 'integer', 'complex'))
  
  part_dimension <- dim
  part_dimension[length(dim)] <- 1
  
  #####
  meta <- list(
    lazyarray_version = 0,
    file_format = 'fst',
    storage_format = storage_format,
    dim = dim,
    dimnames = dimnames,
    # partitioned = multipart,
    # prefix = prefix,
    part_dimension = part_dimension,
    postfix = '.fst',
    compress_level = compress_level
    # file_names = file_names
  )
  
  dir.create(path, showWarnings = TRUE, recursive = TRUE)
  path <- normalizePath(path, mustWork = TRUE)
  
  meta_path <- file.path(path, meta_name)
  save_yaml(meta, meta_path)
  
  ClassLazyArray$new(path = path, read_only = FALSE, meta_name = meta_name)
  
}



#' @title Load Lazy Array from Given Path
#' @author Zhengjia Wang
#' @param path character, path of the array
#' @param read_only whether setting data is allowed
#' @param meta_name header file name, default is \code{"lazyarray.meta"}
#' @return A \code{ClassLazyArray} instance
#' @examples 
#' 
#' path <- tempfile()
#' create_lazyarray(path, 'double', dim = c(3,4,5))
#' 
#' x <- load_lazyarray(path, read_only = FALSE)
#' x[2,3:4, 2:1] <- 1:4
#' x[ , , 2]
#' 
#' # Changing dimension for multiple partition data only
#' dim(x) <- c(3,4,6)
#' dimnames(x) <- list(dim1 = as.character(1:3),
#'                     dim2 = letters[1:4], 
#'                     dim3 = LETTERS[1:6])
#' names(dimnames(x))
#' subset(x, dim1 ~ dim1 == '2', dim2 ~ dim2 %in% c('a', 'c'), drop = TRUE)
#' 
#' # Free up space
#' x$remove_data()
#' 
#' \donttest{
#' 
#' # This example needs at least 4 GB hard disk space and it takes
#' # time to run for performance profile
#' 
#' # Speed test
#' path <- tempfile()
#' x <- create_lazyarray(path, 'complex', dim = c(100,200,300,20))
#' 
#' # automatically call x$remove_data() upon garbage collection
#' x$flag_auto_clean(TRUE)
#' 
#' 
#' # set data (4 GB data) using 4 cores, compression level 50
#' # data creation ~10 s, disk IO ~15-20 seconds, ~250MB/s
#' 
#' system.time({
#'   lapply(1:20, function(ii){
#'     # Generating partition data (~10 sec)
#'     tmp <- rnorm(100*200*300) * (1+2i)
#'     
#'     # Write to disk (~16 sec)
#'     x[,,,ii] <- tmp
#'     NULL
#'   })
#' })
#' 
#' # Reading 64 MB data using 4 cores
#' # ~0.25 seconds
#' 
#' system.time({
#'   x[1:100, sample(200, 200), 100:1, 2:4]
#' })
#' 
#' # This call requires 4GB of RAM
#' # Reading all 4GB data using 4 cores
#' # ~4 seconds (1 GB/s)
#' 
#' system.time({
#'   x[]
#' })
#' 
#' }
#' 
#' @export
load_lazyarray <- function(path, read_only = TRUE, meta_name = 'lazyarray.meta'){
  path <- normalizePath(path, mustWork = TRUE)
  ClassLazyArray$new(path = path, read_only = read_only, meta_name = meta_name)
}
