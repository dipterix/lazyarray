# create arrays on disk

#' @title Create a lazy-array with given format and dimension
#' @author Zhengjia Wang
#' @description Create a directory to store lazy-array. The path must be missing. See \code{\link{load_lazyarray}} for more details
#' @param path path to a local drive to store array data
#' @param storage_format data type, choices are \code{"double"}, 
#' \code{"integer"}, \code{"character"}, and \code{"complex"}
#' @param dim integer vector, dimension of array, see \code{\link{dim}}
#' @param dimnames list of vectors, names of each dimension, see \code{\link{dimnames}}
#' @param multipart whether to split array into multiple partitions, default is true
#' @param prefix character prefix of array partition
#' @param multipart_mode 1, or 2, mode of partition, see details.
#' @param file_names data file names without prefix/extensions; see details.
#' @param compress_level 0 to 100, level of compression. 0 means
#' no compression, 100 means maximum compression. For persistent data,
#' it's recommended to set 100. Default is 50.
#' @param meta_name header file name, default is \code{"lazyarray.meta"}
#' @return A \code{ClassLazyArray} instance
#' @details Lazy array stores array into hard drive, and load them on
#' demand. It differs from other packages such as \code{"bigmemory"}
#' that the internal reading uses multi-thread, which gains significant 
#' speed boost on solid state drives. 
#' 
#' One lazy array contains two parts: data file(s) and a meta file.
#' The data files can be stored in two ways: non-partitioned and 
#' partitioned. 
#' 
#' For non-partitioned data array, the dimension is 
#' set at the creation of the array and cannot be mutable once created
#' 
#' For partitioned data array, there are also two partition modes, 
#' defined by \code{`multipart_mode`}. For mode 1, each partition 
#' has the same dimension size as the array. The last dimension is \code{1}.
#' For example, a data with dimension \code{c(2,3,5)} 
#' partitioned with mode 1 will have each partition dimension stored
#' with \code{c(2,3,1)}. For mode 2, the last dimension will be dropped
#' when storing each partitions.
#' 
#' \code{file_names} is used when irregular partition names should be used.
#' If \code{multipart=FALSE}, the whole array is stored in a single file under
#' \code{path}. The file name is \code{<prefix><file_name>.fst}. For example,
#' by default \code{prefix=""}, and \code{file_name=""}, then \code{path/.fst}
#' stores the array data. If \code{multipart=TRUE}, then \code{file_names}
#' should be a character vector of length equal to array's last dimension. A
#' \code{3x4x5} array has 5 partitions, each partition name follows 
#' \code{<prefix><file_name>.fst} convention, and one can always use
#' \code{arr$get_partition_fpath()} to find location of partition files.
#' For examples, see \code{\link{lazyarray}}.
#' 
#' @export
create_lazyarray <- function(
  path, storage_format, dim, dimnames = NULL, compress_level = 50L, prefix = "",
  multipart = TRUE, multipart_mode = 1, file_names = NULL,
  meta_name = 'lazyarray.meta'){
  
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
  
  if( multipart ){
    if(length(file_names) == 0){
      file_names <- seq_len(dim[[length(dim)]])
    } else if(length(file_names) != dim[[length(dim)]]){
      stop(sprintf(
        'file_names must be either NULL or length of %d when multipart=TRUE',
        dim[[length(dim)]]
      ))
    }
  } else {
    if(!length(file_names)){
      file_names <- ''
    } else {
      if(length(file_names) > 1 || !isTRUE(is.character(file_names))){
        stop('file_names must be either NULL or character(1) when multipart=FALSE')
      }
    }
  }
  
  if(any(duplicated(file_names))){
    stop('file_names has duplicated values. Please rename them.')
  }
  
  # check if dim matches with dimnames
  if(!is.null(dimnames)){
    dnl <- sapply(dimnames, length)
    if(length(dnl) != length(dim) || !all(dnl - dim == 0)){
      stop("Invalid dimension")
    }
  }
  
  multipart <- as.logical(multipart)
  if(multipart && multipart_mode == 2 && length(dim) == 2){
    warning("multipart_mode must be 1 for matrix")
  }
  
  stopifnot(storage_format %in% c('character', 'double', 'int', 'complex'))
  
  if( multipart_mode == 1 ){
    part_dimension <- dim
    part_dimension[length(dim)] <- 1
  } else if(multipart_mode == 2){
    part_dimension <- dim[-length(dim)]
  }
  
  #####
  meta <- list(
    lazyarray_version = 0,
    file_format = 'fst',
    storage_format = storage_format,
    dim = dim,
    dimnames = dimnames,
    partitioned = multipart,
    prefix = prefix,
    part_dimension = part_dimension,
    postfix = '.fst',
    compress_level = compress_level,
    file_names = file_names
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
#' create_lazyarray(path, 'double', dim = c(3,4,5), multipart = TRUE)
#' 
#' x <- load_lazyarray(path, read_only = FALSE)
#' x[2,3:4, 2:1] <- 1:4
#' x[ , , 2]
#' 
#' # Expend dimension for multiple partition data only
#' dim(x) <- c(3,4,6)
#' dimnames(x) <- list(dim1 = as.character(1:3),
#'                     dim2 = letters[1:4], 
#'                     dim3 = LETTERS[1:6])
#' x[ , , 'B', drop = FALSE]
#' 
#' # Non-standard subset methods
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
#' x <- create_lazyarray(path, 'complex', dim = c(100,200,300,20), 
#'                       multipart = TRUE, multipart_mode = 1)
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
