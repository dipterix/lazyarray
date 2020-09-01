#' @rdname lazyarray
#' @export
lazymatrix <- function(
  path, storage_format, dim, dimnames = NULL, 
  multipart = TRUE, prefix = "",
  multipart_mode = 1, compress_level = 50L,
  file_names = list('', seq_len(dim[[length(dim)]]))[[multipart + 1]],
  meta_name = 'lazyarray.meta', 
  read_only = FALSE, quiet = FALSE, ...
){
  if(file.exists(path) && !dir.exists(path)){
    stop('lazymatrix path must be a directory path, but a file was found.')
  }
  
  if(!dir.exists(path)){
    if(length(dim) != 2){
      stop('dim must have length of 2 for matrix')
    }
    # not exists, create a new one
    arr <- create_lazyarray(
      path = path, storage_format = storage_format, dim = dim,
      dimnames = dimnames,  compress_level = compress_level, prefix = prefix,
      multipart = multipart, multipart_mode = multipart_mode, 
      file_names = file_names, meta_name = meta_name)
    return(as.lazymatrix(arr, read_only = read_only))
  }
  
  # path exists, locate meta_name
  if(file.exists(file.path(path, meta_name))){
    path <- normalizePath(path, mustWork = TRUE)
    arr <- ClassLazyMatrix$new(path = path, read_only = read_only, meta_name = meta_name)
    return(arr)
  }
  
  if(!quiet){
    message('meta file not found, create one with existing files')
  }
  
  
  # library(raveio)
  # ts <- lapply(1:20, function(ii){
  #   Tensor$new(
  #     data = 1:9000, c(30,300,1),
  #     dimnames = list(A = 1:30, B = 1:300, C = ii),
  #     varnames = c('A', 'B', 'C'), use_index = 2, temporary = FALSE)
  # })
  # file_names <- sapply(ts, '[[', 'swap_file')
  # path <- stringr::str_split(file_names[[1]], '/file|\\.fst', simplify = TRUE)[,1]
  # file_names <- stringr::str_split(file_names, '/file|\\.fst', simplify = TRUE)[,2]
  # prefix <- 'file'
  # join_tensors(ts, temporary = FALSE)
  
  # Otherwise meta_name does not exist
  if(multipart){
    if(length(file_names) != dim[[length(dim)]]){
      stop('path exists, but cannot find meta file. Please specify file_names\n',
           '  See ', sQuote('?lazyarray'), ' for more details')
    }
    path <- normalizePath(path)
    fs <- file.path(path, sprintf('%s%s.fst', prefix, file_names))
    fe <- file.exists(fs)
    
    if(any(fe)){
      fs <- fs[fe]
      ds <- sapply(fs, function(f){
        # f=ts[[1]]$swap_file
        tryCatch({
          meta <- cpp_fst_meta(normalizePath(f))
          if(inherits(meta, 'fst_error')){ stop(meta) }
          meta
        }, error = function(e){
          stop('Cannot open array file(s). \n  ', f)
        })
        c(meta$nrOfCols, meta$nrOfRows)
      }, USE.NAMES = FALSE)
      
      ds <- unique(t(ds))
      if(length(ds) != 2){
        stop('All existing files must be homogeneous')
      }
      
      mp_dim <- dim[-length(dim)]
      len1 <- prod(mp_dim)
      len2 <- ds[[1]] * ds[[2]]
      
      if(storage_format == 'complex'){
        len1 <- len1 * 2
      }
      
      
      if(len1 != len2){
        stop('Dimension provided does not match with existing files')
      }
      
      if(multipart_mode == 1){
        last_d <- ds[[1]]
        if(storage_format != 'complex'){ last_d <- last_d * 2 }
        if(last_d != 2){
          stop('Multipart mode=1, partition dimension should be ', 
               paste(mp_dim, collapse = 'x'), 
               'x1, but invalid dimension found.')
        }
        part_dimension <- c(mp_dim, 1)
      } else {
        part_dimension <- mp_dim
      }
    } else {
      # no file exists, new data?
      if( multipart_mode == 1 ){
        part_dimension <- dim
        part_dimension[length(dim)] <- 1
      } else if(multipart_mode == 2){
        part_dimension <- dim[-length(dim)]
      }
    }
  } else {
    if(length(file_names) == 0){
      file_names <- ''
    }
    if(length(file_names) != 1){
      stop('path exists, but cannot find meta file. Please specify file_names\n',
           '  See ', sQuote('?lazyarray'), ' for more details')
    }
    path <- normalizePath(path)
    f <- file.path(path, sprintf('%s%s.fst', prefix, file_names))
    
    meta <- tryCatch({
      meta <- cpp_fst_meta(f)
      if(inherits(meta, 'fst_error')){
        stop(meta)
      }
      meta
    }, error = function(e){
      stop('Cannot open array file(s). \n  ', f)
    })
    
    last_dim <- meta$nrOfCols
    prev_dim <- meta$nrOfRows
    
    if(last_dim != dim[length(dim)] || (last_dim * prev_dim != prod(dim))){
      stop(sprintf(
        'Array dimension not match, expected last dimension to be %d and total length %d, but last dim(%d) and length(%d) is given',
        last_dim, prev_dim * last_dim, dim[[length(dim)]], prod(dim)
      ))
    }
    
    part_dimension <- dim
  }
  
  if(length(dim) != 2){
    stop('dim must have length of 2 for matrix')
  }
  
  # make a meta file
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
  
  meta_path <- file.path(path, meta_name)
  save_yaml(meta, meta_path)
  
  ClassLazyMatrix$new(path = path, read_only = read_only, meta_name = meta_name)
  
}


#' @export
as.lazymatrix <- function(x, read_only = NA, ...){
  UseMethod('as.lazymatrix')
}

#' @export
as.lazymatrix.LazyArray <- function(x, read_only = NA, ...){
  path <- dirname(x$storage_path)
  meta_name <- x$meta_name
  if(is.na(read_only)){
    read_only <- !x$can_write
  }
  ClassLazyMatrix$new(path = path, read_only = read_only, meta_name = meta_name)
}
