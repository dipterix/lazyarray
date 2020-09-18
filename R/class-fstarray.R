#' @noRd
#' @title Internal Class definition for \code{FstArray}
#' @author Zhengjia Wang
#' @description Internal class definition of lazy array objects
FstArray <- R6::R6Class(
  classname = "FstArray",
  portable = TRUE,
  inherit = AbstractLazyArray,
  private = list(
    .compress_level = 50
  ),
  public = list(
    print = function(...){
      cat("<FstArray> (", private$.storage_format, ')\n', sep = '')
      cat('Dimension:\t', paste(sprintf('%d ', private$.dim), collapse = 'x '), '\n')
      cat('File format: fst\n')
      invisible(self)
    },
    
    initialize = function(path, dim, storage_format = c('double', 'integer', 'complex', 'character'),
                          read_only = TRUE, meta_name = 'lazyarray.meta'){
      private$.file_format <- "fst"
      private$.compress_level <- 50
      if(missing(dim)){
        super$initialize(path = path, storage_format = storage_format, 
                         read_only = read_only, meta_name = meta_name)
      } else {
        super$initialize(path = path, dim = dim, storage_format = storage_format, 
                         read_only = read_only, meta_name = meta_name)
      }
      
    },
    has_partition = function(part){
      stopifnot(length(part) == 1)
      file <- self$get_partition_fpath(part, full_path = TRUE)
      cname <- "V1"
      if(self$storage_format == 'complex'){
        cname <- c("V1R", "V1I")
      }
      if(file.exists(file)){
        try({
          return(checkFstMeta(file, expect_nrows = self$partition_length, cnames = cname));
        }, silent = TRUE)
      }
      return(FALSE)
    },
    get_partition_data = function(part, reshape = NULL){
      if(self$has_partition(part)){
        file <- self$get_partition_fpath(part, full_path = TRUE)
        
        if(self$storage_format == 'complex'){
          cname <- c("V1R", "V1I")
          re <- fstRetrieve(file, cname, 1L, NULL)
          re <- re$resTable$V1R + (re$resTable$V1I) * 1i
        } else {
          cname <- "V1"
          re <- fstRetrieve(file, cname, 1L, NULL)
          re <- re$resTable$V1
        }
        if(is.null(reshape)){
          reshapeOrDrop(re, reshape = self$partition_dim(), drop = FALSE)
        } else {
          reshapeOrDrop(re, reshape = as.numeric(reshape), drop = FALSE)
        }
        
        return(re)
      } else {
        array(self$sample_na, self$partition_dim())
      }
    },
    
    `@chunk_map` = function(
      map_function, max_nchunks = 50, ...
    ){
      
      if(!is.function(map_function)){
        stop("map_function must be a function")
      }
      if(length(formals(map_function)) < 2){
        map_f <- function(data, chunk, idx){
          map_function(data)
        }
      } else if(length(formals(map_function)) < 2){
        map_f <- function(data, chunk, idx){
          map_function(data, chunk)
        }
      } else {
        map_f <- map_function
      }
      
      nrows <- self$partition_length
      ncols <- self$npart
      # get chunk size
      chunkf <- make_chunks(nrows, max_nchunks = max_nchunks, ...)
      files <- self$get_partition_fpath()
      partition_locations <- list(
        numeric(0),
        seq_len(ncols)
      )
      
      sdata <- self$sample_na
      
      lapply2(seq_len(chunkf$nchunks), function(ii){
        idx_range <- chunkf$get_indices(ii, as_numeric = TRUE)[[1]]
        chunk_data <- lazyLoadOld(files = files, partition_dim = c(nrows, 1), 
                                  partition_locations = list(seq.int(idx_range[[1]], idx_range[[2]]), 1L), 
                                  ndim = 2L, value_type = sdata)
        map_f(chunk_data, ii, idx_range)
      })
    }
    
    
    
  ),
  active = list(
    
    compress_level = function(v){
      if(!missing(v)){
        stopifnot(v >= 0 & v <= 100)
        private$.compress_level <- v
        self$save_meta()
      }
      private$.compress_level
    },
    
    meta = function(){
      list(
        dim = self$dim,
        dimnames = self$dimnames,
        storage_format = self$storage_format,
        file_format = private$.file_format,
        compress_level = private$.compress_level
      )
    }
    
    
    
  )
)




#' @export
`[.FstArray` <- function(x, ..., drop = TRUE, reshape = NULL){
  if(!x$is_valid){
    stop("`[.FstArray`: x is no longer valid (data has been removed).")
  }
  if(!is.null(reshape)){
    reshape <- as.numeric(reshape)
    stopifnot(all(reshape>=0))
  }
  drop <- isTRUE(drop)
  
  subsetFST(rootPath = x$storage_path,listOrEnv = environment(),
            dim = x$dim,dtype = x$sexptype,reshape = reshape,drop = drop)
}

#' @export
`[<-.FstArray` <- function(x, ..., value){
  if(!x$is_valid){
    stop("`[<-.FstArray`: x is no longer valid (data has been removed).")
  }
  if(!x$can_write){
    stop("`[<-.FstArray`: x is read-only")
  }
  
  parts <- subsetAssignFST(values = value, file = x$storage_path, listOrEnv = environment(),
                  dim = x$dim, dtype = x$sexptype,
                  compression = as.integer(x$compress_level),uniformEncoding = TRUE)
  if(isTRUE(any(parts == -1))){
    x$generate_summary()
  } else {
    parts <- parts[!is.na(parts) && parts > 0 && parts <= x$npart]
    if(length(parts)){
      x$generate_summary(parts)
    }
  }
  # # get 
  # 
  # x$generate_summary()
  invisible(x)
}

