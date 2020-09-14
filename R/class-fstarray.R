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
  
  subsetAssignFST(values = value, file = x$storage_path, listOrEnv = environment(),
                  dim = x$dim, dtype = x$sexptype,
                  compression = as.integer(x$compress_level),uniformEncoding = TRUE)
  
  # # get 
  # 
  # x$generate_summary()
  invisible(x)
}

