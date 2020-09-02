#' @noRd
#' @title Internal Class definition for \code{LazyMatrix}
#' @author Zhengjia Wang
#' @description Internal class definition of lazy matrix objects
ClassLazyMatrix <- R6::R6Class(
  classname = "LazyMatrix",
  portable = TRUE,
  inherit = ClassLazyArray,
  private = list(
    
  ),
  public = list(
    #' @field @transposed whether the matrix is transposed
    `@transposed` = FALSE,
    
    #' @description Transpose matrix
    transpose = function(){
      new <- self$clone(deep = FALSE)
      new$`@transposed` = !new$`@transposed`
      return( new )
    },
    
    #' @description Override print method
    #' @param ... ignored
    #' @return self instance
    print = function(...){
      cat("<LazyMatrix> (", private$storage_format, ')\n', sep = '')
      cat('Dimension:\t', paste(sprintf('%d ', self$dim), collapse = 'x '), 
          ifelse(self$`@transposed`, ' (transposed)', ''), '\n')
      cat('Partitioned:\t', private$partitioned , '\n')
      cat('File format:\t', sprintf('%s[part]%s', private$prefix, private$postfix), '\n')
      invisible(self)
    },
    
    #' @description Constructor
    #' @param path directory to store data into
    #' @param read_only whether modification is allowed
    #' @param meta_name meta file to store the data into
    initialize = function(path, read_only = TRUE, meta_name = 'lazyarray.meta'){
      super$initialize(path, read_only = read_only, meta_name = meta_name)
      
      # get dimension information
      if(length(private$.dim) != 2){
        d <- private$.dim
        if(length(d) == 1){
          private$.dim <- c(d, 1)
        } else {
          private$.dim <- c(prod(d[-length(x)]), d[[length(d)]])
        }
      }
      
    },
    
    #' @description Set \code{\link[base]{dim}} and \code{\link[base]{dimnames}} of the matrix
    #' @param dim integer vector of the matrix dimension; see \code{\link[base]{dim}}
    #' @param dimnames named list of dimension names; see \code{\link[base]{dimnames}}
    set_dim = function(dim, dimnames){
      stopifnot(!private$read_only && private$.valid)
      if(length(dim) != 2){
        stop('dim must be length of 2')
      }
      if(self$`@transposed`){ dim <- rev(dim) }
      if(length(dim) != length(private$.dim)){
        stop('Cannot set dimension from ', deparse1(private$.dim), ' to ', 
             deparse1(private$.dim), '. Different # of dimensions.')
      }
      dif <- private$.dim - dim
      if(prod(dim) == 0){
        stop("zero length array is not supported.")
      }
      if(private$partitioned){
        # dim is allowed to vary along the last dimension, like c(1,2,4,3) -> c(1,2,4,5)
        if(!all(dif[-length(dif)] == 0)){
          stop('For multi-part arrays, you can only increase/decrease the last dimension, like from c(12,3,1) to c(12,3,X)')
        } 
      } else if(!all(dif == 0)){
        stop("For single data file arrays, you cannot change dimension once initialized")
      }
      # check dimension vs dim
      mis_dimnames <- missing(dimnames)
      if(mis_dimnames){
        dimnames <- private$.dimnames
      }
      if(!mis_dimnames && length(dimnames)){
        if(!is.list(dimnames)){
          stop('dimnames must be a list')
        }
        if(self$`@transposed`){
          dimnames <- rev(dimnames)
        }
        dnl <- sapply(dimnames, length)
        if(length(dimnames) != length(dim) || !all(dnl - dim == 0)){
          stop("dim does not matches with dimnames")
        }
      }
      private$.dim <- dim
      for(ii in seq_along(dimnames)){
        dimnames[[ii]] <- dimnames[[ii]][seq_len(dim[[ii]])]
      }
      private$.dimnames <- dimnames
      private$save_meta()
    },
    
    
    
    #' @description Internal method to set data
    #' @param value vector of data to be set
    #' @param i,j index set
    `@set_data` = function(value, i = NULL, j = NULL){
      stopifnot(!private$read_only && private$.valid)
      
      if(self$`@transposed`){
        
        val_dim <- c(length(j), length(i))
        
        if(any(val_dim <= 0)){
          # no change
          return(invisible(self))
        }
        
        if(length(value) == 1){
          value <- array(value, dim = val_dim)
        } else{
          stopifnot(length(value) == prod(val_dim))
          dim(value) <- rev(val_dim)
          value <- t(value)
        }
        
        super$`@set_data`(value, j, i)
      } else {
        super$`@set_data`(value, i, j)
      }
    },
    
    #' @description Internal method to read data
    #' @param i,j index set
    #' @param drop whether to drop dimension after subset, default is true
    `@get_data` = function(i, j, drop = TRUE){
      if(missing(j)){
        j <- seq_len(self$dim[[2]])
      }
      if(missing(i)){
        i <- seq_len(self$dim[[1]])
      }
      
      if(self$`@transposed`){
        re <- t(super$`@get_data`(j, i, drop = FALSE))
        if(drop){
          re <- drop(re)
        }
      } else {
        re <- super$`@get_data`(i, j, drop = drop)
      }
      return(re)
    },
    
    
    #' @description Matrix multiplication
    #' @param x another \code{LazyMatrix} or normal matrix
    #' @param chunk_size chunk size to use
    #' @param ... ignored
    multiply_mat = function(x, chunk_size = 'auto', ...){
      d1 <- self$dim
      d2 <- dim(x)
      stopifnot(d1[[2]] == d2[[1]] && length(d2) == 2)
      nrows <- d1[[2]]
      d3 <- c(d1[[1]], d2[[2]])
      dn1 <- self$dimnames[[1]]
      dn2 <- dimnames(x)[[2]]
      if(is.null(dn1) || is.null(dn2)){
        dimnames <- NULL
      } else {
        dimnames <- structure(list(dn1, dn2), names = c(
          names(self$dimnames)[[1]], names(dimnames(x))[[2]]
        ))
      }
      
      
      if(isTRUE(chunk_size == 'auto')){
        chunk_size <- 1024
      }
      
      chunk_size <- min(chunk_size, nrows)
      
      nloops <- floor(nrows / chunk_size)
      if(nloops * chunk_size < nrows){
        nloops <- nloops + 1
      }
      if(nloops > 200){
        # force nloops to be 200
        nloops <- 200
        chunk_size <- floor(nrows / nloops)
        if(nloops * chunk_size < nrows){
          chunk_size <- chunk_size + 1
        }
      }
      
      path <- tempfile(pattern = 'lazymatrix_tmp_')
      if(dir.exists(path)){
        path <- tempfile(pattern = 'lazymatrix_tmp1_')
      }
      re <- create_lazyarray(
        path, storage_format = 'double', dim = c(d3, nloops)
      )
      
      lapply2(seq_len(nloops), function(ii){
        re$make_writable()
        idx1 <- (ii-1) * chunk_size + 1
        idx2 <- min((ii) * chunk_size, nrows)
        if(idx1 > idx2){
          re[,,ii] <- 0
        }
        idx <- seq.int(idx1, idx2)
        re[,,ii] <- self[, idx, drop = FALSE] %*% x[idx, , drop = FALSE]
        NULL
      })
      
      if(has_dipsaus()){
        re <- dipsaus::collapse(re[], keep = c(1,2))
      } else {
        re <- apply(re[], 2, rowSums)
      }
      re
    }
    
  ),
  active = list(
    #' @field dim dimension of the matrix
    dim = function(){
      v <- super$dim
      if(self$`@transposed`){
        v <- rev(v)
      }
      v
    },
    
    #' @field dimnames dimension names of the matrix
    dimnames = function(){
      v <- super$dimnames
      if(self$`@transposed`){
        v <- rev(v)
      }
      v
    },
    
    #' @field rownames row names of the matrix
    rownames = function(){
      d <- self$dimnames
      if(!is.null(d)){
        d <- d[[1]]
      }
      d
    },
    
    #' @field colnames column names of the matrix
    colnames = function(){
      d <- self$dimnames
      if(!is.null(d)){
        d <- d[[2]]
      }
      d
    }
  )
)


