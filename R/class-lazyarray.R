LazyArray <- R6::R6Class(
  classname = "LazyArray",
  portable = TRUE,
  private = list(
    .valid = TRUE,
    .dir = character(0),
    .path = character(0),
    .dim = integer(0),
    .dimnames = NULL,
    lazyarray_version = 0,
    file_format = 'fst',
    storage_format = character(0),
    partitioned = FALSE,
    prefix = "",
    postfix = '.fst',
    read_only = TRUE,
    compress_level = 50,
    auto_clean = FALSE,
    part_dimension = integer(0),  # dimension in each file. dim[-length(dim)] or c(dim[-length(dim)], 1)
    sample_data = function(){
      switch(
        private$storage_format,
        "double" = 0.1,
        "integer" = 0L,
        "complex" = 0i,
        "character" = "",
        {
          stop("Invalid internal storage format")
        }
      )
    },
    save_meta = function(){
      meta = list(
        lazyarray_version = private$lazyarray_version,
        file_format = private$file_format,
        storage_format = private$storage_format,
        dim = private$.dim,
        dimnames = private$.dimnames,
        partitioned = private$partitioned,
        prefix = private$prefix,
        part_dimension = private$part_dimension,
        postfix = private$postfix,
        compress_level = private$compress_level
      )
      save_yaml(meta, private$.path)
    }
  ),
  public = list(
    print = function(...){
      cat("<LazyArray> (", private$storage_format, ')\n', sep = '')
      cat('Dimension:\t', paste(sprintf('%d ', private$.dim), collapse = 'x '), '\n')
      cat('Partitioned:\t', private$partitioned , '\n')
      cat('File format:\t', sprintf('%s[part]%s', private$prefix, private$postfix), '\n')
      invisible(self)
    },
    initialize = function(path, read_only = TRUE){
      private$read_only = read_only
      stopifnot(dir.exists(path))
      private$.dir = normalizePath(path, mustWork = TRUE)
      private$.path = normalizePath(file.path(path, self$meta_name), mustWork = TRUE)
      
      # load yaml
      meta = load_yaml(private$.path)
      
      # check format version
      if(is.null(meta$lazyarray_version)){
        if(!isTRUE(meta$lazyarray_version >= self$min_version)){
          stop("Minimal version ", self$min_version, " required, but the file version is ", private$.meta$lazyarray_version)
        }
        private$lazyarray_version = meta$lazyarray_version
      } else {
        private$lazyarray_version = self$min_version
      }
      
      # check format: fst? hdf5?
      if(length(meta$file_format) != 1){
        stop("File format not found.")
      }
      private$file_format = meta$file_format
      
      # check data format
      if(length(meta$storage_format) != 1 ||
         ! meta$storage_format %in% c('double', 'integer', 'character', 'complex') ){
        stop(sprintf("Cannot find valid storage format. Supported data formats are %s, %s, %s, and %s",
             sQuote('double'), sQuote('integer'), sQuote('character'), sQuote('complex')))
      }
      private$storage_format = meta$storage_format
      
      # get dimension information
      if(length(meta$dim) < 2){
        stop("Invalid dimension information.")
      }
      private$.dim = meta$dim
      
      # get dimension names
      if(!is.null(meta$dimnames) && length(meta$dimnames) != length(meta$dim)){
        stop("Dimension name does not match.")
      }
      private$.dimnames = meta$dimnames
      
      # check whether the file is partitioned
      if(isTRUE(meta$partitioned)){
        private$partitioned = TRUE
        private$part_dimension = meta$part_dimension
      }
      
      if(length(meta$prefix) != 1){
        stop('Invalid prefix')
      }
      private$prefix = meta$prefix
      
      if(!length(meta$postfix) || !is.character(meta$postfix)){
        stop("Cannot find file postfix")
      }
      private$postfix = meta$postfix
      
      if(!isTRUE(is.numeric(meta$compress_level))){
        meta$compress_level = 50
      }
      private$compress_level = meta$compress_level
      
    },
    
    flag_auto_clean = function(auto){
      private$auto_clean = auto
    },
    
    finalize = function(){
      if(private$auto_clean){
        self$remove_data(warn = FALSE)
      }
    },
    
    remove_data = function(force = FALSE, warn = TRUE){
      if(dir.exists(private$.dir)){
        if(force || file.exists(private$.path)){
          unlink(private$.dir, recursive = TRUE)
          private$.valid = FALSE
        }
      } else {
        private$.valid = FALSE
      }
      if(warn && dir.exists(private$.dir)){
        warning("LazyArray not fully cleaned at: ", private$.dir)
      }
      return(!private$.valid)
    },
    
    make_writable = function(){
      private$read_only = FALSE
    },
    
    make_readonly = function(){
      private$read_only = TRUE
    },
    
    set_dim = function(dim, dimnames){
      stopifnot(!private$read_only && private$.valid)
      if(length(dim) != length(private$.dim)){
        stop('Cannot set dimension from ', deparse1(private$.dim), ' to ', 
             deparse1(private$.dim), '. Different # of dimensions.')
      }
      dif = private$.dim - dim
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
      mis_dimnames = missing(dimnames)
      if(mis_dimnames){
        dimnames = private$.dimnames
      }
      if(!mis_dimnames && length(dimnames)){
        if(!is.list(dimnames)){
          stop('dimnames must be a list')
        }
        dnl <- sapply(dimnames, length)
        if(length(dimnames) != length(dim) || !all(dnl - dim == 0)){
          stop("dim does not matches with dimnames")
        }
      }
      private$.dim = dim
      for(ii in seq_along(dimnames)){
        dimnames[[ii]] <- dimnames[[ii]][seq_len(dim[[ii]])]
      }
      private$.dimnames = dimnames
      private$save_meta()
    },
    
    get_file_format = function(){
      private$file_format
    },
    
    get_storage_format = function(){
      private$storage_format
    },
    
    is_multi_part = function(){
      private$partitioned
    },
    
    partition_dim = function(){
      if(private$partitioned){
        private$part_dimension
      }else{
        private$.dim
      }
    },
    
    get_partition_fpath = function(part, full_path = TRUE){
      if(private$partitioned){
        res <- sprintf('%s%d%s', private$prefix, part, private$postfix)
      } else {
        # ignore part
        res <- sprintf('%s%s', private$prefix, private$postfix)
      }
      if(full_path){
        res <- file.path(private$.dir, res)
      }
      res
    },
    
    `@set_data` = function(value, ...){
      stopifnot(!private$read_only && private$.valid)
      
      idx = list(...)
      ndim = self$ndim
      stopifnot(length(idx) == ndim)
      idx = lapply(idx, function(ii){
        # ii = ii[!is.na(ii)]
        as.integer(ii)
      })
      
      val_dim = sapply(idx, length)
      
      if(any(val_dim <= 0)){
        # no change
        return(invisible(self))
      }
      
      if(length(value) == 1){
        value = array(value, dim = val_dim)
      } else{
        stopifnot(length(value) == prod(val_dim))
      }
      
      x <- NULL
      if(is.null(dim(value)) || length(dim(value)) != length(val_dim) || !all(dim(value) == val_dim) ){
        dim(value) = val_dim
      }
      
      if(private$partitioned){
        part_dimension <- private$part_dimension
        partition_locations <-  lapply(part_dimension, seq_len)
        
        part_idx = idx
        part_idx[[ndim]] = 1
        args <- c(list(x = quote(x)), part_idx, list(value = quote(value)))
        
        
        # We save for each file
        ii <- 1
        last_idx <- idx[[ndim]]
        
        partial_len <- prod(val_dim[-ndim])
        lapply(seq_len(val_dim[[ndim]]), function(ii){
          value <- value[partial_len * (ii-1) + seq_len(partial_len)]
        # })
        # apply(value, ndim, function(value){
          # get partition file
          part <- last_idx[[ii]]
          if(part > self$dim[ndim]){
            return(FALSE)
          }
          fname = self$get_partition_fpath(part, full_path = TRUE)
          x <- cpp_load_lazyarray(fname, partition_locations, part_dimension, self$ndim, private$sample_data())
          # x <- do.call('[<-', args)
          # make a call instead of do.call
          call <- as.call(c(list(quote(`[<-`)), args))
          x <- eval(call)
          
          cpp_create_lazyarray(x, part_dimension, fname, compression = private$compress_level, uniformEncoding = TRUE)
          ii <<- ii+1
          return(TRUE)
        })
        
      } else {
        fname <- self$get_partition_fpath(full_path = TRUE)
        partition_locations <- lapply(self$dim, seq_len)
        args <- c(list(x = quote(x)), idx, list(value = quote(value)))
        x <- cpp_load_lazyarray(fname, partition_locations, self$dim, self$ndim, private$sample_data())
        x <- do.call('[<-', args)
        cpp_create_lazyarray(x, self$dim, fname, compression = private$compress_level, uniformEncoding = TRUE)
      }
      invisible(self)
    },
    
    set_compress_level = function(level){
      stopifnot(level >= 0 & level <= 100)
      private$compress_level = level
      private$save_meta()
    },
    
    get_compress_level = function(){
      private$compress_level
    },
    
    
    `@get_data` = function(..., drop = TRUE){
      stopifnot(private$.valid)
      idx = list(...)
      if(!length(idx)){
        # case 1 x[], get all the data
        idx <- lapply(self$dim, seq_len)
      } else if(length(idx) == 1){
        # case 1 x[a:b], get data by length, we have to calculate indices
        stop("subset with length is not supported right now")
      } else {
        stopifnot(length(idx) == self$ndim)
        idx = lapply(idx, function(id){
          id = as.integer(id)
          id[is.na(id)] = -1L
          id
        })
      }
      
      # check which files need to be loaded
      files <- self$get_partition_fpath(idx[[self$ndim]], full_path = TRUE)
      
      if(private$partitioned){
        if(length(self$partition_dim()) == self$ndim){
          idx[[self$ndim]] <- 1
        }else{
          idx[[self$ndim]] <- NULL
        }
      }
      re <- cpp_load_lazyarray(files, partition_locations = idx, 
                         partition_dim = self$partition_dim(), 
                         ndim = self$ndim, value_type = private$sample_data())
      if(drop){
        re <- drop(re)
      }
      re
    },
    
    
    `@sample_data` = function(){
      private$sample_data()
    }
    
  ),
  active = list(
    meta_name = function(){
      'lazyarray.meta'
    },
    min_version = function(){
      0
    },
    version = function(){
      private$lazyarray_version
    },
    dim = function(){
      private$.dim
    },
    dimnames = function(){
      private$.dimnames
    },
    ndim = function(){ length(private$.dim) },
    can_write = function(){
      !private$read_only
    }
  )
)


