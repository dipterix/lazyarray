#' @noRd
#' @title Internal Class definition for \code{LazyArray}
#' @author Zhengjia Wang
#' @description Internal class definition of lazy array objects
ClassLazyArray <- R6::R6Class(
  classname = "LazyArray",
  portable = TRUE,
  private = list(
    .valid = TRUE,
    .dir = character(0),
    .path = character(0),
    .dim = integer(0),
    .dimnames = NULL,
    .meta_name = 'lazyarray.meta',
    lazyarray_version = 0,
    file_format = 'fst',
    storage_format = character(0),
    partitioned = FALSE,
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
      meta <- list(
        lazyarray_version = private$lazyarray_version,
        file_format = private$file_format,
        storage_format = private$storage_format,
        dim = private$.dim,
        dimnames = private$.dimnames,
        partitioned = private$partitioned,
        part_dimension = private$part_dimension,
        postfix = private$postfix,
        compress_level = private$compress_level
      )
      save_yaml(meta, private$.path)
    }
  ),
  public = list(
    #' @description Override print method
    #' @param ... ignored
    #' @return self instance
    print = function(...){
      cat("<LazyArray> (", private$storage_format, ')\n', sep = '')
      cat('Dimension:\t', paste(sprintf('%d ', private$.dim), collapse = 'x '), '\n')
      cat('Partitioned:\t', private$partitioned , '\n')
      cat('File format:\t', sprintf('[part]%s', private$postfix), '\n')
      invisible(self)
    },
    
    #' @description Constructor
    #' @param path directory to store data into
    #' @param read_only whether modification is allowed
    #' @param meta_name meta file to store the data into
    initialize = function(path, read_only = TRUE, meta_name = 'lazyarray.meta'){
      private$.meta_name <- meta_name
      private$read_only <- read_only
      stopifnot(dir.exists(path))
      private$.dir <- normalizePath(path, mustWork = TRUE)
      private$.path <- normalizePath(file.path(path, self$meta_name), mustWork = TRUE)
      
      # load yaml
      meta <- load_yaml(private$.path)
      
      # check format version
      if(is.null(meta$lazyarray_version)){
        if(!isTRUE(meta$lazyarray_version >= self$min_version)){
          stop("Minimal version ", self$min_version, " required, but the file version is ", private$.meta$lazyarray_version)
        }
        private$lazyarray_version <- meta$lazyarray_version
      } else {
        private$lazyarray_version <- self$min_version
      }
      
      # check format: fst? hdf5?
      if(length(meta$file_format) != 1){
        stop("File format not found.")
      }
      private$file_format <- meta$file_format
      
      # check data format
      if(length(meta$storage_format) != 1 ||
         ! meta$storage_format %in% c('double', 'integer', 'character', 'complex') ){
        stop(sprintf("Cannot find valid storage format. Supported data formats are %s, %s, %s, and %s",
             sQuote('double'), sQuote('integer'), sQuote('character'), sQuote('complex')))
      }
      private$storage_format <- meta$storage_format
      
      # get dimension information
      if(length(meta$dim) < 2){
        stop("Invalid dimension information.")
      }
      private$.dim <- meta$dim
      
      # get dimension names
      if(!is.null(meta$dimnames) && length(meta$dimnames) != length(meta$dim)){
        stop("Dimension name does not match.")
      }
      private$.dimnames <- meta$dimnames
      
      # check whether the file is partitioned
      private$partitioned <- TRUE
      private$part_dimension <- meta$part_dimension
      
      n_part <- meta$dim[[length(meta$dim)]]
      
      if(!length(meta$postfix) || !is.character(meta$postfix)){
        stop("Cannot find file postfix")
      }
      private$postfix <- meta$postfix
      
      if(!isTRUE(is.numeric(meta$compress_level))){
        meta$compress_level <- 50
      }
      private$compress_level <- meta$compress_level
      
    },
    
    #' @description Set auto clean flag
    #' @param auto logical whether the data on hard disk will be automatically cleaned
    flag_auto_clean = function(auto){
      private$auto_clean <- auto
    },
    
    #' @description Override finalize method
    finalize = function(){
      if(private$auto_clean){
        self$remove_data(warn = FALSE)
      }
    },
    
    #' @description Remove data on hard disk
    #' @param force whether to force remove the data
    #' @param warn whether to show warning if not fully cleaned
    remove_data = function(force = FALSE, warn = TRUE){
      if(!private$.valid){ return(FALSE) }
      if(dir.exists(private$.dir)){
        if(force || file.exists(private$.path)){
          
          # list all files within .dir
          fs <-
            c(private$.meta_name,
              self$get_partition_fpath(full_path = FALSE),
              self$get_partition_fpath(full_path = FALSE, summary_file = TRUE))
          all_fs <- list.files(private$.dir, all.files = TRUE, 
                               recursive = FALSE, full.names = FALSE, 
                               include.dirs = TRUE)
          
          rest <- all_fs[!all_fs %in% c(fs, '.', '..')]
          sel_metas <- grepl('\\.meta$', rest)
          
          if(!length(rest) || all(sel_metas)){
            # cannot remove all files because some other files exist, 
            # not created by me
            unlink(private$.dir, recursive = TRUE)
          } else {
            lapply(fs, function(f){
              f <- file.path(private$.dir, f)
              if(file.exists(f)){
                unlink(f)
              }
            })
          }
          private$.valid <- FALSE
        }
      } else {
        private$.valid <- FALSE
      }
      if(warn && dir.exists(private$.dir)){
        warning("LazyArray not fully cleaned at: ", private$.dir, 
                '. Some files noted created by this array were detected. ',
                'Please manually remove them if they are no longer used.')
      }
      return(!private$.valid)
    },
    
    #' @description Make instance writable
    make_writable = function(){
      private$read_only <- FALSE
    },
    
    #' @description Make instance read-only
    make_readonly = function(){
      private$read_only <- TRUE
    },
    
    #' @description Set \code{\link[base]{dim}} and \code{\link[base]{dimnames}} of the array
    #' @param dim integer vector of the array dimension; see \code{\link[base]{dim}}
    #' @param dimnames named list of dimension names; see \code{\link[base]{dimnames}}
    set_dim = function(dim, dimnames){
      stopifnot(!private$read_only && private$.valid)
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
    
    #' @description Partition format, currently only \code{'fst'} is supported
    get_file_format = function(){
      private$file_format
    },
    
    #' @description Data storage format, expected to be one of the 
    #' followings: 'double', 'integer', 'character', or 'complex'
    get_storage_format = function(){
      private$storage_format
    },
    
    #' @description Whether partitioned based on the last dimension
    is_multi_part = function(){
      private$partitioned
    },
    
    #' @description Returns dimension of each partition
    partition_dim = function(){
      if(private$partitioned){
        private$part_dimension
      }else{
        private$.dim
      }
    },
    
    #' @description Internally used to calculate min, max, range, and sum 
    #' squares, can only be used when array is numeric
    #' @param partitions integer, partition numbers
    #' then \code{map_function} must be specified
    #' @param map_function mapping function, receive numeric vectors and returns
    #' a numeric vector
    #' @param split whether to further partition the data
    #' @param ... passed as internal options
    `@partition_map` = function(
      partitions, map_function = NULL, split = TRUE, ...
    ){
      if(!self$is_multi_part()){
        stop("Cannot call @partition_reduce if array is not multi-part")
      }
      
      sformat <- self$get_storage_format()
      
      is_complex = sformat == 'complex'
      
      stopifnot(is.function(map_function))
      
      more_args <- list(...)
      pd <- self$partition_dim()
      
      # self = arr; p = 1
      paths <- self$get_partition_fpath(partitions)
      
      res <- lapply2(seq_along(partitions), function(ii){
        part <- partitions[[ii]]
        
        path <- paths[[ii]]
        
        map_f2 <- function(x){
          map_function(x, part)
        }
        
        if(!file.exists(path)){
          re <- map_f2(array(NA, dim = pd))
          if(!is.null(re)){
            attr(re, 'chunk_length') <- prod(pd)
          }
          
          return(list(re))
        }
        
        meta <- fstMeta(path)
        
        if(inherits(meta, 'fst_error')){
          stop(meta)
        }
        
        # decide column names
        if(is_complex){
          ncols = floor(meta$nrOfCols / 2)
          colnms <- sapply(c('R', 'I'), function(s){
            sprintf('V%d%s', seq_len(ncols), s)
          })
          colnms <- matrix(colnms, ncol = 2)
        } else {
          ncols = meta$nrOfCols
          colnms <- as.matrix(sprintf('V%d', seq_len(ncols)))
        }
          
        if( split ){
          more_args[['dim']] <- c(meta$nrOfRows, ncols)
          more_args[['recursive']] <- !isFALSE(more_args[['recursive']])
          chunks <- do.call(make_chunks, more_args)
          
          res <- lapply(seq_len(chunks$nchunks), function(i){
            idx <- chunks$get_indices(i, as_numeric = TRUE)
            idx[[1]] <- idx[[1]]
            lapply(seq.int(idx[[2]][[1]], idx[[2]][[2]]), function(col){
              lazyMapReduceByPartition(path, colnms[col, ], idx[[1]][[1]], idx[[1]][[2]], map_f2)
            })
          })
          res <- unlist(res, recursive = FALSE)
        } else {
          res <- lapply(seq_len(ncols), function(ii){
            lazyMapReduceByPartition(path, colnms[ii,], 1L, NULL, map_f2, reshape = pd)
          })
        }
        
        res
      })
      
      res <- unlist(res, recursive = FALSE)
      
      res
      
    },
    
    
    `@chunk_map` = function(
      map_function, max_nchunks = 50, ...
    ){
      if(!self$is_multi_part()){
        stop("Cannot call @chunk_map if array is not multi-part")
      }
      
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
      
      nrows <- prod(self$partition_dim())
      ncols <- self$npart
      # get chunk size
      chunkf <- make_chunks(nrows, max_nchunks = max_nchunks, ...)
      files <- self$get_partition_fpath()
      partition_locations <- list(
        numeric(0),
        seq_len(ncols)
      )
      
      sdata <- self$`@sample_data`()
      
      lapply2(seq_len(chunkf$nchunks), function(ii){
        idx_range <- chunkf$get_indices(ii, as_numeric = TRUE)[[1]]
        chunk_data <- lazyLoadOld(files = files, partition_dim = c(nrows, 1), 
                           partition_locations = list(seq.int(idx_range[[1]], idx_range[[2]]), 1L), 
                           ndim = 2L, value_type = sdata)
        map_f(chunk_data, ii, idx_range)
      })
    },
    
    #' @description Get partition path
    #' @param part integer representing the partition
    #' @param full_path whether return the full system path
    #' @param summary_file whether to return summary file
    #' @return Character file name or full path
    get_partition_fpath = function(part, full_path = TRUE, summary_file = FALSE){
      if(missing(part)){
        part <- seq_len(self$npart)
      } else {
        part <- as.integer(part)
        if(base::anyNA(part) || any(part <= 0)){
          stop("partition number must be all positive")
        }
      }
      res <- sprintf('%s%s', part, private$postfix)
      if(full_path){
        res <- file.path(private$.dir, res)
      }
      if(summary_file){
        res <- sprintf('%s.summary', res)
      }
      res
    },
    
    #' @description Internal method to set data
    #' @param value vector of data to be set
    #' @param ... index set
    `@set_data` = function(value, ...){
      stopifnot(!private$read_only && private$.valid)
      
      idx <- list(...)
      ndim <- self$ndim
      stopifnot(length(idx) == ndim)
      idx <- lapply(idx, function(ii){
        # ii = ii[!is.na(ii)]
        as.integer(ii)
      })
      
      val_dim <- sapply(idx, length)
      
      if(any(val_dim <= 0)){
        # no change
        return(invisible(self))
      }
      
      if(length(value) == 1){
        value <- array(value, dim = val_dim)
      } else{
        stopifnot(length(value) == prod(val_dim))
      }
      
      x <- NULL
      if(is.null(dim(value)) || length(dim(value)) != length(val_dim) || !all(dim(value) == val_dim) ){
        dim(value) <- val_dim
      }
      
      if(private$partitioned){
        part_dimension <- private$part_dimension
        partition_locations <-  lapply(part_dimension, seq_len)
        
        part_idx <- idx
        part_idx[[ndim]] <- 1
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
          fname <- self$get_partition_fpath(part, full_path = TRUE)
          
          if(prod(vapply(part_idx, length, 1L)) == prod(self$partition_dim())){
            x <- array(value, dim = self$partition_dim())
          } else {
            x <- lazyLoadOld(fname, partition_locations, part_dimension, self$ndim, private$sample_data())
            # x <- do.call('[<-', args)
            # make a call instead of do.call
            call <- as.call(c(list(quote(`[<-`)), args))
            x <- eval(call)
          }
          
          cpp_create_lazyarray(x, part_dimension, fname, compression = private$compress_level, uniformEncoding = TRUE)
          
          self$`@generate_parition_summary`(part, x = x)
          
          ii <<- ii+1
          return(TRUE)
        })
        
      } else {
        fname <- self$get_partition_fpath(full_path = TRUE)
        partition_locations <- lapply(self$dim, seq_len)
        args <- c(list(x = quote(x)), idx, list(value = quote(value)))
        x <- lazyLoadOld(fname, partition_locations, self$dim, self$ndim, private$sample_data())
        x <- do.call('[<-', args)
        cpp_create_lazyarray(x, self$dim, fname, compression = private$compress_level, uniformEncoding = TRUE)
      }
      invisible(self)
    },
    
    #' @description Internal method to generate partition summary statistics
    #' @param part integer, partition number
    #' @param x partition data
    `@generate_parition_summary` = function(part, x){
      
      if(!self$is_multi_part()){ return() }
      if(self$npart < part){
        stop("Wrong partition number: ", part)
      }
      
      storage_format <- private$storage_format
      
      # update summary statistics
      summary_file <- self$get_partition_fpath(part, summary_file = TRUE)
      
      smry <- list()
      suppressWarnings({
        smry$nas <- sum(is.na(x))
        smry$length <- length(x)
        if( storage_format %in% c('double', 'integer') ){
          smry$max <- max(x, na.rm = TRUE)
          smry$min <- min(x, na.rm = TRUE)
          smry$mean <- mean(x, na.rm = TRUE)
          smry$sd <- sd(x, na.rm = TRUE)
          smry$range <- c(smry$min, smry$max)
          smry$var <- smry$sd^2
          valid_length <- smry$length - smry$nas
          smry$meansq <- smry$var * (1 - 1 / valid_length) + smry$mean^2
        } else if( storage_format == 'complex' ){
          smry$mean <- mean(x, na.rm = TRUE)
        }
      })
      
      smry$storage_format <- storage_format
      
      saveRDS(smry, summary_file, ascii = TRUE)
      

    },
    
    #' @description Get partition summaryr statistics
    #' @param part integer, partition number
    #' @param types type of statistics to get, options are \code{'min'}, 
    #' \code{'max'}, \code{'range'}, \code{'mean'}, \code{'sd'}, \code{'var'}, 
    #' \code{'nas'}, \code{'length'}; can be more than one options
    #' @param show_warn whether to show warnings if array type is inconsistent
    #' with types of statistics
    partition_summary = function(
      part, 
      types = c('min', 'max', 'range', 'mean', 'sd', 'var', 'nas', 'length'),
      show_warn = TRUE
    ){
      if(!self$is_multi_part()){
        stop("Cannot call partition_summary if array is not multi-part")
      }
      
      if(part > self$npart){
        stop("partition overflow")
      }
      
      storage_format <- self$get_storage_format()
      if(!storage_format %in% c('double', 'integer') &&
         types %in% c('min', 'max', 'range', 'sd', 'var')){
        if( show_warn ){
          warning('Summary [min,max, range, sd, var] cannot be obtained from storage_format: ', storage_format)
        }
        types <- types[!types %in% c('min', 'max', 'range', 'sd', 'var')]
      }
      
      if(!length(types)){
        return(NULL)
      }
      fname <- self$get_partition_fpath(part, summary_file = TRUE)
      fst_name <- self$get_partition_fpath(part, summary_file = FALSE)
      
      if(file.exists(fname)){
        tryCatch({
          readRDS(fname)
        }, error = function(e){
          unlink(fname)
        })
      }
      
      if(!file.exists(fname) && file.exists(fst_name)){
        # generate summary file
        self$`@partition_map`(
          partitions = part, split = FALSE, 
          map_function = function(x, part){
            self$`@generate_parition_summary`(part, x)
        })
      }
      
      if(file.exists(fname)){
        smry <- readRDS(fname)
      } else {
        # Not exist! partition data is missing
        plen <- prod(self$partition_dim())
        smry <- list(
          nas = plen,
          length = plen,
          max = -Inf,
          min = Inf,
          mean = NA_real_,
          sd = NA_real_,
          range = c(-Inf, Inf),
          var = NA_real_,
          meansq = NA_real_,
          storage_format = storage_format
        )
      }
      
      smry[types]
      
    },
    
    #' @description Set compression level
    #' @param level from 0 to 100. 0 means no compression, 100 means max compression
    set_compress_level = function(level){
      stopifnot(level >= 0 & level <= 100)
      private$compress_level <- level
      private$save_meta()
    },
    
    #' @description Get compression level
    get_compress_level = function(){
      private$compress_level
    },
    
    #' @description Internal method to read data
    #' @param ... index set
    #' @param drop whether to drop dimension after subset, default is true
    `@get_data` = function(..., drop = TRUE){
      stopifnot(private$.valid)
      idx <- list(...)
      if(!length(idx)){
        # case 1 x[], get all the data
        idx <- lapply(self$dim, seq_len)
      } else if(length(idx) == 1){
        # case 1 x[a:b], get data by length, we have to calculate indices
        stop("subset with length is not supported right now")
      } else {
        stopifnot(length(idx) == self$ndim)
        idx <- lapply(idx, function(id){
          id <- as.integer(id)
          id[is.na(id)] <- -1L
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
      re <- lazyLoadOld(files, partition_locations = idx, 
                         partition_dim = self$partition_dim(), 
                         ndim = self$ndim, value_type = private$sample_data())
      if(drop){
        re <- drop(re)
      }
      re
    },
    
    
    #' @description Internal method to obtain a sample data to be used to determine storage mode
    `@sample_data` = function(){
      private$sample_data()
    }
    
  ),
  active = list(
    #' @field meta_name file name to store meta information
    meta_name = function(){
      private$.meta_name
    },
    
    #' @field min_version minimal version supported, for backward compatibility concerns
    min_version = function(){
      0
    },
    
    #' @field version current version of lazy data instance
    version = function(){
      private$lazyarray_version
    },
    
    #' @field dim dimension of the data
    dim = function(){
      private$.dim
    },
    
    #' @field dimnames dimension names of the data
    dimnames = function(){
      private$.dimnames
    },
    
    #' @field ndim length of dimensions
    ndim = function(){ length(private$.dim) },
    
    #' @field can_write is array read-only or writable
    can_write = function(){
      !private$read_only
    },
    
    #' @field storage_path directory where the data is stored at
    storage_path = function(){
      private$.path
    },
    
    #' @field npart number of partitions
    npart = function(){
      private$.dim[[length(private$.dim)]]
    },
    
    #' @field filesize total disk space used in gigatypes
    filesize = function(){
      files <-
        self$get_partition_fpath(
          part = seq_len(self$npart),
          full_path = TRUE,
          summary_file = FALSE
        )
      files <- files[file.exists(files)]
      if(length(files)){
        sum(file.info(files, extra_cols = FALSE)$size) / 1024^3
      } else {
        0
      }
      
    },
    
    #' @field expected_filesize expected file size in gigatypes if all partition
    #' files exist 
    expected_filesize = function(){
      
      fsize <- self$filesize
      if(fsize > 0){
        files <-
          self$get_partition_fpath(
            part = seq_len(self$npart),
            full_path = TRUE,
            summary_file = FALSE
          )
        return(fsize / mean(file.exists(files)))
      }
      
      unit_size <- 32
      if(private$storage_format == 'integer'){
        unit_size <- 4
      } else if(private$storage_format == 'double'){
        unit_size <- 8
      } else if(private$storage_format == 'complex'){
        unit_size <- 16
      }
      
      prod(self$dim) * unit_size / 1024^3
    },
    
    #' @field storage_formats_avail storage format supported
    storage_formats_avail = function(){
      c('double', 'integer', 'character', 'complex')
    },
    
    #' @field is_valid whether the array is valid
    is_valid = function(){
      private$.valid
    }
    
  )
)


