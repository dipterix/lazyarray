#' @noRd
#' @title Internal Class definition for \code{AbstractLazyArray}
#' @author Zhengjia Wang
#' @description Internal class definition of lazy array objects
AbstractLazyArray <- R6::R6Class(
  classname = "AbstractLazyArray",
  portable = TRUE,
  private = list(
    .valid = TRUE,
    .path = character(0),
    .meta_name = 'lazyarray.meta',
    .file_format = '',
    .storage_format = character(0),
    .read_only = TRUE,
    .auto_clean = FALSE,
    # dimension information
    .dim = NULL,
    .dimnames = NULL,
    finalize = function(){
      if(private$.auto_clean){
        self$remove_data(warn = FALSE)
      }
    }
  ),
  public = list(
    raw_meta = NULL,
    
    save_meta = function(){
      x <- self$raw_meta
      y <- self$meta
      x[names(y)] <- y
      write_yaml(x, file = self$meta_path)
    },
    
    print = function(...){
      cat("<LazyArray> (", private$.storage_format, ')\n', sep = '')
      cat('Dimension:\t', paste(sprintf('%d ', private$.dim), collapse = 'x '), '\n')
      invisible(self)
    },
    
    flag_auto_clean = function(auto){
      private$.auto_clean <- auto
    },
    
    
    remove_data = function(force = FALSE, warn = TRUE){
      if(!private$.valid){ return(FALSE) }
      if(dir.exists(private$.path)){
        if(force || file.exists(private$.path)){
          
          # list all files within .dir
          fs <-
            c(private$.meta_name,
              self$get_partition_fpath(full_path = FALSE),
              self$get_partition_fpath(full_path = FALSE, summary_file = TRUE))
          all_fs <- list.files(private$.path, all.files = TRUE, 
                               recursive = FALSE, full.names = FALSE, 
                               include.dirs = TRUE)
          
          rest <- all_fs[!all_fs %in% c(fs, '.', '..')]
          sel_metas <- grepl('\\.meta$', rest)
          
          if(!length(rest) || all(sel_metas)){
            # cannot remove all files because some other files exist, 
            # not created by me
            unlink(private$.path, recursive = TRUE)
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
      if(warn && dir.exists(private$.path)){
        warning("LazyArray not fully cleaned at: ", private$.path, 
                '. Some files noted created by this array were detected. ',
                'Please manually remove them if they are no longer used.')
      }
      return(!private$.valid)
    },
    
    make_writable = function(){
      private$.read_only <- FALSE
    },
    
    make_readonly = function(){
      private$.read_only <- TRUE
    },
    
    get_file_format = function(){
      sprintf('.%s', private$.file_format)
    },
    
    get_partition_fpath = function(part, full_path = TRUE, summary_file = FALSE){
      if(missing(part)){
        part <- seq_len(self$npart)
      } else {
        part <- as.integer(part)
        if(base::anyNA(part) || any(part <= 0)){
          stop("partition number must be all positive")
        }
      }
      res <- sprintf('%s%s', part, self$get_file_format())
      if(full_path){
        res <- file.path(private$.path, res)
      }
      if(summary_file){
        res <- sprintf('%s.summary', res)
      }
      res
    },
    
    partition_dim = function(){
      re <- private$.dim
      re[length(re)] <- 1
      re
    },
    
    initialize = function(path, dim, storage_format = c('double', 'integer', 'complex', 'character'),
                          read_only = TRUE, meta_name = 'lazyarray.meta'){
      storage_format <- match.arg(storage_format)
      private$.meta_name <- meta_name
      private$.read_only <- read_only
      if(!dir.exists(path)){
        dir_create(path)
      }
      path <- normalizePath(path)
      private$.path <- path
      private$.storage_format <- storage_format
      if(file.exists(self$meta_path)){
        self$raw_meta <- load_yaml(self$meta_path)
        private$.dim <- self$raw_meta$dim
        private$.dimnames <- self$raw_meta$dimnames
        private$.storage_format <- self$raw_meta$storage_format
        if(private$.file_format != self$raw_meta$file_format){
          stop("File format mismatch. Header: ", self$raw_meta$file_format, "; provided: ", private$.file_format)
        }
        if(xor(
          private$.storage_format == 'complex',
          storage_format == 'complex'
        )){
          stop("complex is not compatible with the other data types")
        }
      }
      if(!is.list(self$raw_meta)){
        self$raw_meta <- list()
      }
      if(!missing(dim)){
        if(length(private$.dim)){
          dim_alt <- dim
          dim_alt[[length(dim_alt)]] <- 1
          if(prod(self$partition_dim()) != prod(dim_alt)){
            stop("partition length mismatch with existing meta information. Please check dimension.\n",
                 "header information: ", paste(private$.dim, collapse = 'x'),
                 "; provided: ", paste(dim, collapse = 'x'))
          }
        }
        private$.dim <- dim
      } else if(!file.exists(self$meta_path)){
        stop("dim is missing, so does meta file. Please specify dimension to create new array")
      } 
    },
    
    get_partition_data = function(part, reshape = NULL){
      stop("Not implemented yet")
    },
    
    generate_summary = function(parts){
      if(missing(parts)){
        parts <- seq_len(self$npart)
      }
      lapply2(parts, function(ii){
        x <- self$get_partition_data(ii, reshape = c(self$partition_length, 1))
        self$`@generate_parition_summary`(ii, x)
      })
    },
    
    `@generate_parition_summary` = function(part, x){
      
      if(self$npart < part){
        stop("Wrong partition number: ", part)
      }
      
      storage_format <- private$.storage_format
      
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
      
      
    }
    
    
  ),
  active = list(
    meta_name = function(){
      private$.meta_name
    },
    
    meta_path = function(){
      file.path(private$.path, private$.meta_name)
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
      !private$.read_only
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
      if(private$.storage_format == 'integer'){
        unit_size <- 4
      } else if(private$.storage_format == 'double'){
        unit_size <- 8
      } else if(private$.storage_format == 'complex'){
        unit_size <- 16
      }
      
      prod(self$dim) * unit_size / 1024^3
    },
    
    storage_format = function(){
      private$.storage_format
    },
    #' @field storage_formats_avail storage format supported
    storage_formats_avail = function(){
      c('double', 'integer', 'character', 'complex')
    },
    
    #' @field is_valid whether the array is valid
    is_valid = function(){
      private$.valid
    },
    
    meta = function(){
      list(
        dim = self$dim,
        dimnames = self$dimnames,
        storage_format = self$storage_format,
        file_format = private$.file_format
      )
    },
    
    sexptype = function(){
      sample_data <- 1
      storage.mode(sample_data) <- self$storage_format
      getSexpType(sample_data)
    },
    
    partition_length = function(){
      prod(self$partition_dim())
    },
    
    sample_na = function(){
      sampleNA <- NA
      storage.mode(sampleNA) <- self$storage_format
      sampleNA
    }
  )
)


