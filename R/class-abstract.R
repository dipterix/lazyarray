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
      stopifnot(self$can_write)
      x <- self$raw_meta
      y <- self$meta
      x[names(y)] <- y
      x$dimnames <- NULL
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
        if(isTRUE(private$.file_format != self$raw_meta$file_format)){
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
      
      
    },
    
    
    `@chunk_map` = function(
      map_function, max_nchunks = 50, ...
    ){
      # use chunk_map, do not call this function directly
      
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
      
      lapply2(seq_len(chunkf$nchunks), function(ii){
        idx_range <- chunkf$get_indices(ii, as_numeric = TRUE)[[1]]
        map_f(
          self[seq.int(idx_range[[1]], idx_range[[2]]),,drop=FALSE],
          ii, idx_range
        )
      })
    },
    
    get_partition_summary = function(
      part, 
      types = c('min', 'max', 'range', 'mean', 'sd', 'var', 'nas', 'length'),
      show_warn = TRUE, cache = TRUE
    ){
      
      if(part > self$npart){
        stop("partition overflow")
      }
      
      storage_format <- self$storage_format
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
      
      if(file.exists(fname)){
        tryCatch({
          readRDS(fname)
        }, error = function(e){
          unlink(fname)
        })
      }
      
      if(!cache || (!file.exists(fname) && self$has_partition(part))){
        # generate summary file
        self$generate_summary(part)
      }
      
      if(file.exists(fname)){
        smry <- readRDS(fname)
      } else {
        # Not exist! partition data is missing
        plen <- self$partition_length
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
      isFALSE(private$.read_only)
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
        dim = private$.dim,
        dimnames = private$.dimnames,
        storage_format = private$.storage_format,
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






#' @export
dim.AbstractLazyArray <- function(x){
  x$dim
}

#' @export
`dim<-.AbstractLazyArray` <- function(x, value){
  stopifnot(length(value) >= 2)
  tmp <- value
  tmp[[length(tmp)]] <- 1
  
  if(prod(tmp) != x$partition_length){
    stop("Partition length mismatch with new dimension.")
  }
  x$.__enclos_env__$private$.dim <- value
  x$.__enclos_env__$private$.dimnames <- NULL
  return(x)
}

#' @export
`dimnames<-.AbstractLazyArray` <- function(x, value){
  if(!is.null(value)){
    name_length <- sapply(value, length)
    if(!all(name_length == 0 | name_length == x$dim)){
      stop("Incorrect dimension")
    }
  }
  x$.__enclos_env__$private$.dimnames <- value
  return(x)
}


#' @export
dimnames.AbstractLazyArray <- function(x){
  x$dimnames
}

#' @export
length.AbstractLazyArray <- function(x){
  prod(x$dim)
}

#' @export
subset.AbstractLazyArray <- function(x, ..., env = parent.frame(), drop = FALSE){
  formats <- list(...)
  dnams <- x$dimnames
  nms <- names(dnams)
  d_env <- new.env(parent = env)
  sel <- lapply(x$dim, function(n){ rep(TRUE, n) })
  names(sel) <- nms
  for(nm in nms){
    if(nm != ''){
      d_env[[nm]] <- dnams[[nm]]
    }
  }
  
  # evaluate formates
  
  
  for(fmt in formats){
    if(!identical(fmt[[1]], quote(`~`))){
      stop("Subset formula ", deparse1(fmt), " is invalid for subsetting a lazy array. Use some thing like 'var ~ var < 2'")
    }
    fmt[[1]] <- quote(`=`)
    tmp_env <- new.env(parent = d_env)
    eval(fmt, envir = tmp_env)
    for(nm in names(tmp_env)){
      if(nm %in% nms){
        if(is.logical(tmp_env[[nm]])){
          sel[[nm]] <- sel[[nm]] & tmp_env[[nm]]
        } else{
          stop("Subset formula ", deparse1(fmt), "does not return TRUE/FALSE results")
        }
      }
    }
  }
  
  sel <- lapply(sel, which)
  names(sel) <- NULL
  sel$drop <- drop
  
  
  do.call(`[`, c(list(quote(x)), sel))
  
}

#' @export
max.AbstractLazyArray <- function(x, cache = TRUE, ..., na.rm = FALSE){
  
  dim <- x$dim
  len <- prod(dim)
  
  if(!len){
    return(max(NULL, na.rm = na.rm))
  }
  
  # get partition summary
  mx <- lapply2(seq_len(x$npart), function(part){
    x$get_partition_summary(part = part, types = c('max', 'nas'), show_warn = FALSE, cache = cache)
  })
  
  if(!na.rm && any(sapply(mx, '[[', 'nas') > 0)){
    re <- NA
  } else {
    re <- max(sapply(mx, '[[', 'max'))
  }
  
  if(cache)
    re <- structure(re, messages = "summary generated from cache.")
  
  re
  
}


#' @export
min.AbstractLazyArray <- function(x, cache = TRUE, ..., na.rm = FALSE){
  dim <- x$dim
  len <- prod(dim)
  
  if(!len){
    return(min(NULL, na.rm = na.rm))
  }
  
  # get partition summary
  mx <- lapply2(seq_len(x$npart), function(part){
    x$get_partition_summary(part = part, types = c('min', 'nas'), show_warn = FALSE, cache = cache)
  })
  if(!na.rm && any(sapply(mx, '[[', 'nas') > 0)){
    re <- NA
  } else {
    re <- min(sapply(mx, '[[', 'min'))
  }
  if(cache)
    re <- structure(re, messages = "summary generated from cache.")
  
  re
  
}

#' @export
range.AbstractLazyArray <- function(x, cache = TRUE, ..., na.rm = FALSE){
  min <- min(x, na.rm = na.rm, cache = cache)
  if(is.na(min)){ re <- c(NA, NA) } else {
    re <- c(min, max(x, na.rm = na.rm, cache = TRUE))
  }
  
  if(cache)
    re <- structure(re, messages = "summary generated from cache.")
  
  re
}

#' @export
mean.AbstractLazyArray <- function(x, cache = TRUE, ..., na.rm = FALSE){
  mx <- lapply2(seq_len(x$npart), function(part){
    x$get_partition_summary(part = part, types = c('mean', 'nas', 'length'), show_warn = FALSE, cache = cache)
  })
  
  nas <- sapply(mx, '[[', 'nas')
  if(!na.rm && sum(nas) > 0){
    return(NA)
  }
  
  len <- sapply(mx, '[[', 'length') - nas
  mean <- sapply(mx, '[[', 'mean')
  tl <- length(x) - sum(nas)
  mean <- mean[len > 0]
  len <- len[len > 0]
  if(cache){
    re <- structure(sum(mean * len / tl), messages = "summary generated from cache.", count = sum(len))
  } else {
    re <- structure(
      sum(mean * len / tl),
      count = sum(len),
      
    )
  }

  re  
}

#' @export
sum.AbstractLazyArray <- function(x, cache = TRUE, ..., na.rm = FALSE){
  m <- mean(x, ..., na.rm = na.rm, cache = cache)
  
  if(!is.na(m)){
    m <- m * attr(m, 'count')
  }
  
  if(cache){
    m <- structure(m, messages = "summary generated from cache.")
  }
  m
}


#' @export
head.AbstractLazyArray <- function(x, n = 2L, ...){
  dm <- x$dim
  dm <- lapply(dm, function(ii){
    seq_len(min(ii, n))
  })
  sd <- sapply(dm, length)
  
  if(any(sd == 0)){
    return(structure(array(NA, dim = sd), slice_dimension = paste(sd, collapse = ' x ')))
  }
  
  structure(do.call('[', c(list(quote(x)), dm)), slice_dimension = paste(sd, collapse = ' x '))
}

#' @export
tail.AbstractLazyArray <- function(x, n = 2L, ...){
  dm <- x$dim
  dm <- lapply(dm, function(ii){
    if(ii <= 0){ return(integer(0)) }
    seq.int(max(1, ii - n + 1), ii)
  })
  sd <- sapply(dm, length)
  
  if(any(sd == 0)){
    return(structure(array(NA, dim = sd), slice_dimension = paste(sd, collapse = ' x ')))
  }
  
  structure(do.call('[', c(list(quote(x)), dm)), slice_dimension = paste(sd, collapse = ' x '))
}

#' @export
summary.AbstractLazyArray <- function(object, cache = TRUE, quiet = FALSE, ...){
  if(!quiet){
    if(cache){
      message("Summary data is generated from cache. If you have changed data recently, please re-cache")
    } else {
      message("If the data does change frequently, you might want to set `cache=TRUE` to speed-up")
    }
  }
  
  x <- structure(list(
    storage_format = object$storage_format,
    npart = object$npart,
    dim = object$dim,
    dimnames = object$dimnames,
    class = class(object),
    cache = cache
  ), class = 'LazyArray-summary')  
  
  x$partitions = t(sapply(seq_len(object$npart), function(ii){
    unlist(object$get_partition_summary(
      ii, types = c('min', 'max', 'mean', 'sd', 'nas', 'length'), 
      show_warn = FALSE, cache = cache
    ))
  }))
  
  x$partitions <- as.data.frame(x$partitions)
  
  nms <- structure(
    c('Min', 'Max', 'Mean', 'Standard Deviation', 'NAs', 'Length'),
    names = c('min', 'max', 'mean', 'sd', 'nas', 'length')
  )
  
  names(x$partitions) <- nms[names(x$partitions)]
  
  x
}

#' @export
`print.LazyArray-summary` <- function(x, n = 5L, ...){
  cat(sprintf('Class:      [%s]\n', paste(x$class, collapse = ' -> ')))
  cat(sprintf('Type:       [%s]\n', x$storage_format))
  cat(sprintf('Dimension:  [%s]\n', paste(x$dim, collapse = ' x ')))
  
  if(is.null(x$dimnames)){
    cat("Dimension names: None\n")
  } else {
    cat("Dimension names: \n")
    op <- capture.output(str(x$dimnames, comp.str = '   ', strict.width = 'cut'))
    op <- op[-1]
    cat(op, sep = '\n')
    cat('\n')
  }
  cat(sprintf('Partitions: [%d]\n', x$npart))
  cat(sprintf('Partition summaries: \n'))
  
  if(x$npart > 3 * n){
    print(head(x$partitions, n))
    cat(sprintf("[ ..., omitted %s rows ]\n", x$npart - 3 * n))
    print(tail(x$partitions, n))
    cat('\n')
    cat('* Call `.$partitions` to get summary table for each parition\n')
  } else {
    print(x$partitions)
  }
  
  cat('* Partition summary values use `na.rm=TRUE`\n')
  if(x$cache){
    cat('** Partition summary are generated from cache. If you have changed data recently, please re-run `x$generate_summary()`\n')
  }
  cat('\n')
  
  invisible(x)
}






