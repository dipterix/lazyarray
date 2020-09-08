#' @export
`[.LazyArray` <- function(x, ..., drop = TRUE, reshape = NULL){
  
  dim <- dim(x)
  ndots <- ...length()
  if(!ndots %in% c(0,1,length(dim))){
    stop("Dimension not match: 0, 1, ", length(dim))
  }
  
  
  dots_value <- parseDots(environment(), TRUE)
  
  files <- x$get_partition_fpath()
  
  subf <- function(i, ...){
    .Call(`_lazyarray_lazySubset`, files, environment(), dim,
          x$`@sample_data`(), reshape, drop)
  }
  
  if(length(dots_value) == 0){
    return(subf())
  }
  
  element_type <- attr(dots_value, 'element_type')
  
  # element_type is SEXP type
  # 13: integer
  # 14: double
  # 10: logical
  
  is_logical <- element_type == 10
  if(any(is_logical)){
    logical_dim <- which(is_logical)
    
    dots_value[is_logical] <- lapply(logical_dim, function(ii){
      seq_len(dim[ii])[dots_value[[ii]]]
    })
  }
  do.call('subf', dots_value)
  
}


get_missing_value <- function(){
  (function(...){
    parseDots(environment(), FALSE)[[1]]
  })(,)
}


#' @export
`[.LazyMatrix` <- function(x, i, j, drop = TRUE){
  
  miss_j <- missing(j)
  miss_i <- missing(i)
  
  if(x$`@transposed`){
    y <- t(x)
  } else {
    y <- x
  }
  
  Narg <- nargs() - !missing(drop)
  
  # x[] or x[i]
  if(Narg < 3){
    if(miss_i){
      re <- `[.LazyArray`(y, drop = FALSE)
      if(x$`@transposed`){
        re <- t(re)
      }
      if(drop){
        re <- drop(re)
      }
      return(re)
    } else {
      dim <- dim(x)
      if(x$`@transposed`){
        rows <- (i - 1) %% dim[1]
        rows <- rows + 1
        cols <- (i - rows) / dim[1] + 1
        re <- `[.LazyArray`(y, cols + (rows-1) * dim[2])
      } else {
        re <- `[.LazyArray`(y, i)
      }
      return(re)
    }
  }
  
  # x[,]
  if(miss_i && miss_j){
    re <- `[.LazyArray`(y, drop = FALSE)
    if(x$`@transposed`){
      re <- t(re)
    }
    if(drop){
      re <- drop(re)
    }
    return(re)
  }
  
  # x[i,]
  if(miss_j){
    if(x$`@transposed`){
      re <- t(`[.LazyArray`(y, , i, drop = FALSE))
    } else {
      re <- `[.LazyArray`(y, i, , drop = FALSE)
    }
    if(drop){
      re <- drop(re)
    }
    return(re)
  }
  
  # x[,j]
  if(miss_i){
    if(x$`@transposed`){
      re <- t(`[.LazyArray`(y, j, , drop = FALSE))
    } else {
      re <- `[.LazyArray`(y, , j, drop = FALSE)
    }
    if(drop){
      re <- drop(re)
    }
    return(re)
  }
  
  # x[i,j]
  if(x$`@transposed`){
    re <- t(`[.LazyArray`(y, j, i, drop = FALSE))
  } else {
    re <- `[.LazyArray`(y, i, j, drop = FALSE)
  }
    
  if(drop){
    re <- drop(re)
  }
  return(re)
  
}

#' @export
`[<-.LazyArray` <- function(x, ..., value){
  
  # check dimensions
  nidx <- ...length()
  idx <- list()
  dim <- x$dim
  if(nidx == length(dim)){
    for(ii in seq_len(nidx)){
      idx[[ ii ]] <- tryCatch({
        tmp <- ...elt(ii)
        if(is.logical(tmp)){
          if(length(tmp) < dim[ii]){
            tmp <- rep(tmp, floor(dim[ii] / length(tmp)))
            tmp <- tmp[seq_len(dim[ii])]
          }
          if(length(tmp) > dim[ii]){
            stop("index out of bound at index ", ii)
          }
          which(tmp)
        } else if (is.numeric(tmp)){
          tmp
        } else if (is.character(tmp)){
          unlist(lapply(tmp, function(s){
            which(x$dimnames[[ii]] == s)
          }))
        }
      }, error = function(e){
        seq_len(dim[[ii]])
      })
    }
    target_dim <- sapply(idx, length)
    if(prod(target_dim) == 0){
      return(x)
    }
    idx <- c(list(value = quote(value)), idx)
    return(do.call(x$`@set_data`, idx))
  }
  if(...length() == 1){
    idx <- tryCatch({
      ...elt(1)
    }, error = function(e){
      NULL
    })
    if(length(idx)){
      stop('lazyarray x[a:b] is not supported right now')
    }
  }
  idx <- c(list(value = quote(value)), lapply(x$dim, seq_len))
  do.call(x$`@set_data`, idx)
  
}


#' @export
dim.LazyArray <- function(x){
  x$dim
}

#' @export
`dim<-.LazyArray` <- function(x, value){
  x$set_dim(dim = value)
  return(x)
}

#' @export
`dimnames<-.LazyArray` <- function(x, value){
  x$set_dim(dim = x$dim, dimnames = value)
  return(x)
}


#' @export
dimnames.LazyArray <- function(x){
  x$dimnames
}

#' @export
length.LazyArray <- function(x){
  prod(x$dim)
}

#' @export
subset.LazyArray <- function(x, ..., env = parent.frame(), drop = FALSE){
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
  
  do.call(x$`@get_data`, sel)
  
}

#' @export
max.LazyArray <- function(x, ..., na.rm = FALSE){
  
  dim <- x$dim
  len <- prod(dim)
  
  if(!len){
    return(max(NULL, na.rm = TRUE))
  }
  
  if(x$is_multi_part()){
    
    # get partition summary
    mx <- lapply2(seq_len(x$npart), function(part){
      x$partition_summary(part = part, types = c('max', 'nas'), show_warn = FALSE)
    })
    
    if(!na.rm && any(sapply(mx, '[[', 'nas') > 0)){
      return(NA)
    }
    
    return(max(sapply(mx, '[[', 'max')))
    
  }

  # no partition, just manually calculate all  
  partition <- make_chunks(dim, recursive = TRUE, ...)
    
  mx <- lapply2(seq_len(partition$nchunks), function(chunk){
    idx <- partition$get_indices(chunk)
    x$is_multi_part()
    expr <- parse(text = sprintf('max(x[%s], na.rm = %s)', idx, na.rm))
    suppressWarnings(eval(expr))
  })
  
  
  mx <- unlist(mx)
  
  if(any(is.na(mx))){
    return(NA)
  }
  if(all(mx < 0) && all(is.infinite(mx))){
    return(max(NULL, na.rm = TRUE))
  }
  mx <- mx[!is.infinite(mx)]
  max(mx)
  
}


#' @export
min.LazyArray <- function(x, ..., na.rm = FALSE){
  dim <- x$dim
  len <- prod(dim)
  
  if(!len){
    return(min(NULL, na.rm = TRUE))
  }
  
  if(x$is_multi_part()){
    
    # get partition summary
    mx <- lapply2(seq_len(x$npart), function(part){
      x$partition_summary(part = part, types = c('min', 'nas'), show_warn = FALSE)
    })
    
    if(!na.rm && any(sapply(mx, '[[', 'nas') > 0)){
      return(NA)
    }
    
    return(min(sapply(mx, '[[', 'min')))
    
  }
  partition <- make_chunks(dim, recursive = TRUE, ...)
  
  mx <- lapply2(seq_len(partition$nchunks), function(chunk){
      idx <- partition$get_indices(chunk)
      x$is_multi_part()
      expr <- parse(text = sprintf('min(x[%s], na.rm = %s)', idx, na.rm))
      suppressWarnings(eval(expr))
    })
  
  mx <- unlist(mx)
  
  if(any(is.na(mx))){
    return(NA)
  }
  if(all(mx > 0) && all(is.infinite(mx))){
    return(min(NULL, na.rm = TRUE))
  }
  mx <- mx[!is.infinite(mx)]
  min(mx)
  
}

#' @export
range.LazyArray <- function(x, ..., na.rm = FALSE){
  c(min(x, na.rm = na.rm), max(x, na.rm = na.rm))
}

#' @export
mean.LazyArray <- function(x, ..., na.rm = FALSE){
  mx <- lapply2(seq_len(x$npart), function(part){
    x$partition_summary(part = part, types = c('mean', 'nas', 'length'), show_warn = FALSE)
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
  structure(
    sum(mean * len / tl),
    count = sum(len)
  )
}

#' @export
sum.LazyArray <- function(x, ..., na.rm = FALSE){
  m <- mean(x, ..., na.rm = na.rm)
  
  if(is.na(m)){
    return(m)
  }
  m * attr(m, 'count')
}


#' @export
head.LazyArray <- function(x, n = 2L, ...){
  dm <- x$dim
  dm <- lapply(dm, function(ii){
    seq_len(min(ii, n))
  })
  sd <- sapply(dm, length)
  
  if(any(sd == 0)){
    return(structure(array(NA, dim = sd), slice_dimension = paste(sd, collapse = ' x ')))
  }
  
  structure(do.call(x$`@get_data`, dm), slice_dimension = paste(sd, collapse = ' x '))
}

#' @export
tail.LazyArray <- function(x, n = 2L, ...){
  dm <- x$dim
  dm <- lapply(dm, function(ii){
    if(ii <= 0){ return(integer(0)) }
    seq.int(max(1, ii - n + 1), ii)
  })
  sd <- sapply(dm, length)
  
  if(any(sd == 0)){
    return(structure(array(NA, dim = sd), slice_dimension = paste(sd, collapse = ' x ')))
  }
  
  structure(do.call(x$`@get_data`, dm), slice_dimension = paste(sd, collapse = ' x '))
}

#' @export
summary.LazyArray <- function(object, ...){
  x <- structure(list(
    storage_format = object$get_storage_format(),
    npart = object$npart,
    dim = object$dim,
    dimnames = object$dimnames,
    class = class(object),
    multi_part = object$is_multi_part()
  ), class = 'LazyArray-summary')  
  
  if(x$multi_part){
    x$partitions = t(sapply(seq_len(object$npart), function(ii){
      unlist(object$partition_summary(
        ii, types = c('min', 'max', 'mean', 'sd', 'nas', 'length'), 
        show_warn = FALSE
      ))
    }))
                             
    x$partitions <- as.data.frame(x$partitions)
    
    nms <- structure(
      c('Min', 'Max', 'Mean', 'Standard Deviation', 'NAs', 'Length'),
      names = c('min', 'max', 'mean', 'sd', 'nas', 'length')
    )
    
    names(x$partitions) <- nms[names(x$partitions)]
  }
  
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
  if(x$multi_part){
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
    
    cat('** Partition summary values use `na.rm=TRUE`\n')
    cat('\n')
  }
  
  invisible(x)
}



