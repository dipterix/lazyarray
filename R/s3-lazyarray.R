#' @export
`[.LazyArray` <- function(x, ..., drop = TRUE){
  
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
            re <- which(x$dimnames[[ii]] == s)
            if(length(re)){ re[[1]] } else { -1 }
          }))
        }
      }, error = function(e){
        seq_len(dim[[ii]])
      })
    }
    target_dim <- sapply(idx, length)
    if(prod(target_dim) == 0){
      if(drop){
        return(x$`@sample_data`()[NULL])
      } else {
        return(array(x$`@sample_data`(), dim = target_dim))
      }
      
    }
    
    idx$drop <- drop
    return(do.call(x$`@get_data`, idx))
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
  
  
  x$`@get_data`(drop = drop)
  
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


