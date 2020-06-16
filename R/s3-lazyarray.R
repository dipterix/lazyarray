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
