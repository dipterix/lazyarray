dir_create <- function(path, showWarnings = FALSE, recursive = TRUE, ...){
  dir.create(path = path, showWarnings = showWarnings, recursive = recursive, ...)
}

load_yaml <- function(path, ...){
  read_yaml(path, ...)
}


save_yaml <- function(x, path, ...){
  write_yaml(x, path, ...)
}


deparse1 <- function(..., collapse = ''){
  paste0(deparse(...), collapse = collapse)
}

rand_string <- function(length = 10){
  paste(sample(c(letters, LETTERS, 0:9), length, replace = TRUE), collapse = '')
}

make_chunks <- function(dim, chunk_size = 1024, max_nchunks = 200, recursive = FALSE){
  max_nchunks <- floor(max_nchunks)
  len <- prod(dim)
  drange <- lapply(dim, function(d){ c(1, d) })
  
  
  if(len == 0){
    return(list(nchunks = 0, get_indices = function(i, as_numeric = FALSE){
      if(as_numeric){ return(NULL) }
      paste(rep('', length(dim)), collapse = ',')
    }))
  }
  if(len < chunk_size || len <= getOption('lazyarray.chunk_memory', 80) * 125000){
    return(list(nchunks = 1, get_indices = function(i, as_numeric = FALSE){
      if(as_numeric){ return( drange ) }
      paste(rep('', length(dim)), collapse = ',')
    }))
  }
  
  lastdim <- dim[length(dim)]
  
  if( len < chunk_size * max_nchunks ){
    max_nchunks <- ceiling(len / chunk_size);
    if( chunk_size * max_nchunks < len ){
      max_nchunks <- max_nchunks + 1
    }
  }
  
  if(!recursive && lastdim < max_nchunks){
    max_nchunks <- lastdim
  }
  
  if(lastdim >= max_nchunks){
    nchunks <- max_nchunks
    m <- ceiling(lastdim / max_nchunks)
    x2 <- m * nchunks - lastdim
    x1 <- nchunks - x2
    return(list(
      nchunks = nchunks,
      get_indices = function(i, as_numeric = FALSE){
        if( i <= x1 ){
          s <- (i - 1) * m + 1
          e <- i * m
        } else {
          s <- x1 * m + (i-x1-1) * (m-1) + 1
          e <- x1 * m + (i-x1) * (m-1)
        }
        if(as_numeric){
          re <- drange
          re[[length(dim)]] <- c(s, e)
          return(re)
        } else {
          re <- paste(rep('', length(dim)), collapse = ',')
          if(s == e){
            return(sprintf('%s%d', re, s))
          } else {
            return(sprintf('%s%d:%d', re, s, e))
          }
        }
        
        
      }
    ))
  }
  
  # lastdim < max_nchunks and recursive
  if( lastdim > max_nchunks / 2 ){
    return(list(
      nchunks = lastdim,
      get_indices = function(i, as_numeric = FALSE){
        if(as_numeric){
          re <- drange
          re[[length(dim)]] <- c(i, i)
          return(re)
        } else {
          re <- paste(rep('', length(dim)), collapse = ',')
          sprintf('%s%d', re, i)
        }
      }
    ))
  }
  re <-
    Recall(
      dim[-length(dim)],
      chunk_size = chunk_size,
      max_nchunks = max_nchunks / lastdim,
      recursive = FALSE
    )
  
  nchunks = re$nchunks * lastdim
  get_indices <- function(i, as_numeric = FALSE){
    i1 <- floor((i - 1) / lastdim) + 1
    i2 <- i - lastdim * (i1-1)
    
    s <- re$get_indices(i1, as_numeric = as_numeric)
    
    if(as_numeric){
      s[[length(dim)]] <- c(i2, i2)
      return(s)
    } else {
      return(sprintf('%s,%d', s, i2))
    }
    
  }
  
  return(list(
    nchunks = nchunks,
    get_indices = get_indices
  ))
  
}

lapply2 <- function(x, FUN, ...){
  if( length(x) > 1 && has_dipsaus() ){
    dipsaus::lapply_async2(x, FUN, FUN.args = list(...), plan = FALSE)
  } else {
    lapply(x, FUN)
  }
}


