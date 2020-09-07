#' @export
t.LazyMatrix <- function(x){
  x$transpose()
}


auto_chunks <- function(x, limit = 0.5){
  files <- x$get_partition_fpath()
  if(length(files)){
    fct <- mean(file.exists(files))
  } else {
    fct <- 1
  }
  
  max_nchunks <- x$filesize / limit
  if( fct > 0 ){
    max_nchunks <- max_nchunks / fct
  }
  max_nchunks <- max(ceiling(max_nchunks), 1L)
  max_nchunks
}


#' Matrix multiplication
#' @param x,y \code{LazyMatrix} or anything that can be 
#' transformed into \code{LazyMatrix}
#' @param max_chunks max number of chunks to split the data
#' @param weights weights while perform \code{x \%*\% y}. If not \code{NULL},
#' the results will be \code{x \%*\% diag(weights) \%*\% y}
#' @param hybrid_limit measured in gigabytes. If the estimated memory usage
#' does not exceed \code{hybrid_limit}, then perform naive multiplication, 
#' i.e. \code{x[] \%*\% y[]} in-memory
#' @return Depends on whether \code{hybrid_limit} is reached. If hybrid limit
#' is not reached, i.e. multiplication in-memory, then returns a matrix. 
#' Otherwise return a \code{LazyMatrix}.
#' @details Depending on whether \code{x} or \code{y} is transposed, the 
#' performance varies a lot under "lazy" mode. \code{lazy_matmul} is optimized
#' for partition-major \code{LazyMatrix} instances. For example, if \code{m}
#' is a \code{n-by-p} matrix with \code{n < p}, it's highly recommended to 
#' create lazy version of matrices by \code{t(as.lazyarray(t(m)))} or 
#' \code{as.lazymatrix(m)}
#' @examples 
#' 
#' x <- matrix(1:16,4)
#' y <- as.lazymatrix(x)
#' all(lazy_matmul(x, x) == x%*%x)
#' 
#' # In-memory calculation (x < 2GB)
#' lazy_matmul(y, y)
#' 
#' # Lazy mode (slow but larger memory capacity)
#' re <- lazy_matmul(y, y, hybrid_limit = 0)
#' re
#' 
#' all(re[] == x%*%x)
#' 
#' \dontrun{
#' # ------------ Lazy-mode performance comparison (slow example) ------------
#' a <- matrix(1:100000, 25)
#' 
#' # 4000 partitions, slow
#' row_major <- as.lazyarray(a)
#' 
#' # 25 partitions, fast
#' col_major <- as.lazyarray(t(a))
#' 
#' # 1. x (25 partitions), y (25 partitions)
#' x <- t(as.lazymatrix(col_major))
#' y <- as.lazymatrix(col_major)
#' system.time(lazy_matmul(x, y, hybrid_limit = 0))
#' 
#' # 2. x (4000 partitions), y (25 partitions)
#' x <- as.lazymatrix(row_major)
#' y <- as.lazymatrix(col_major)
#' system.time(lazy_matmul(x, y, hybrid_limit = 0))
#' 
#' # 3. x (4000 partitions), y (4000 partitions)
#' x <- as.lazymatrix(row_major)
#' y <- t(as.lazymatrix(row_major))
#' system.time(lazy_matmul(x, y, hybrid_limit = 0))
#' 
#' 
#' # 3. x (25 partitions), y (4000 partitions)
#' x <- t(as.lazymatrix(col_major))
#' y <- t(as.lazymatrix(row_major))
#' system.time(lazy_matmul(x, y, hybrid_limit = 0))
#' 
#' }
#' 
#' @export
lazy_matmul <- function(x, y, max_chunks, weights = NULL, hybrid_limit = 2){
  
  x <- as.lazymatrix(x, read_only = TRUE)
  y <- as.lazymatrix(y, read_only = TRUE)
  
  dx <- dim(x)
  dy <- dim(y)
  
  stopifnot(dx[2] == dy[1])
  
  total_l <- dy[1]
  
  
  if(!is.null(weights)){
    stopifnot(length(weights) == total_l)
    lply <- lapply
  } else {
    lply <- lapply2
  }
  
  if(any(c(dx, dy) == 0)){
    return(matrix(numeric(0), nrow = dx[1], ncol = dy[2]))
  }
  
  
  sformat <- x$get_storage_format()
  if(sformat == 'integer'){
    sformat <- 'double'
  }
  stopifnot(sformat %in% c('double', 'complex'))
  
  dn <- c(dx[1], dy[2])
  # re <- lazymatrix(tempfile(), storage_format = sformat, dim = dn)
  
  
  xfiles <- x$get_partition_fpath()
  yfiles <- y$get_partition_fpath()
  
  x_sdata <- x$`@sample_data`()
  y_sdata <- y$`@sample_data`()
  
  if( sformat == 'complex' ){
    coln <- sprintf('V%d%s', 1, c('R', 'I'))
  } else {
    coln <- sprintf('V%d', 1)
  }
  
  if(x$`@transposed` && y$`@transposed`){
    if(missing(max_chunks)){
      res <- Recall(t(y), t(x), weights = weights)
    } else {
      res <- Recall(t(y), t(x), max_chunks = max_chunks, weights = weights)
    }
    
    
    return(t(res))
  }
  
  
  # check if x and y is small enough
  if(x$expected_filesize * max(1, dy[[2]] / dy[[1]]) < hybrid_limit){
    if(is.null(weights)){
      return(x[] %*% y[])
    }
    return(x[] %*% (y[] * weights))
  }
  
  res <- lazymatrix(tempfile(), storage_format = sformat, dim = dn)  
  
  if(!x$`@transposed` && y$`@transposed`){
    # x[chunk,] %*% y[,ii]
    
    if(missing(max_chunks)){
      max_chunks <- auto_chunks(x)
    }
    
    # get chunk - y
    max_ychunksize <- max(0.5 / (x$expected_filesize / max_chunks / dx[2]), 1)
    max_ychunksize <- ceiling(max_ychunksize)
    max_ychunksize <- min(max_ychunksize, dy[2])
    
    chunkf <- make_chunks(dy[[2]], chunk_size = max_ychunksize)
    
    lapply(seq_len(chunkf$nchunks), function(ii){
      
      sub_idy <- chunkf$get_indices(ii, as_numeric = TRUE)[[1]]
      sub_seq <- seq.int(sub_idy[1], sub_idy[2])
      chunk_y <- y[,sub_seq,drop=FALSE]
      if(!is.null(weights)){
        chunk_y <- chunk_y * weights
      }
      chunk_map(x, function(chunk_x){
        chunk_x %*% chunk_y
      }, reduce = function(mapped){
        res[,sub_seq] <- do.call('rbind', mapped)
        NULL
      }, max_nchunks = max_chunks)
    })
    
    
    return(res)
  }
  
  if(x$`@transposed` && !y$`@transposed`){
    # x is transposed and y is not (best case)
    if(missing(max_chunks)){
      max_chunks <- auto_chunks(x)
    }
    
    chunkf <- make_chunks(ncol(x), max_nchunks = max_chunks, recursive = FALSE)
    
    mapped <- lply(seq_len(chunkf$nchunks), function(ii){
      idx_range <- chunkf$get_indices(ii, as_numeric = TRUE)[[1]]
      idx_seq <- seq.int(idx_range[[1]], idx_range[[2]])
      partition_locations = list(idx_seq, 1L)
      # get chunks from x and y
      chunk_x <- t(lazyLoadOld(files = xfiles, partition_dim = c(total_l, 1), 
                                    partition_locations = partition_locations, 
                                    ndim = 2L, value_type = x_sdata))
      chunk_y <- lazyLoadOld(files = yfiles, partition_dim = c(total_l, 1), 
                                    partition_locations = partition_locations, 
                                    ndim = 2L, value_type = y_sdata)
      if(!is.null(weights)){
        chunk_weights <- weights[idx_seq]
        
        chunk_y <- chunk_y * chunk_weights
      }
      
      chunk_x %*% chunk_y
      
    })
    
    re <- 0
    for(pdata in mapped){
      re <- pdata + re
    }
    res[] <- re
    
    return(res)
  }
  
  
  if(!x$`@transposed` && !y$`@transposed`){
    # y must be a small matrix
    
    # x is transposed and y is not (best case)
    if(missing(max_chunks)){
      max_chunks <- auto_chunks(x)
    }
    
    y <- y[]
    
    if(!is.null(weights)){
      y <- y * weights
    }
    
    chunkf <- make_chunks(nrow(x), max_nchunks = max_chunks, recursive = FALSE)
    
    chunk_size <- chunkf$get_indices(1, as_numeric = TRUE)[[1]]
    csize <- chunk_size[2]-chunk_size[1]+1
    tmp <- lazyarray(tempfile(), storage_format = sformat, dim = c(dy[2], csize, chunkf$nchunks))
    
    idx <- lapply(seq_len(chunkf$nchunks), function(ii){
      
      idx_range <- chunkf$get_indices(ii, as_numeric = TRUE)[[1]]
      idx_seq <- seq.int(idx_range[[1]], idx_range[[2]])
      partition_locations = list(idx_seq, 1L)
      # get chunks from x and y
      
      part <- lazyLoadOld(
        xfiles, partition_locations = partition_locations, 
        partition_dim = c(dx[[1]], 1L), ndim = 2L, value_type = x_sdata
      )
      part <- t(part %*% y)
      tmp[,seq_len(ncol(part)),ii] <- part
      ncol(part)
    })
    
    idx <- unlist(idx)
    
    lply(seq_len(dy[[2]]), function(col){
      
      pdata1 <- tmp[col, ,idx == csize]
      if(any(idx < csize)){
        pdata1 <- c(pdata1, tmp[col, seq_len(csize-1),idx < csize])
      }
      
      res[,col] <- pdata1
      NULL
      
    })
    return(res)
    
  }
  
  
}



