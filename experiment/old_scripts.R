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
  
  has_idx <- FALSE
  if(...length() == 1){
    tryCatch({
      idx <- ...elt(1)
      has_idx <- TRUE
    }, error = function(e){})
  }
  
  if(has_idx){
    if(!length(idx)){
      return(logical(0))
    } else {
      # stop('lazyarray x[a:b] is not supported right now')
      
      # idx to each partition?
      dm <- dim(x)
      part_size<- length(x) / dm[[length(dm)]]
      
      partition_idx <- ((idx - 1) %% part_size) + 1
      partition <- (idx - partition_idx) / part_size + 1
      
      if(isTRUE(x$`@transposed`)){
        tmp <- partition
        partition <- partition_idx
        partition_idx <- tmp
      }
      
      re <- partition_map(x, function(slice, part){
        sel <- partition == part
        list(
          data = slice[partition_idx[sel]],
          sel = sel
        )
      }, reduce = function(l){
        re <- rep(NA, length(partition))
        for(ii in seq_along(l)){
          re[l[[ii]]$sel] <- l[[ii]]$data
        }
        re
      })
      return(re)
    }
    
  }
  
  
  x$`@get_data`(drop = drop)
  
}
