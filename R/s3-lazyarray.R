#' @title Load Lazy Array from Given Path
#' @param path character, path of the array
#' @param read_only whether setting data is allowed
#' @return A \code{LazyArray} object
#' @examples 
#' 
#' path <- tempfile()
#' create_lazyarray(path, 'double', dim = c(3,4,5), multipart = TRUE)
#' 
#' x <- load_lazyarray(path, read_only = FALSE)
#' x[2,3:4, 2:1] <- 1:4
#' x[ , , 2]
#' 
#' # Expend dimension for multiple partition data only
#' dim(x) <- c(3,4,6)
#' dimnames(x) <- list(dim1 = as.character(1:3),
#'                     dim2 = letters[1:4], 
#'                     dim3 = LETTERS[1:6])
#' x[ , , 'B', drop = FALSE]
#' 
#' # Non-standard subset methods
#' names(dimnames(x))
#' subset(x, dim1 ~ dim1 == '2', dim2 ~ dim2 %in% c('a', 'c'), drop = TRUE)
#' 
#' # Free up space
#' x$remove_data()
#' 
#' \donttest{
#' 
#' # This example needs at lease 4 GB hard disk space
#' 
#' # Speed test
#' path <- tempfile()
#' x <- create_lazyarray(path, 'complex', dim = c(100,200,300,20), 
#'                       multipart = TRUE, multipart_mode = 1)
#' 
#' # automatically call x$remove_data() upon garbage collection
#' x$flag_auto_clean(TRUE)
#' 
#' 
#' # set data (4 GB data) using 4 cores, compression level 50
#' # ~30-40 seconds, ~100MB/s
#' 
#' system.time({
#'   lapply(1:20, function(ii){
#'     x[,,,ii] <- rnorm(100*200*300) * (1+2i)
#'   })
#' })
#' 
#' # Reading 64 MB data using 4 cores
#' # ~0.25 seconds
#' 
#' system.time({
#'   x[1:100, sample(200, 200), 100:1, 2:4]
#' })
#' 
#' # This call requires 4GB of RAM
#' # Reading all 4GB data using 4 cores
#' # ~4 seconds (1 GB/s)
#' 
#' system.time({
#'   x[]
#' })
#' 
#' }
#' 
#' @export
load_lazyarray <- function(path, read_only = TRUE){
  LazyArray$new(path = path, read_only = read_only)
}


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
            tmp = rep(tmp, floor(dim[ii] / length(tmp)))
            tmp = tmp[seq_len(dim[ii])]
          }
          if(length(tmp) > dim[ii]){
            stop("index out of bound at index ", ii)
          }
          which(tmp)
        } else if (is.numeric(tmp)){
          tmp
        } else if (is.character(tmp)){
          unlist(lapply(tmp, function(s){
            re = which(x$dimnames[[ii]] == s)
            if(length(re)){ re[[1]] } else { -1 }
          }))
        }
      }, error = function(e){
        seq_len(dim[[ii]])
      })
    }
    target_dim = sapply(idx, length)
    if(prod(target_dim) == 0){
      if(drop){
        return(x$`@sample_data`()[NULL])
      } else {
        return(array(x$`@sample_data`(), dim = target_dim))
      }
      
    }
    
    idx$drop = drop
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
            tmp = rep(tmp, floor(dim[ii] / length(tmp)))
            tmp = tmp[seq_len(dim[ii])]
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
    target_dim = sapply(idx, length)
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
  nms = names(dnams)
  d_env <- new.env(parent = env)
  sel <- lapply(x$dim, function(n){ rep(TRUE, n) })
  names(sel) = nms
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
    fmt[[1]] = quote(`=`)
    tmp_env <- new.env(parent = d_env)
    eval(fmt, envir = tmp_env)
    for(nm in names(tmp_env)){
      if(nm %in% nms){
        if(is.logical(tmp_env[[nm]])){
          sel[[nm]] = sel[[nm]] & tmp_env[[nm]]
        } else{
          stop("Subset formula ", deparse1(fmt), "does not return TRUE/FALSE results")
        }
      }
    }
  }
  
  sel = lapply(sel, which)
  names(sel) <- NULL
  sel$drop = drop
  
  do.call(x$`@get_data`, sel)
  
}
