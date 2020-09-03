
#' Generate partition summary statistics for array objects along the last 
#' dimension
#' @param x An array or \code{LazyArray}
#' @param na.rm whether to remove \code{NA} when calculating summary statistics
#' @param ... passed to other methods or ignored
#' @return A data frame with the flowwing possible columns: \code{Min}, 
#' \code{Max}, \code{Mean}, \code{Standard Deviation}, \code{NAs} (total number
#'  of \code{NA}), and \code{Length}.
#' @name partition_table
#' @examples 
#' 
#' # R array
#' x <- array(1:27, c(3,3,3))
#' partition_table(x)
#' 
#' # LazyArray
#' x <- lazyarray(tempfile(), 'double', c(3,3,3))
#' x[] <- 1:27
#' partition_table(x)
#' 
#' @export
partition_table <- function(x, na.rm = FALSE, ...){
  UseMethod('partition_table')
}

#' @rdname partition_table
#' @export
partition_table.array <- function(x, na.rm = FALSE, ...){
  dim <- dim(x)
  suppressWarnings({
    smry <- t(apply(x, length(dim), function(d){
      c(
        min(d, na.rm = na.rm),
        max(d, na.rm = na.rm),
        mean(d, na.rm = na.rm),
        sd(d, na.rm = na.rm),
        sum(is.na(d)),
        length(d)
      )
    }))
  })
  smry <- as.data.frame(smry)
  names(smry) <- c('Min', 'Max', 'Mean', 'Standard Deviation', 'NAs', 'Length')
  smry
}

#' @rdname partition_table
#' @export
partition_table.LazyArray <- function(x, na.rm = FALSE, ...){
  smry <- summary(x)
  re <- smry$partitions
  re$Count <- re$Length - re$NAs
  re
}


#' Apply function along the last dimension of an array and aggregate the results
#' @name partition_map
#' @param x R array or \code{LazyArray}
#' @param map_fun function that takes in a slice of array and an optional 
#' argument indicating current partition number
#' @param reduce function that accept a list of results returned by 
#' \code{map_fun}, can be missing
#' @param partitions integers of partitions, i.e. the slices of array to be 
#' applied to, can be missing. If missing, then applies to all partitions
#' @param ... internally used
#' @return If \code{reduce} is missing, returns a list of results. Each result
#' is returned by \code{map_fun}, and the total length equals to number of 
#' partitions mapped. If \code{reduce} is a function, that list of results will
#' be passed to \code{reduce} and \code{partition_map} returns the results 
#' generated from \code{reduce}.
#' @examples 
#' 
#' # -------------------------- Ordinary R array ---------------------------
#' 
#' x <- array(1:24, c(2,3,4))
#' partition_map(x, function(slice, part){
#'   sum(slice)
#' })
#' 
#' # When reduce and partitions are missing, the following code is equivalent
#' as.list(apply(x, 3, sum))
#' 
#' # When reduce is present
#' partition_map(x, function(slice, part){
#'   sum(slice)
#' }, function(slice_sum){
#'   max(unlist(slice_sum))
#' })
#' 
#' # equivalently, we could call
#' slice_sum <- partition_map(x, function(slice, part){
#'   sum(slice)
#' })
#' max(unlist(slice_sum))
#' 
#' # When partition is specified
#' # Partition 1, 2, and 4 exist but 5 is missing
#' # when a partition is missing, the missing slice will be NA
#' partition_map(x, function(slice, part){
#'   sum(slice)
#' }, partitions = c(1,2,4,5))
#' 
#' # -------------------------- LazyArray ---------------------------
#' x <- lazyarray(tempfile(), 'complex', c(2,3,4))
#' x[] <- 1:24 + (24:1) * 1i
#' 
#' partition_map(x, function(slice, part){
#'   slice[1, ,] * slice[2, ,]
#' }, reduce = function(mapped_prod){
#'   mean(unlist(mapped_prod))
#' })
#' 
#' 
#' 
#' @export
partition_map <- function(x, map_fun, reduce, partitions, ...){
  UseMethod('partition_map')
}

#' @export
partition_map.array <- function(x, map_fun, reduce, partitions, ...){
  if(length(formals(map_fun)) == 1){
    mfun <- function(x, part){
      map_fun(x)
    }
  } else {
    mfun <- map_fun
  }
  dim <- dim(x)
  available_partitions <- seq_len(dim[[length(dim)]])
  substr <- paste(rep('', length(dim)), collapse = ',')
  
  if(missing(partitions)){
    partitions <- available_partitions
  }
  
  res <- lapply(partitions, function(part){
    if(part %in% available_partitions){
      slice <- eval(parse(text = sprintf('x[%s%d,drop=FALSE]', substr, part)))
    } else {
      slice <- array(NA, c(dim[-length(dim)], 1))
    }
    mfun(slice, part)
  })
  
  if(!missing(reduce) && is.function(reduce)){
    res <- reduce(res)
  }
  
  res
}

#' @export
partition_map.LazyArray <- function(x, map_fun, reduce, partitions, further_split = FALSE, ...){
  if(missing(partitions)){
    partitions <- seq_len(x$npart)
  } else {
    partitions <- as.integer(partitions)
    partitions <- partitions[partitions > 0 & partitions <= x$npart]
  }
  
  if(length(formals(map_fun)) == 1){
    mfun <- function(x, part){
      map_fun(x)
    }
  } else {
    mfun <- map_fun
  }
  
  mapped <- x$`@partition_map`(
    partitions,
    map_function = mfun,
    split = FALSE, ...
  )
  if(!missing(reduce)){
    
    reduce(mapped)
  } else {
    return(mapped)
  }
  
}

