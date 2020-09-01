

#' @export
partition_table <- function(x, ...){
  UseMethod('partition_table')
}

#' @export
partition_table.LazyArray <- function(x, na.rm = FALSE, ...){
  smry <- summary(x)
  re <- smry$partitions
  re$Count <- re$Length - re$NAs
  re
}

#' @export
partition_map <- function(x, map_fun, reduce, partitions, ...){
  UseMethod('partition_map')
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


