`[.FileArray` <- function(x, ..., drop = TRUE, reshape = NULL){
  if(!x$is_valid){
    stop("`[.FileArray`: x is no longer valid (data has been removed).")
  }
  if(!is.null(reshape)){
    reshape <- as.numeric(reshape)
    stopifnot(all(reshape>=0))
  }
  drop <- isTRUE(drop)
  # get schedule
  
  parsed <- parseAndScheduleBlocks(environment(), x$dim, TRUE)
  # parsed <- parseAndScheduleBlocks(list(1:10,2:10,3:10,4:10), x$dim, TRUE)
  # parsed <- parseAndScheduleBlocks(list(1,1,1,1), x$dim, TRUE)
  
  if(parsed$subset_mode == 1){
    stop("FstArray does not support single subscript (x[i]), try x[] or x[i,j,k,...]")
  }
  
  re <- array(x$sample_na, parsed$target_dimension)
  
  if(parsed$expected_length == 0){
    reshapeOrDrop(re, reshape, drop)
    return(re)
  }
  
  partition_length <- prod(x$partition_dim())
  
  # x[]
  if(parsed$subset_mode == 2){
    
    blocksize <- partition_length
    blocksize[[length(blocksize)]] <- 1
    blocksize <- prod2(blocksize)
    
    # copy all to re inplace
    for(ii in seq_len(x$npart)){
      if(x$has_partition(ii)){
        sub <- x$get_partition_data(ii)
        subsetAssignVector(re, blocksize * (ii-1) + 1, sub)
      }
    }
  } else {
    # x[i,j,k]
    loc <- parsed$location_indices
    if(!is.numeric(loc[[4]])){
      # missing, all partitions
      partitions <- seq_len(x$npart)
    } else {
      partitions <- loc[[4]]
    }
    # check if the schedule is made
    schedule <- parsed$schedule
    block_ndims <- schedule$block_ndims
    
    ptr <- 1
    blocksize <- schedule$block_expected_length
    
    for(file_ii in partitions){
      # No file, NA
      if(!x$has_partition(file_ii)){
        ptr = ptr + blocksize * schedule$schedule_counts_per_part
        next
      }
      
      file <- x$get_partition_fpath(file_ii, full_path = TRUE, type = 'combined')
      ptr_file <- filematrix::fm.open(file)
      
      if(schedule$block_indexed){
        # file exists
        for(schedule_ii in schedule$schedule_index){
          row_number <- blocksize * (schedule_ii-1) + schedule$block_schedule
          subsetAssignVector(re, ptr, ptr_file[row_number, 1])
          ptr = ptr + blocksize
        }
      } else {
        # ndim == 2
        row_number <- loc[[1]]
        if(is.numeric(row_number)){
          # x[i,j]
          buffer <- ptr_file[row_number, 1]
        } else {
          # x[i,j]
          buffer <- ptr_file[, 1]
        }
        subsetAssignVector(re, ptr, buffer)
        ptr = ptr + length(buffer)
      }
      
      
    }
    
  }
  
  reshapeOrDrop(re, reshape, drop)
  re
}
