#' @noRd
#' @title Internal Class definition for \code{FstArray}
#' @author Zhengjia Wang
#' @description Internal class definition of lazy array objects
FileArray <- R6::R6Class(
  classname = "FileArray",
  portable = TRUE,
  inherit = AbstractLazyArray,
  public = list(
    print = function(...){
      cat("<FileArray> (", private$.storage_format, ')\n', sep = '')
      cat('Dimension:\t', paste(sprintf('%d ', private$.dim), collapse = 'x '), '\n')
      cat('File format: bmat\n')
      invisible(self)
    },
    
    initialize = function(path, dim, storage_format = c('double', 'integer'),
                          read_only = TRUE, meta_name = 'lazyarray.meta'){
      private$.file_format <- "bmat"
      storage_format <- match.arg(storage_format)
      if(missing(dim)){
        super$initialize(path = path, storage_format = storage_format, 
                         read_only = read_only, meta_name = meta_name)
      } else {
        super$initialize(path = path, dim = dim, storage_format = storage_format, 
                         read_only = read_only, meta_name = meta_name)
      }
      # cannot convert types
      if(length(self$raw_meta$storage_format) && storage_format != self$raw_meta$storage_format){
        stop("Data format inconsistent (cannot type-convert). Header info: ", 
             self$raw_meta$storage_format, "; provided: ", storage_format)
      }
    },
    
    get_partition_fpath = function(part, full_path = TRUE, summary_file = FALSE, type = c('data', 'desc', 'combined')){
      type <- match.arg(type)
      if(missing(part)){
        part <- seq_len(self$npart)
      } else {
        part <- as.integer(part)
        if(base::anyNA(part) || any(part <= 0)){
          stop("partition number must be all positive: ", part)
        }
      }
      res <- sprintf('%s%s', part, self$get_file_format())
      if(full_path){
        res <- file.path(private$.path, res)
      }
      if(summary_file){
        res <- sprintf('%s.summary', res)
        return(res)
      }
      if(type == 'desc'){
        res <- sprintf('%s.desc.txt', part)
      } else if(type == 'combined'){
        res <- sprintf('%s', part)
      } else {
        res <- sprintf('%s%s', part, self$get_file_format())
      }
      if(full_path){
        res <- file.path(private$.path, res)
      }
      res
    },
    
    has_partition = function(part){
      stopifnot(length(part) == 1)
      file <- self$get_partition_fpath(part, full_path = TRUE)
      desc <- self$get_partition_fpath(part, full_path = TRUE, type = 'desc')
      if(file.exists(file) && file.exists(desc)){
        return(TRUE)
      }
      return(FALSE)
    },
    initialize_partition = function(part, nofill = FALSE){
      if(!self$has_partition(part)){
        file <- self$get_partition_fpath(part, full_path = TRUE, type = 'combined')
        ptr <- filematrix::fm.create(file, self$partition_length, 1, type = self$storage_format)
        if(!nofill){
          ptr[] <- rep(self$sample_na, self$partition_length)
        }
        filematrix::close(ptr)
        return(TRUE)
      }
      return(FALSE)
    },
    get_partition_data = function(part, reshape = NULL){
      if(self$has_partition(part)){
        file <- self$get_partition_fpath(part, full_path = TRUE, type = 'combined')
        ptr <- filematrix::fm.open(file)
        re <- ptr[]
        filematrix::close(ptr)
        if(is.null(reshape)){
          reshapeOrDrop(re, reshape = self$partition_dim(), drop = FALSE)
        } else {
          reshapeOrDrop(re, reshape = as.numeric(reshape), drop = FALSE)
        }
        
        return(re)
      } else {
        array(self$sample_na, self$partition_dim())
      }
    }
  )
)


#' @export
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
  
  parsed <- parseAndScheduleBlocks2(environment(), x$dim, TRUE)
  # parsed <- parseAndScheduleBlocks2(list(1:10,2:10,3:10,4:10), x$dim, TRUE)
  # parsed <- parseAndScheduleBlocks2(list(1,1,1,1), x$dim, TRUE)
  
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
    if(!is.numeric(loc[[x$ndim]])){
      # missing, all partitions
      partitions <- seq_len(x$npart)
    } else {
      partitions <- loc[[x$ndim]]
    }
    # check if the schedule is made
    schedule <- parsed$schedule
    block_ndims <- schedule$block_ndims
    
    ptr <- 1
    blocksize <- schedule$block_expected_length
    
    
    for(file_ii in partitions){
      # No file, NA
      if(is.na(file_ii) || !x$has_partition(file_ii)){
        ptr = ptr + blocksize * schedule$schedule_counts_per_part
        next
      }
      
      file <- x$get_partition_fpath(file_ii, full_path = TRUE, type = 'combined')
      ptr_file <- filematrix::fm.open(file)
      
      if(schedule$block_indexed){
        # file exists
        for(schedule_ii in schedule$schedule_index){
          if(!is.na(schedule_ii)){
            row_number <- blocksize * (schedule_ii-1) + schedule$block_schedule
            row_number[row_number < 0] <- NA
            if(length(row_number)){
              subsetAssignVector(re, ptr, ptr_file[row_number, 1])
            }
          }
          ptr = ptr + blocksize
        }
      } else {
        # ndim == 2
        row_number <- loc[[1]]
        buffer <- tryCatch({
          row_number[row_number < 0] <- NA
          ptr_file[row_number, 1]
        }, error = function(e){
          ptr_file[, 1]
        })
        subsetAssignVector(re, ptr, buffer)
        ptr = ptr + length(buffer)
      }
      filematrix::close(ptr_file)
      
    }
    
  }
  
  reshapeOrDrop(re, reshape, drop)
  re
}

# `[.FileArray` <- function(x, ..., drop = TRUE, reshape = NULL){
#   
#   parsed <- parseSlices(environment(), dim = x$dim, pos_subscript = TRUE)
#   if(!is.null(reshape)){
#     reshape <- as.numeric(reshape)
#     stopifnot(prod(reshape) != parsed$expected_length)
#   }
#   if(parsed$subset_mode == 1){
#     stop("Single subscript (x[i]) is unsupported. Try using x[] or x[i,j,k,..]")
#   }
#   
#   if(parsed$subset_mode == 2){
#     re <- array(x$sample_na, dim = x$dim)
#     file_ii = 1
#     ptr_re = 1
#     block_size = x$partition_length
#     while(file_ii <= x$npart){
#       # check file 
#       if(x$has_partition(file_ii)){
#         subsetAssignVector(re, ptr_re, x$get_partition_data(file_ii))
#       }
#       ptr_re <- ptr_re + block_size
#       file_ii = file_ii + 1
#     }
#     return(re)
#   }
#   
#   
#   partitions <- parsed$location_indices[[x$ndim]]
#   sub_loc <- parsed$location_indices
#   sub_loc[[x$ndim]] <- 1
#   re_dim <- parsed$target_dimension
#   
#   re <- array(x$sample_na, dim = re_dim)
#   block_size <- re_dim
#   block_size[[x$ndim]] <- 1
#   block_size <- prod(block_size)
#   ptr_re <- 1
#   v <- NULL
#   
#   call <- as.call(c(list(quote(`[`), quote(v)), sub_loc))
#   
#   if(!is.numeric(partitions)){
#     file_ii = 1
#     while(file_ii <= x$npart){
#       
#       # check file 
#       if(x$has_partition(file_ii)){
#         
#         v <- x$get_partition_data(file_ii)
#         sub <- eval(call)
#         rm(v)
#         subsetAssignVector(re, ptr_re, sub)
#         
#       }
#       ptr_re <- ptr_re + block_size
#       file_ii = file_ii + 1
#     }
#   } else {
#     for(file_ii in partitions){
#       
#       # check file 
#       if(x$has_partition(file_ii)){
#         
#         v <- x$get_partition_data(file_ii)
#         sub <- eval(call)
#         rm(v)
#         subsetAssignVector(re, ptr_re, sub)
#         
#       }
#       ptr_re <- ptr_re + block_size
#     }
#   }
#   drop <- isTRUE(drop)
#   
#   reshapeOrDrop(re, reshape, drop)
#   re
# }

#' @export
`[<-.FileArray` <- function(x, ..., value){
  if(!x$is_valid){
    stop("`[<-.FileArray`: x is no longer valid (data has been removed).")
  }
  if(!x$can_write){
    stop("`[<-.FileArray`: x is read-only")
  }
  
  parsed <- parseAndScheduleBlocks2(environment(), x$dim, TRUE)
  # parsed <- parseAndScheduleBlocks2(list(1:10,2:10,3:10,4:10), x$dim, TRUE)
  # parsed <- parseAndScheduleBlocks2(list(1,1,1,1), x$dim, TRUE)
  
  if(parsed$subset_mode == 1){
    stop("FstArray does not support single subscript (x[i]<-v), try x[]<-v or x[i,j,k,...]<-v")
  }
  partition_length <- prod(x$partition_dim())
  
  # x[]
  if(parsed$subset_mode == 2){
    value <- array(value, dim = x$dim)
    fake_idx <- lapply(x$dim, function(x){ get_missing_value() })
    slice_value <- function(ii){
      fake_idx[[x$ndim]] <- ii
      do.call(`[`, c(list(quote(value)), fake_idx))
    }
    # copy all to re inplace
    for(ii in seq_len(x$npart)){
      x$initialize_partition(part = ii, nofill = TRUE)
      file <- x$get_partition_fpath(ii, full_path = TRUE, type = 'combined')
      ptr_file <- filematrix::fm.open(file)
      ptr_file[] <- slice_value(ii)
      filematrix::close(ptr_file)
    }
  } else {
    # x[i,j,k]
    loc <- parsed$location_indices
    if(!is.numeric(loc[[x$ndim]])){
      # missing, all partitions
      partitions <- seq_len(x$npart)
    } else {
      partitions <- loc[[x$ndim]]
    }
    # check if the schedule is made
    schedule <- parsed$schedule
    block_ndims <- schedule$block_ndims
    
    ptr <- 1
    blocksize <- schedule$block_expected_length
    
    if(schedule$block_indexed){
      value <- array(value, dim = c(blocksize, length(schedule$schedule_index), length(partitions)))
    } else {
      
      value <- array(value, dim = c(parsed$expected_length / length(partitions), length(partitions)))
    }
    
    for(ff in seq_along(partitions)){
      file_ii <- partitions[[ff]]
      # No file, NA
      x$initialize_partition(part = file_ii)
      file <- x$get_partition_fpath(file_ii, full_path = TRUE, type = 'combined')
      ptr_file <- filematrix::fm.open(file, readonly = FALSE)
      
      if(schedule$block_indexed){
        # file exists
        for(ii in seq_along(schedule$schedule_index)){
          schedule_ii <- schedule$schedule_index[[ii]]
          row_number <- blocksize * (schedule_ii-1) + schedule$block_schedule
          sel <- row_number > 0
          ptr_file[row_number[sel], 1] <- value[sel,ii,ff]
        }
      } else {
        # ndim == 2
        row_number <- loc[[1]]
        tryCatch({
          sel <- row_number > 0
          ptr_file[row_number[sel], 1] <- value[sel,ff]
        }, error = function(e){
          ptr_file[, 1] <- value[,ff]
        })
      }
      filematrix::close(ptr_file)
      
    }
    
  }
  
  invisible(x)
}

