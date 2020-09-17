require(testthat)

# devtools::load_all()
dim <- c(5,7,8,10)
slice <- list(c(1,1,2),1,2:3,4:5)

check_schedule <- function(slice, dim){
  slice_copy <- lapply(slice, I); force(slice_copy)
  res <- parseAndScheduleBlocks2(slice_copy, dim)
  re <- res$schedule
  block_lb <- getLazyBlockSize()
  block_ub <- 31250000
  ndims <- length(dim)
  
  valid_slices <- lapply(seq_along(slice), function(ii){
    x <- slice[[ii]]
    if(isTRUE(x == get_missing_value())){
      x <- seq_len(dim[[ii]])
    } else {
      x <- x[is.na(x) | x > 0]
    }
    x
  })
  
  get_missing_value()
  
  # redo this procedure in R
  cum_dim <- cumprod(dim)
  block_ndims <- which(cum_dim > block_lb)
  if(!length(block_ndims)){ block_ndims <- ndims }
  block_ndims <- block_ndims[[1]]
  if( block_ndims == ndims ){ block_ndims = block_ndims - 1 }
  
  # [3000,7,3] if buffer_margin=1 => [3000] x [7,1] x [3]
  # [3000,7,3] if buffer_margin=2 => [3000,7] x [1,1] x [3]
  # [100,100,100,100,1] buffer_margin=2 => [100,100] x [100,100,1] x [1]
  block_dimension <- dim[seq_len(block_ndims)]
  schedule_dimension <- dim[-seq_len(block_ndims)]
  partition_index <- slice[[ndims]]
  partition_index <- partition_index[is.na(partition_index) | partition_index > 0]
  partition_counts <- length(partition_index)
  
  # make schedules
  block_expected_length <- prod(sapply(valid_slices[seq_len(block_ndims)], length))
  schedule_counts_per_part <- prod(sapply(valid_slices, length)) / partition_counts / block_expected_length
  block_indexed <- FALSE
  if(block_ndims >= 2 && block_expected_length <= block_ub){
    block_schedule <- loc2idx3(valid_slices[seq_len(block_ndims)], dim[seq_len(block_ndims)])
    block_indexed <- TRUE
    
    
    expect_equal(re$block_schedule, block_schedule)
    block_schedule[block_schedule < 0] <- NA
    expect_equal(re$block_schedule_start, min(block_schedule, na.rm = TRUE))
    expect_equal(re$block_schedule_end, max(block_schedule, na.rm = TRUE))
    expect_equal(re$block_expected_length, block_expected_length)
  } else {
    # do we need accurate block_expected_length? as it's not actually used if not block indexed
    # block_expected_length <- prod(dim[seq_len(block_ndims)])
    block_schedule_start <- 1
    block_schedule_end <- prod(dim[seq_len(block_ndims)])
    expect_equal(re$block_schedule_start, block_schedule_start)
    expect_equal(re$block_schedule_end, block_schedule_end)
    expect_equal(re$block_expected_length, block_expected_length)
  }
  
  block_prod_dim <- c(1, cumprod(block_dimension))[seq_along(block_dimension)]
  block_length <- prod(block_dimension)
  
  # schedule blocks
  schedule_dimension_alt <- schedule_dimension
  schedule_dimension_alt[[length(schedule_dimension_alt)]] <- 1
  tmp <- valid_slices[-seq_len(block_ndims)]
  tmp[[length(tmp)]] <- 1
  schedule_index <- loc2idx3(tmp, schedule_dimension_alt)
  
  # checks
  expect_equal(re$dimension, dim)
  expect_equal(re$block_ndims, block_ndims)
  expect_equal(re$block_dimension, block_dimension)
  expect_equal(re$schedule_dimension, schedule_dimension)
  # re$partition_index[re$partition_index < -9e18] <- NA_real_
  expect_equal(as.numeric(re$partition_index), partition_index)
  expect_equal(re$partition_counts, partition_counts)
  expect_equal(re$schedule_counts_per_part, schedule_counts_per_part)
  expect_equal(re$block_prod_dim, block_prod_dim)
  expect_equal(re$block_indexed, block_indexed)
  expect_equal(re$block_length, block_length)
  expect_equal(re$schedule_index, schedule_index)
}


context("subset scheduler-normal case")

test_that("subset scheduler-normal case", {
  dim <- c(5,7,8,10)
  slice <- list(c(1,1,2),1,2:3,4:5)
  
  setLazyBlockSize(1)
  check_schedule(slice, dim)
  setLazyBlockSize(30)
  check_schedule(slice, dim)
  setLazyBlockSize(300)
  check_schedule(slice, dim)
  setLazyBlockSize(10000)
  check_schedule(slice, dim)
  
})

context("subset scheduler-NA & 0 cases")
test_that("subset scheduler-NA & 0 cases", {
  dim <- c(5,7,8,10)
  
  slice <- lapply(dim, function(x){
    x <- sample(x, x, replace = TRUE)
    x[sample(length(x), 2)] <- NA
    x[sample(length(x), 1)] <- 0
    x
  })
  
  setLazyBlockSize(1)
  check_schedule(slice, dim)
  setLazyBlockSize(30)
  check_schedule(slice, dim)
  setLazyBlockSize(300)
  check_schedule(slice, dim)
  setLazyBlockSize(10000)
  check_schedule(slice, dim)
})


