require(testthat)
test_that("parseAndScheduleBlocks", {
  
  # normal
  dim <- c(5,7,8,10)
  slice <- list(1:5,1,2:3,4:5)
  slice_dim <- sapply(slice, length)
  ndims <- length(dim)
  res <- parseAndScheduleBlocks(slice, dim)
  re <- res$schedule
  expect_equal(re$dimension, dim)
  
  # The last slice
  expect_equivalent(as.numeric(re$partition_index), slice[[ndims]])
  expect_equal(re$schedule_count, length(slice[[ndims]]))
  # [5,7,8] x [1,1] x [8]
  expect_equal(re$block_ndims, 3)
  expect_equal(re$schedule_dimension, prod(sapply(slice[seq_len(re$block_ndims)], length)))
  expect_equal(re$schedule_index * re$schedule_dimension * length((re$partition_index)), prod(slice_dim))
  
  expect_equal(re$block_dimension, dim[seq_len(re$block_ndims)])
  
  expected_block_prod_dim = c(1, cumprod(dim[seq_len(re$block_ndims-1)]))
  expect_equal(re$block_prod_dim, expected_block_prod_dim)
  # within-group schedule
  
  tmp <- lapply(seq_len(re$block_ndims), function(ii){
    (slice[[ii]]-1) * expected_block_prod_dim[[ii]]
  })
  scheduled_idx <- Reduce(function(a,b){outer(a,b,'+')}, tmp) + 1
  expect_equal(re$block_schedule, as.vector(scheduled_idx))
  
  expect_equal(re$block_schedule_start, min(re$block_schedule))
  expect_equal(re$block_schedule_end, max(re$block_schedule))
  expect_equal(re$block_length, prod(dim[1:re$block_ndims]))
  
})
