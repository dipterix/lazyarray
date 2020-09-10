require(testthat)
test_that("parseAndScheduleBlocks", {
  
  # normal
  dim <- c(5,7,8,10)
  res <- parseAndScheduleBlocks(list(1:5,1,2:3,4:5), )
  re <- res$schedule
  expect_equal(re$dimension, dim)
  expect_equivalent(re$partition_index, 4:5)
  
})
