test_that("multiplication works", {
  x <-
    lazyarray::lazyarray(tempfile(),
                         dim = c(100, 100),
                         storage_format = 'double')
  x[, 1] <- 1:100
  x[, 2] <- NA
  x[, 3] <- c(0, rep(NA, 99))
  
  
  testthat::expect_true(is.na(min(x)))
  testthat::expect_true(is.na(max(x)))
  
  testthat::expect_equal(min(x, na.rm=TRUE), 0)
  testthat::expect_equal(max(x, na.rm=TRUE), 100)
  testthat::expect_equal(range(x, na.rm=TRUE), c(0, 100))
  testthat::expect_equal(range(x), c(NA, NA))
  
  testthat::expect_equivalent(mean(x, na.rm = TRUE), 50)
  testthat::expect_equivalent(sum(x, na.rm = TRUE), 5050)
  
  # partition_apply(x, mean)
  
})
