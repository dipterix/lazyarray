
context("Getter/Setter mode = 2")

test_that("Getter/Setter mode = 2", {
  path <- tempfile()
  self <- create_lazyarray(path, 'double', c(1,3,4), multipart = TRUE, multipart_mode = 2)
  private <- self$.__enclos_env__$private
  value <- array(1:4, c(1,2,2))
  
  expect_true(self$can_write)
  
  self$`@set_data`(value, 1.0, 1:2, 1:2)
  
  expect_equal(self$`@get_data`(1, 2, 1), 2)
  
  expect_equal(self$`@get_data`(1, 1:3, 1), c(1,2,NA))
  
  expect_identical(self[drop = FALSE], array(c(1,2,NA, 3,4,NA, rep(NA, 6)), c(1,3,4)))
  
  self[1, 1:2, c(3,10)] <- 1:4
  
  # change to read_only
  private$read_only <- TRUE
  expect_false(self$can_write)
  
  expect_identical(self[drop = FALSE], array(c(1,2,NA, 3,4,NA, 1,2,NA, rep(NA, 3)), c(1,3,4)))
  
  expect_error({self[1, 1:2, c(3,10)] <- 1:4})
  
  # clean up
  self$remove_data()
  
  expect_false(dir.exists(private$.dir))
  
  # operations will result in error
  expect_error(self[1,1,1])
  
})




context("Getter/Setter mode = 1")

test_that("Getter/Setter mode = 1", {
  
  self <- create_lazyarray(tempfile(), 'double', c(1,3,4), multipart = TRUE, multipart_mode = 1)
  private <- self$.__enclos_env__$private
  value <- array(1:4, c(1,2,2))
  
  expect_true(self$can_write)
  
  self$`@set_data`(value, 1.0, 1:2, 1:2)
  
  expect_equal(self$`@get_data`(1, 2, 1), 2)
  
  expect_equal(self$`@get_data`(1, 1:3, 1), c(1,2,NA))
  
  expect_identical(self[drop = FALSE], array(c(1,2,NA, 3,4,NA, rep(NA, 6)), c(1,3,4)))
  
  self[1, 1:2, c(3,10)] <- 1:4
  
  # change to read_only
  private$read_only <- TRUE
  expect_false(self$can_write)
  
  expect_identical(self[drop = FALSE], array(c(1,2,NA, 3,4,NA, 1,2,NA, rep(NA, 3)), c(1,3,4)))
  
  expect_error({self[1, 1:2, c(3,10)] <- 1:4})
  
  # clean up
  self$remove_data()
  
  expect_false(dir.exists(private$.dir))
  
  # operations will result in error
  expect_error(self[1,1,1])
  
})





context("Getter/Setter no partition")

test_that("Getter/Setter no partition", {
  
  self <- create_lazyarray(tempfile(), 'double', c(1,3,4), multipart = FALSE)
  private <- self$.__enclos_env__$private
  value <- array(1:4, c(1,2,2))
  
  expect_true(self$can_write)
  
  self$`@set_data`(value, 1.0, 1:2, 1:2)
  
  expect_equal(self$`@get_data`(1, 2, 1), 2)
  
  expect_equal(self$`@get_data`(1, 1:3, 1), c(1,2,NA))
  
  expect_identical(self[drop = FALSE], array(c(1,2,NA, 3,4,NA, rep(NA, 6)), c(1,3,4)))
  
  expect_error({
    self[1, 1:2, c(3,10)] <- 1:4
  })
  
  # change to read_only
  private$read_only <- TRUE
  expect_false(self$can_write)
  
  expect_identical(self[drop = FALSE], array(c(1,2,NA, 3,4,NA, rep(NA, 6)), c(1,3,4)))
  
  expect_error({self[1, 1:2, 3] <- 1:2})
  
  # clean up
  self$remove_data()
  
  expect_false(dir.exists(private$.dir))
  
  # operations will result in error
  expect_error(self[1,1,1])
  
})





