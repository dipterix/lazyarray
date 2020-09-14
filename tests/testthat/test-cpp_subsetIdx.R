test_that("parseSlices with NAs", {
  dim = c(4, 9, 2)
  a <- function(...){
    parseSlices(environment(), dim, FALSE)
  }
  
  missing_arg = structure(formals(function(a){}), names = NULL)
  
  e <- a(c(0:3,NA))
  expect_equal(e$subset_mode, 1)
  expect_equal(e$expected_length, 4)
  expect_length(e$location_indices, 1)
  expect_equal(e$location_indices[[1]], c(1:3,NA))
  expect_false(e$negative_subscript[[1]])
  
  
  e <- a(-c(0:3,NA))
  expect_equal(e$subset_mode, 1)
  expect_equal(e$expected_length, prod(dim) - 3)
  expect_length(e$location_indices, 1)
  expect_equal(e$location_indices[[1]], structure(c(1:3)))
  expect_true(e$negative_subscript[[1]])
  
  
  e <- a(c(3:0,NA), c(3:0,NA), -c(3:0,NA))
  expect_equal(e$subset_mode, 0)
  expect_equal(range(e$target_dimension - c(4,4,0)), c(0,0))
  expect_equal(e$expected_length, 0)
  expect_length(e$location_indices, 3)
  expect_equal(e$location_indices[[1]], structure(c(3:1,NA)))
  expect_equal(e$location_indices[[2]], structure(c(3:1,NA)))
  expect_equal(e$location_indices[[3]], structure(c(1:2)))
  expect_equal(e$negative_subscript, c(FALSE, FALSE, TRUE))
  
  e <- a(-c(3:0,NA), , -c(3:0,NA))
  expect_equal(e$subset_mode, 0)
  expect_equal(range(e$target_dimension - c(1,9,0)), c(0,0))
  expect_equal(e$expected_length, 0)
  expect_length(e$location_indices, 3)
  expect_equal(e$location_indices[[1]], structure(1:3))
  expect_equal(e$location_indices[[2]], missing_arg[[1]])
  expect_equal(e$location_indices[[3]], structure(c(1:2)))
  expect_equal(e$negative_subscript, c(TRUE, FALSE, TRUE))
  
})

test_that("parseSlices mode: 0", {
  dim = c(4, 9, 2)
  a <- function(...){
    parseSlices(environment(), dim, FALSE)
  }
  
  missing_arg = structure(formals(function(a){}), names = NULL)
  
  # 1: x[1:2, 2:4, 2:1]
  e <- a(1:2, 2:4, 2:1)
  
  expect_equal(e$subset_mode, 0)
  expect_equal(range(e$target_dimension - c(2,3,2)), c(0,0))
  expect_equal(e$expected_length, 12)
  expect_length(e$location_indices, 3)
  expect_equal(e$location_indices[[1]], structure(c(1:2)))
  expect_equal(e$location_indices[[2]], structure(c(2:4)))
  expect_equal(e$location_indices[[3]], structure(c(2:1)))
  expect_false(any(e$negative_subscript))
  
  # 2: x[-(1:2), , 2:1]
  e <- a(-(1:2), , 2:1)
  expect_equal(e$subset_mode, 0)
  expect_equal(range(e$target_dimension - c(2,9,2)), c(0,0))
  expect_equal(e$expected_length, 4*9)
  expect_length(e$location_indices, 3)
  expect_equal(e$location_indices[[1]], structure(c(1:2)))
  expect_equal(e$location_indices[[2]], missing_arg[[1]])
  expect_equal(e$location_indices[[3]], structure(c(2:1)))
  expect_equal(e$negative_subscript, c(TRUE, FALSE, FALSE))
  
  
  # 3: x[, -(20:1), ]
  e <- a(, -(20:1), 0:2)
  expect_equal(e$subset_mode, 0)
  expect_equal(range(e$target_dimension - c(4,0,2)), c(0,0))
  expect_equal(e$expected_length, 0)
  expect_length(e$location_indices, 3)
  expect_equal(e$location_indices[[1]], missing_arg[[1]])
  expect_equal(e$location_indices[[2]], structure(1:9))
  expect_equal(e$location_indices[[3]], structure(c(1:2)))
  expect_equal(e$negative_subscript, c(FALSE, TRUE, FALSE))
  
  # 4: x[0, -(1:20), 0]
  e <- a(0, -(0:20), 0)
  expect_equal(e$subset_mode, 0)
  expect_equal(range(e$target_dimension - c(0,0,0)), c(0,0))
  expect_equal(e$expected_length, 0)
  expect_length(e$location_indices, 3)
  expect_length(e$location_indices[[1]], 0)
  expect_equal(e$location_indices[[2]], structure(c(1:9)))
  expect_length(e$location_indices[[3]], 0)
  expect_equal(e$negative_subscript, c(FALSE, TRUE, FALSE))
  
  e <- a(-(0:2), -(20:1), 0:2)
  expect_equal(e$subset_mode, 0)
  expect_equal(range(e$target_dimension - c(2,0,2)), c(0,0))
  expect_equal(e$expected_length, 0)
  expect_length(e$location_indices, 3)
  expect_equal(e$location_indices[[1]], structure(c(1:2)))
  expect_equal(e$location_indices[[2]], structure(c(1:9)))
  expect_equal(e$location_indices[[3]], structure(c(1:2)))
  expect_equal(e$negative_subscript, c(TRUE, TRUE, FALSE))
  
  e <- a(, , )
  expect_equal(e$subset_mode, 0)
  expect_equal(range(e$target_dimension - dim), c(0,0))
  expect_equal(e$expected_length, prod(dim))
  expect_length(e$location_indices, 3)
  expect_equal(e$location_indices[[1]], missing_arg[[1]])
  expect_equal(e$location_indices[[2]], missing_arg[[1]])
  expect_equal(e$location_indices[[3]], missing_arg[[1]])
  expect_equal(e$negative_subscript, c(FALSE, FALSE, FALSE))
  
  expect_error(a(, ))
  expect_error(a(, ,1:100))
  expect_error(a(1:100, ,))
  expect_error(a(100, ,1:100))
  
})


test_that("parseSlices mode: 2, 1", {
  dim = c(4, 9, 2)
  a <- function(...){
    parseSlices(environment(), dim, FALSE)
  }
  
  expect_error(a(-1:1))
  
  # 1: x[]
  e <- a()
  
  expect_equal(e$subset_mode, 2)
  expect_equal(range(e$target_dimension - dim), c(0,0))
  expect_equal(e$expected_length, prod(dim))
  expect_length(e$location_indices, 1)
  expect_false(any(e$negative_subscript))
  
  # 2: x[1], x[-1], x[0], x[-100], x[100]
  
  e <- a(1)
  expect_equal(e$subset_mode, 1)
  expect_equal(e$expected_length, 1)
  expect_length(e$location_indices, 1)
  expect_true(1 == e$location_indices[[1]])
  expect_false(any(e$negative_subscript))
  
  
  e <- a(-1)
  expect_equal(e$subset_mode, 1)
  expect_equal(e$expected_length, prod(dim) - 1)
  expect_length(e$location_indices, 1)
  expect_true(1 == e$location_indices[[1]])
  expect_equal(e$negative_subscript[[1]], TRUE)
  
  e <- a(0)
  expect_equal(e$subset_mode, 1)
  expect_equal(e$expected_length, 0)
  expect_length(e$location_indices, 1)
  expect_length(e$location_indices[[1]], 0)
  
  e <- a(-100)
  expect_equal(e$subset_mode, 1)
  expect_equal(e$expected_length, prod(dim))
  expect_length(e$location_indices, 1)
  expect_length(e$location_indices[[1]], 0)
  expect_equal(e$negative_subscript[[1]], TRUE)
  
  e <- a(100)
  expect_equal(e$subset_mode, 1)
  expect_equal(e$expected_length, 1)
  expect_length(e$location_indices, 1)
  expect_length(e$location_indices[[1]], 1)
  expect_true(is.na(e$location_indices[[1]]))
  expect_equal(e$negative_subscript[[1]], FALSE)
  
})


test_that("parseSlices with NAs - always positive subscript", {
  dim = c(4, 9, 2)
  a <- function(...){
    parseSlices(environment(), dim, TRUE)
  }
  
  missing_arg = structure(formals(function(a){}), names = NULL)
  
  e <- a(c(0:3,NA))
  expect_equal(e$subset_mode, 1)
  expect_equal(e$expected_length, 4)
  expect_length(e$location_indices, 1)
  expect_equal(e$location_indices[[1]], structure(c(1:3,NA)))
  expect_false(e$negative_subscript[[1]])
  
  
  e <- a(-c(0:3,NA))
  expect_equal(e$subset_mode, 1)
  expect_equal(e$expected_length, prod(dim) - 3)
  expect_length(e$location_indices, 1)
  expect_equal(e$location_indices[[1]], structure(c(4:72)))
  expect_false(e$negative_subscript[[1]])
  
  
  e <- a(c(3:0,NA), c(3:0,NA), -c(3:0,NA))
  expect_equal(e$subset_mode, 0)
  expect_equal(range(e$target_dimension - c(4,4,0)), c(0,0))
  expect_equal(e$expected_length, 0)
  expect_length(e$location_indices, 3)
  expect_equal(e$location_indices[[1]], structure(c(3:1,NA)))
  expect_equal(e$location_indices[[2]], structure(c(3:1,NA)))
  expect_equal(e$location_indices[[3]], structure(numeric(0)))
  expect_equal(e$negative_subscript, c(FALSE, FALSE, FALSE))
  
  e <- a(-c(3:0,NA), , -c(3:0,NA))
  expect_equal(e$subset_mode, 0)
  expect_equal(range(e$target_dimension - c(1,9,0)), c(0,0))
  expect_equal(e$expected_length, 0)
  expect_length(e$location_indices, 3)
  expect_equal(e$location_indices[[1]], structure(4))
  expect_equal(e$location_indices[[2]], missing_arg[[1]])
  expect_equal(e$location_indices[[3]], structure(numeric(0)))
  expect_equal(e$negative_subscript, c(FALSE, FALSE, FALSE))
  
})

test_that("parseSlices mode: 0 - always positive subscript", {
  dim = c(4, 9, 2)
  a <- function(...){
    parseSlices(environment(), dim, TRUE)
  }
  
  missing_arg = structure(formals(function(a){}), names = NULL)
  
  # 2: x[-(1:2), , 2:1]
  e <- a(-(1:2), , 2:1)
  expect_equal(e$subset_mode, 0)
  expect_equal(range(e$target_dimension - c(2,9,2)), c(0,0))
  expect_equal(e$expected_length, 4*9)
  expect_length(e$location_indices, 3)
  expect_equal(e$location_indices[[1]], structure(c(3:4)))
  expect_equal(e$location_indices[[2]], missing_arg[[1]])
  expect_equal(e$location_indices[[3]], structure(c(2:1)))
  expect_equal(e$negative_subscript, c(FALSE, FALSE, FALSE))
  
  
  # 3: x[, -(20:1), ]
  e <- a(, -(20:1), 0:2)
  expect_equal(e$subset_mode, 0)
  expect_equal(range(e$target_dimension - c(4,0,2)), c(0,0))
  expect_equal(e$expected_length, 0)
  expect_length(e$location_indices, 3)
  expect_equal(e$location_indices[[1]], missing_arg[[1]])
  expect_equal(e$location_indices[[2]], structure(numeric(0)))
  expect_equal(e$location_indices[[3]], structure(c(1:2)))
  expect_equal(e$negative_subscript, c(FALSE, FALSE, FALSE))
  
  # 4: x[0, -(1:20), 0]
  e <- a(0, -(0:2), 0)
  expect_equal(e$subset_mode, 0)
  expect_equal(range(e$target_dimension - c(0,7,0)), c(0,0))
  expect_equal(e$expected_length, 0)
  expect_length(e$location_indices, 3)
  expect_length(e$location_indices[[1]], 0)
  expect_equal(e$location_indices[[2]], structure(c(3:9)))
  expect_length(e$location_indices[[3]], 0)
  expect_equal(e$negative_subscript, c(FALSE, FALSE, FALSE))
  
  e <- a(-(0:2), -(20:1), 0:2)
  expect_equal(e$subset_mode, 0)
  expect_equal(range(e$target_dimension - c(2,0,2)), c(0,0))
  expect_equal(e$expected_length, 0)
  expect_length(e$location_indices, 3)
  expect_equal(e$location_indices[[1]], structure(c(3:4)))
  expect_equal(e$location_indices[[2]], structure(numeric(0)))
  expect_equal(e$location_indices[[3]], structure(c(1:2)))
  expect_equal(e$negative_subscript, c(FALSE, FALSE, FALSE))
  
  
})
