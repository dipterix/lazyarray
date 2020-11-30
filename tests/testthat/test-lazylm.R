if(interactive()){
  require(testthat)
  require(lazyarray)
  require(stats)
}

test_that("lazylm", {
  
  # use a baseenv to evaluate
  env <- new.env(parent = asNamespace('lazyarray'))
  with(env, {
    lazylm( V1 ~ .-V2-1 + (V2 > 0), data = local({
      arr <- array(rnorm(72), c(6,3,4))
      arr[1,1,1] <- NA 
      as.lazyarray(arr, type = 'file')
    }), weights = runif(18), offset = rnorm(18) , na.action = 'na.exclude')
  })
  
  
  arr <- array(rnorm(72), c(6,3,4))
  arr[1,1,1] <- NA        # Allow NA to be treated
  offset = rnorm(18)    # offsets and weights are optional
  weights = runif(18)
  na_action <- 'na.omit'

  formula <- V1 ~ .-V2-1 + (V2 > 0)

  data <- as.lazyarray(arr, type = 'file')
  
  dim(arr) <- c(18, 4)
  lm_data <- as.data.frame(arr)
  
  e <- eigen(crossprod(arr[complete.cases(arr), ]))
  # if(!min(e$values > 1e-7)){
  #   skip("Generated eigenvalue for lazylm is too small, just skip this time")
  # }
  
  object <- lazylm(formula, data, weights = weights, offset = offset, na.action = na_action, fitted = TRUE)


  # Compare to stats::lm
  flm <- lm(formula, lm_data, weights = weights, offset = offset, na.action = na_action)
  
  assign('object', object, envir = globalenv())
  assign('flm', flm, envir = globalenv())

  expect_lt(max(abs(coef(object) - coef(flm))), 1e-7)
  expect_lt(max(abs(resid(object) - resid(flm))), 1e-7)
  expect_lt(max(abs(fitted(object) - fitted(flm))), 1e-7)
  
  s1 <- summary(object)
  s2 <- summary(flm)
  
  expect_lt(max(abs(s1$coefficients - s2$coefficients)), 1e-7)
  
  expect_lt(max(abs(s1$r.squared - s2$r.squared)), 1e-7)
  expect_equivalent(s1$df, s2$df)
  
})
