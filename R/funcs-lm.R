# x <- rnorm(100); dim(x) = c(20,5)
# x <- as.lazymatrix(x)
# colnames(x) <- paste0('V', 1:5)
# rownames(x) <- 1:20
# dimnames(x)
# data = x
# formula <- V1+V3 * V1 ~ (.-V3)*V4
# weights = NULL

#' @name lazy_lm
#' @title Perform linear regression on \code{LazyMatrix} object
#' @param data \code{LazyMatrix} object
#' @param formula R formula
#' @param yidx column indices for response variable
#' @param xidx column indices for predictor
#' @param weights weights of observations
#' @param intercept whether to include intercept
#' @param na.action see \code{\link[stats]{lm}}
#' @param center,scale logical or numeric vectors with length equaling to 
#' data columns
#' @param residuals whether to return residuals
#' @param ... passed to \code{\link{chunk_map}}
#' @return A \code{"lm"} or \code{"mlm"} object
#' 
#' @examples 
#' 
#' a <- matrix(rnorm(100), ncol = 5)
#' x <- as.lazymatrix(a)
#' 
#' 
#' stats_lm <- stats::lm.fit(x = x[,-1], y = x[,1])$coefficients
#' 
#' lazy_lms <- lazy_lm_simple(x, yidx = 1, intercept = FALSE)
#' lazy_lms
#' 
#' 
#' # lazy_lm requires column names
#' colnames(x) <- c('y', 'x1', 'x2', 'x3', 'x4')
#' lazy_lm <- lazy_lm(y~.-1, data = x)
#' lazy_lm
#' 
#' 
#' 
#' @export
lazy_lm <- function(formula, data, weights = NULL, na.action = 'na.pass', 
                    center = FALSE, scale = FALSE, residuals = FALSE,...){
  
  data <- as.lazymatrix(data, read_only = TRUE, storage_format = 'double')
  
  if(data$`@transposed`){
    stop('data must be column-major to perform lazy_lm (otherwise this would be a very slow process)')
  }
  
  cnames <- dimnames(data)
  cnames <- cnames[[length(cnames)]]
  if(!length(cnames)){
    stop("lazy_lm cannot find column or partition names")
  }
  
  na.omit = TRUE
  smry_table <- partition_table(data)
  if(isTRUE(center)){
    center <- smry_table$Mean
  }
  
  if(isTRUE(scale)){
    scale <- smry_table$`Standard Deviation`
  }
  
  nobs <- nrow(data)
  force(na.action)
  # Create fake data to build model
  fake_data <- data[seq_len(ncol(data)),,drop=FALSE]
  fake_data <- as.data.frame(fake_data)
  names(fake_data) <- cnames
  mf <- stats::model.frame(formula, data = fake_data, na.action = na.action)
  mt <- attr(mf, "terms")
  y <- model.response(mf, "numeric")
  
  mlm <- is.matrix(y)
  ny <- if (mlm)  nrow(y) else length(y)
  nyr <- if (mlm)  ncol(y) else 1L
  
  if (is.empty.model(mt)) {
    x <- NULL
    z <- list(coefficients = if (mlm) matrix(NA_real_, 0, ny) else numeric(), rank = 0L)
    
    # TODO: add termsand formula
    return(z)
  }
  
  simple_fm <- formula
  simple_fm[[3]] <- quote(0)
  
  if(!is.null(weights)){
    weights <- as.lazymatrix(weights, storage_format = 'double')
  }
  
  
  map_f <- function(chunk_x, chunk_number, chunk_idxrange){
    if(!isFALSE(center) || !isFALSE(scale)){
      chunk_x <- t(chunk_x)
      if( !isFALSE(center) ){
        # need to center chunk
        chunk_x <- chunk_x - center
      }
      if( !isFALSE(scale) ){
        # need to scale
        chunk_x <- chunk_x / scale
      }
      chunk_x <- t(chunk_x)
    }
    chunk_x <- as.data.frame(chunk_x)
    names(chunk_x) <- cnames
    
    mf <- stats::model.frame(simple_fm, data = chunk_x, na.action = na.action)
    y <- model.response(mf, "numeric")
    suppressWarnings({
      x <- model.matrix(mt, chunk_x, contrasts)
    })
    
    chunk_x <- cbind(y, x)
    
    if(!is.null(weights)){
      cross <- crossprod(chunk_x, chunk_x * (weights[seq.int(chunk_idxrange[1], chunk_idxrange[2]), 1, drop = TRUE] / nobs))
    } else {
      cross <- crossprod(chunk_x, chunk_x / nobs)
    }
    cross
    
  }
  
  cross <- chunk_map(data, map_function = map_f, 
                     reduce = function(mapped){
                       Reduce('+', mapped)
                     }, ...)
  coef <- solve(cross[-seq_len(nyr), -seq_len(nyr)]) %*% cross[-seq_len(nyr),seq_len(nyr)]
  
  ret.residuals <- residuals
  residuals <- NULL
  if(ret.residuals){
    residuals <- chunk_map(data, function(chunk_x, chunk_number, chunk_idxrange){
      if(!isFALSE(center) || !isFALSE(scale)){
        chunk_x <- t(chunk_x)
        if( !isFALSE(center) ){
          # need to center chunk
          chunk_x <- chunk_x - center
        }
        if( !isFALSE(scale) ){
          # need to scale
          chunk_x <- chunk_x / scale
        }
        chunk_x <- t(chunk_x)
      }
      chunk_x <- as.data.frame(chunk_x)
      names(chunk_x) <- cnames
      
      mf <- stats::model.frame(simple_fm, data = chunk_x, na.action = na.action)
      y <- model.response(mf, "numeric")
      suppressWarnings({
        x <- model.matrix(mt, chunk_x, contrasts)
      })
      
      y - x %*% coef
    }, function(res){
      unlist(res)
    }, ...)
  }
  
  
  z <- list(
    coefficients = drop(coef),
    na.action = na.action,
    offset = 0,
    call = match.call(),
    terms = mt,
    residuals = residuals
  )
  class(z) <- c(if (mlm) "mlm", "lm")
  z
}

#' @rdname lazy_lm
#' @export
lazy_lm_simple <- function(data, yidx, xidx, intercept = TRUE, weights = NULL, residuals = FALSE, ...){
  
  data <- as.lazymatrix(data, read_only = TRUE, storage_format = 'double')
  
  if(data$`@transposed`){
    stop('data must be column-major to perform lazy_lm_simple (otherwise this would be a very slow process)')
  }
  
  
  cidx <- seq_len(ncol(data))
  nobs <- nrow(data)
  
  stopifnot(all(yidx %in% cidx))
  
  if(missing(xidx)){
    xidx <- cidx[!cidx %in% yidx]
  }
  
  if(!length(xidx) && !intercept){
    x <- NULL
    z <- list(coefficients = if (length(yidx) > 1) matrix(NA_real_, 0, length(yidx)) else numeric(), rank = 0L)
    
    return(z)
  }
  
  
  if(!is.null(weights)){
    stopifnot(length(weights) == nobs)
    weights <- as.lazymatrix(weights, storage_format = 'double')
  }
  
  cross <- chunk_map(data, function(chunk_x, ii, chunk_idxrange){
    chunk_x <- chunk_x[, c(yidx, xidx)]
    if(intercept){
      chunk_x <- cbind(chunk_x, 1)
    }
    if(!is.null(weights)){
      cross <- crossprod(chunk_x, chunk_x * (weights[seq.int(chunk_idxrange[1], chunk_idxrange[2]), 1, drop = TRUE] / nobs))
    } else {
      cross <- crossprod(chunk_x, chunk_x / nobs)
    }
    cross
  }, reduce = function(mapped){
    Reduce('+', mapped)
  }, ...)
  
  coef <- solve(cross[-seq_along(yidx), -seq_along(yidx)]) %*% cross[-seq_along(yidx),seq_along(yidx)]
  
  ret.residuals <- residuals
  residuals <- NULL
  if(ret.residuals){
    residuals <- chunk_map(data, function(chunk_x, chunk_number, chunk_idxrange){
      x <- chunk_x[,xidx]
      if(intercept){
        x <- cbind(x, 1)
      }
      chunk_x[,yidx] - x %*% coef
    }, function(res){
      unlist(res)
    }, ...)
  }
  
  dnames <- colnames(data)
  if(!length(dnames)){
    dnames <- sprintf('V%d', seq_len(ncol(data)))
  }
  if(intercept){
    coef <- c(coef[length(coef)], coef[-length(coef)])
    names(coef) <- c('(Intercept)', dnames[xidx])
  } else {
    coef <- drop(coef)
    names(coef) <- dnames[xidx]
  }
  
  
  z <- list(
    coefficients = coef,
    na.action = 'na.pass',
    offset = 0,
    call = match.call(),
    residuals = residuals
  )
  class(z) <- c(if (length(yidx) > 1) "mlm", "lm")
  z
  
}


