sub_lm_model <- function(formula, data, weights = NULL, offset = NULL, contrasts = NULL, na.action = getOption('na.action'), method = "model.frame"){
  
  mf <- stats::lm(formula = formula, data = data, method = "model.frame", na.action = na.action)
  mt <- attr(mf, "terms")
  y <- model.response(mf, "numeric")
  if(!is.null(contrasts)){
    x <- model.matrix(mt, mf, contrasts = contrasts)
  } else {
    x <- model.matrix(mt, mf)
  }
  
  
  # t(X) W (X), X W y
  if(length(offset)){
    y <- y - offset
  }
  if(length(weights)){
    crossprod(x, cbind(x, y) * weights)
  } else {
    crossprod(x, cbind(x, y))
  }
}


#' @export
lazylm <- function (formula, data, weights = NULL, offset = NULL, contrasts = NULL, 
                    na.action = getOption('na.action'), ...) {
  cl <- match.call()
  fake_data <- as.data.frame(matrix(seq_len(data$npart), nrow = 1), make.names = FALSE)
  
  if(length(dimnames(x)) == data$ndim){
    nms <- dimnames(x)[[data$ndim]]
    names(fake_data) <- nms
  }
  
  mf <- stats::lm(formula = formula, data = fake_data, weights = NULL, offset = NULL, method = "model.frame", na.action = na.action)
  mt <- attr(mf, "terms")
  y <- model.response(mf, "numeric")
  mlm <- is.matrix(y)
  nobs <- data$partition_length
  
  
  if (is.empty.model(mt)) {
    x <- NULL
    z <- list(coefficients = if (mlm) matrix(NA_real_, 0, ncol(y)) else numeric(), weights = weights)
    return(z)
  } else {
    
    var_names <- globals::findGlobals(attr(mt, 'predvars'), list2env(fake_data), attributes = FALSE, unlist = TRUE)
    sel <- names(fake_data) %in% var_names
    partitions <- which(sel)
    nms <- names(fake_data)[sel]
    
    if(!length(weights) %in% c(0,1,nobs)) {
      stop("weights must have length 0,1,nobs...")
    } else if(length(weights) == nobs) {
      weights <- as.lazymatrix(weights, dim = c(nobs, 1), type = 'filearray')
    }
    if(!length(offset) %in% c(0,1,nobs)) {
      stop("offset must have length 0,1,nobs...")
    } else if(length(offset) == nobs) {
      offset <- as.lazymatrix(offset, dim = c(nobs, 1), type = 'filearray')
    }
    
    res <- chunk_map(x, map_fun = function(data, chunk, idx){
      data <- as.data.frame.matrix(data, row.names = NULL, make.names = FALSE)
      names(data) <- nms
      
      if(length(weights) == nobs){
        weights <- weights[seq.int(idx[1], idx[2]), 1]
      }
      if(length(offset) == nobs){
        offset <- offset[seq.int(idx[1], idx[2]), 1]
      }
      sub_lm_model(formula = formula, data = data, weights = weights, offset = offset, na.action = na.action)
    }, reduce = function(mapped){
      twty <- apply(simplify2array(mapped, higher = TRUE), 2, rowSums)
      p <- nrow(twty)
      
      xtx <- twty[,seq_len(p)]
      xty <- twty[,-seq_len(p)]
      
      list(
        xtx = xtx,
        xty = xty,
        coef = crossprod(solve(xtx), xty)
      )
    }, partitions = partitions, ...)
    
    z <- list(
      coefficients = res$coef,
      weights = weights,
      xtwx = res$xtx,
      xtwy = res$xty,
      partitions = partitions,
      varnames = nms
    )
    
  }
  
  # z$call <- 
  z$na.action <- attr(mf, "na.action")
  z$offset <- offset
  z$contrasts <- contrasts
  z$call <- cl
  attr(mt, '.Environment') <- NULL
  z$terms <- mt
  
  
  class(z) <- c(if (mlm) "mlm", "lm")
  z
}


# x <- as.lazymatrix(matrix(rnorm(2e4), ncol = 2), type = 'filearray')
# flm <- lazylm(V1 ~ (V1 > 0), x); flm
