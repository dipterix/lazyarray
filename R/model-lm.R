sub_lm_model <- function(formula, data, weights = NULL, offset = NULL, 
                         contrasts = NULL, na.action = getOption('na.action'), 
                         method = c("model.summary", "model.data")){
  method <- match.arg(method)
  environment(formula) <- environment()
  mf <- lm(formula = formula, data = data, method = "model.frame", 
           na.action = na.action, weights = weights, offset = offset)
  mt <- attr(mf, "terms")
  y <- model.response(mf, "numeric")
  if(!is.null(contrasts)){
    x <- model.matrix(mt, mf, contrasts = contrasts)
  } else {
    x <- model.matrix(mt, mf)
  }
  nx <- nrow(x)
  weights <- model.weights(mf)
  if(!nx){
    return(
      data = 0,
      nsub = nx,
      subw = weights
    )
  }
  offset <- model.offset(mf)
  
  # t(X) W (X), X W y
  if(method == "model.summary"){
    if(length(offset)){
      y <- y - offset
    }
    if(length(weights)){
      data <- crossprod(x, cbind(x, y) * weights)
    } else {
      data <- crossprod(x, cbind(x, y))
    }
    return(list(
      data = data,
      nsub = nx,
      subw = weights
    ))
  } else {
    return(list(
      y = y,
      x = x,
      w = weights,
      offset = offset
    ))
  }
  
}


#' Fitting linear models using \code{lazyarray}
#' @param formula an object of class \code{\link[stats]{formula}}. For variable
#' names to be used, see 'Details'
#' @param data a \code{lazyarray} object
#' @param fitted whether to calculate fitted data and residuals. This may
#' take time and memory if data is large
#' @param qr.tol the tolerance for detecting linear dependencies in the 
#' partitions of \code{data}; see \code{\link{qr}}
#' @param weights,offset,contrasts,na.action see \code{\link[stats]{lm}}
#' @param ... passed to \code{\link{chunk_map}}
#' @return An object of class \code{c("lazylm", "lm")} or for multiple 
#' responses of class \code{c("lazylm", "mlm")}.
#' @details The array will be reshaped to a matrix first before fitting the 
#' linear models. A \eqn{100 x 20 x 5} array will be reshaped to a 
#' \eqn{2000 x 5} lazy matrix.
#' The variables are the partitions of the array. If \code{dimnames}
#' are set for the last margin index, then those will be used as variable 
#' names, otherwise \code{lazylm} automatically assign 
#' \eqn{"V1", "V2", "V3", ...} as each partition names.
#' 
#' @examples 
#' 
#' library(lazyarray)
#' arr <- array(rnorm(72), c(6,3,4))
#' arr[1,1,1] <- NA        # Allow NA to be treated
#' offset = rnorm(18)    # offset and weights are optional
#' weights = runif(18)
#' 
#' formula <- V1 ~ .-V2-1 + (V2 > 0)
#' 
#' data <- as.lazyarray(arr, type = 'file')
#' object <- lazylm(formula, data, weights = weights, offset = offset)
#' 
#' 
#' # Compare to stats::lm
#' dim(arr) <- c(18, 4)
#' lm_data <- as.data.frame(arr)
#' flm <- lm(formula, lm_data, weights = weights, offset = offset)
#' 
#' cbind(coef(object), coef(flm))
#' cbind(resid(object), resid(flm))
#' cbind(fitted(object), fitted(flm))
#' summary(object)
#' summary(flm)
#' 
#' @export
lazylm <- function (formula, data, fitted = FALSE, weights = NULL, 
                    offset = NULL, contrasts = NULL, 
                    na.action = getOption('na.action'), qr.tol = 1e-7, ...) {
  cl <- match.call()
  fake_data <- as.data.frame(matrix(seq_len(data$npart), nrow = 1), make.names = FALSE)
  
  if(length(dimnames(data)) == data$ndim){
    nms <- dimnames(data)[[data$ndim]]
    names(fake_data) <- nms
  } else {
    nms <- names(fake_data)
  }
  
  mf <- stats::lm(formula = formula, data = fake_data, weights = NULL, offset = NULL, method = "model.frame", na.action = na.action)
  mt <- attr(mf, "terms")
  y <- model.response(mf)
  mlm <- is.matrix(y)
  nobs <- data$partition_length
  
  z <- new.env(parent = emptyenv())
  
  if (is.empty.model(mt)) {
    z$coefficients <- if (mlm) matrix(NA_real_, 0, ncol(y)) else numeric()
    z$weights <- weights
    z$nobs <- nobs
  } else {
    
    # Tricks to find globals
    varenv <- new.env()
    this_env <- environment()
    var_names <- NULL
    lapply(nms, function(nm){
      delayedAssign(nm, {
        this_env$var_names <- c(var_names, nm)
        fake_data[[nm]]
      }, assign.env = varenv)
      return()
    })
    
    eval(attr(mt, 'predvars'), envir = varenv)
    
    sel <- nms %in% var_names
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
    
    res <- chunk_map(data, map_fun = function(data, chunk, idx){
      data <- as.data.frame(data, row.names = NULL, make.names = FALSE)
      names(data) <- nms
      
      if(length(weights) == nobs){
        weights <- weights[seq.int(idx[1], idx[2]), 1]
      }
      if(length(offset) == nobs){
        offset <- offset[seq.int(idx[1], idx[2]), 1]
      }
      sub_lm_model(formula = formula, data = data, weights = weights, offset = offset, na.action = na.action, contrasts = contrasts)
    }, reduce = function(mapped){
      
      sub_obs <- vapply(mapped, '[[', 0, 'nsub')
      has_obs <- sub_obs > 0
      mapped <- mapped[has_obs]
      twty <- sapply(mapped, '[[', 'data', simplify = TRUE, USE.NAMES = FALSE)
      twty <- rowSums(twty)
      dim(twty) <- dim(mapped[[1]]$data)
      dimnames(twty) <- dimnames(mapped[[1]]$data)
      
      model_weights <- unlist(lapply(mapped, '[[', 'subw'))
      
      
      # twty <- apply(simplify2array(mapped, higher = TRUE), 2, rowSums)
      p <- nrow(twty)
      
      xtx <- twty[,seq_len(p)]
      xty <- twty[,-seq_len(p)]
      
      # use Qr decomp instead of solve
      a <- qr(xtx, tol = qr.tol)
      xtx_solve <- t(qr.coef(a, diag(1, p)))
      # xtx_solve[is.na(xtx_solve)] <- 0
      
      nobs <- sum(sub_obs)
      
      z$model_weights <- model_weights
      z$xtwx <- xtx
      z$xtwy <- xty
      z$xtx_solve <- xtx_solve
      z$coefficients <- crossprod(xtx_solve, xty)
      z$rank <- a$rank
      z$qr <- a
      z$nobs <- nobs
      z$df.residual <- nobs - a$rank
      z$x_ncols <- p
    }, partitions = partitions, ...)
    
    z$weights <- weights
    z$partitions <- partitions
    z$varnames <- nms
    z$weights <- weights
    
  }
  
  # z$call <- 
  z$na.action <- NULL
  z$na_option <- na.action
  z$offset <- offset
  z$contrasts <- contrasts
  z$call <- cl
  z$var_names <- var_names
  attr(mt, '.Environment') <- NULL
  z$terms <- mt
  z$raw_nobs <- data$partition_length
  z$formula <- formula
  
  z$lazy_mat = data
  
  
  class(z) <- c("lazylm", if (mlm) "mlm", "lm")
  
  if(fitted){
    tryCatch({
      fitted(z)
    }, error = function(e){
      warning("Error while trying to obtain fitted values: ", e$message)
    })
  }
  
  z
}


#' @export
residuals.lazylm <- function(object, ...) {
  
  if(is.null(object$residuals)){
    # calculate residual
    data <- object$lazy_mat
    nms <- object$var_names
    weights <- object$weights
    offset <- object$offset
    coef <- object$coefficients
    partitions <- object$partitions
    contrasts <- object$contrasts
    nobs <- data$partition_length
    na.action <- object$na_option
    formula <- object$formula
    
    is_mlm <- inherits(object, 'mlm')
    
    chunk_map(data, map_fun = function(data, chunk, idx){
      data <- as.data.frame(data, row.names = NULL, make.names = FALSE)
      names(data) <- nms
      
      if(length(weights) == nobs){
        weights <- weights[seq.int(idx[1], idx[2]), 1]
      }
      if(length(offset) == nobs){
        offset <- offset[seq.int(idx[1], idx[2]), 1]
      }
      md <- sub_lm_model(formula = formula, data = data, weights = weights, contrasts = contrasts,
                         offset = offset, na.action = na.action, method = 'model.data')
      fit <- md$x %*% coef
      if(!is.null(md$offset)){
        fit <- fit + md$offset
      }
      resid <-  md$y - fit
      if(!is.null(md$w)){
        resid <- resid # sqrt(md$w)
      }
      list(
        resid = resid,
        fitted = fit
      )
    }, reduce = function(mapped){
      
      resid <- sapply(mapped, '[[', 'resid', simplify = FALSE)
      fitted <- sapply(mapped, '[[', 'fitted', simplify = FALSE)
      object$residuals <- do.call('rbind', resid)
      object$fitted.values <- do.call('rbind', fitted)
      NULL
    }, partitions = partitions)
  }
  object$residuals
  
}

#' @export
fitted.lazylm <- function (object, ...) {
  residuals.lazylm(object)
  object$fitted.values
}

#' @export
summary.lazylm <- function(object, ...){
  z <- object
  p <- z$rank
  rdf <- z$df.residual
  
  residuals.lazylm(z)
  
  ans <- list(
    call = z$call,
    terms = z$terms
  )
  if (!is.null(z$model_weights)) {
    ans$weights = z$model_weights
  }
  class(ans) <- c("summary.lazylm", "summary.lm")
  
  if (p == 0) {
    r <- z$residuals
    n <- length(r)
    w <- z$model_weights
    if (is.null(w)) {
      rss <- sum(r^2)
    } else {
      rss <- sum(w * r^2)
      r <- sqrt(w) * r
    }
    resvar <- rss/rdf
    ans$aliased <- is.na(coef(object))
    ans$residuals <- as.vector(r)
    ans$df <- c(0L, n, length(ans$aliased))
    ans$coefficients <- matrix(NA_real_, 0L, 4L, dimnames = list(NULL, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")))
    ans$sigma <- sqrt(resvar)
    ans$r.squared <- ans$adj.r.squared <- 0
    ans$cov.unscaled <- matrix(NA_real_, 0L, 0L)
    return(ans)
  }
  if (is.null(z$terms)) 
    stop("invalid 'lazylm' object:  no 'terms' component")
  if (!inherits(object, "lazylm")) 
    warning("calling summary.lm(<fake-lazylm-object>) ...")
  n <- object$nobs
  if (is.na(z$df.residual) || n - p != z$df.residual) 
    warning("residual degrees of freedom in object suggest this is not an \"lazylm\" fit")
  r <- as.vector(z$residuals)
  f <- as.vector(z$fitted.values)
  w <- z$model_weights
  if (is.null(w)) {
    mss <- if (attr(z$terms, "intercept")) {
      sum((f - mean(f))^2)
    } else {
      sum(f^2)
    }
    rss <- sum(r^2)
  } else {
    mss <- if (attr(z$terms, "intercept")) {
      m <- sum(w * f/sum(w))
      sum(w * (f - m)^2)
    } else {
      sum(w * f^2)
    }
    rss <- sum(w * r^2)
    r <- sqrt(w) * r
  }
  resvar <- rss/rdf
  if (is.finite(resvar) && resvar < (mean(f)^2 + var(c(f))) * 1e-30) 
    warning("essentially perfect fit: summary may be unreliable")
  # p1 <- seq_len(p)
  # R <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
  # z$xtx_solve
  # se <- sqrt(diag(R) * resvar)
  # est <- z$coefficients[Qr$pivot[p1]]
  # tval <- est/se
  se <- sqrt(diag(z$xtx_solve) * resvar)
  est <- as.vector(z$coefficients)
  tval <- est / se
  
  ans$residuals <- as.vector(r)
  ans$coefficients <- cbind(
    Estimate = est,
    `Std. Error` = se,
    `t value` = tval,
    `Pr(>|t|)` = 2 * stats::pt(abs(tval), rdf, lower.tail = FALSE)
  )
  ans$aliased <- is.na(z$coefficients)
  ans$sigma <- sqrt(resvar)
  ans$df <- c(p, rdf, z$x_ncols)
  if (p != attr(z$terms, "intercept")) {
    df.int <- if (attr(z$terms, "intercept")) { 1L } else { 0L }
    ans$r.squared <- mss/(mss + rss)
    ans$adj.r.squared <- 1 - (1 - ans$r.squared) * ((n - df.int)/rdf)
    ans$fstatistic <- c(value = (mss/(p - df.int))/resvar, numdf = p - df.int, dendf = rdf)
  } else {
    ans$r.squared <- ans$adj.r.squared <- 0
  }
  ans$lazyobj <- object
  ans
}

#' @export
print.summary.lazylm <- function(x, ...){
  res <- NextMethod()
  
  if(isTRUE(x$na_option %in% c('na.omit', 'na.exclude')) && x$lazyobj$nobs != x$lazyobj$raw_nobs){
    cat(sprintf("(%d observation deleted due to missingness)", x$lazyobj$raw_nobs - x$lazyobj$nobs))
  }
  
  
  invisible(res)
}
