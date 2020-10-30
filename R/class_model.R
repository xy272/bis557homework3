my_glm_nr_logistic <-
  function(X, y, maxit=25L, tol=1e-10)
  {
    beta <- rep(0,ncol(X))
    for(j in seq(1L, maxit)) {
      b_old <- beta
      p <- 1 / (1 + exp(- X %*% beta))
      W <- as.numeric(p * (1 - p))
      XtX <- crossprod(X, diag(W) %*% X)
      score <- t(X) %*% (y - p)
      delta <- solve(XtX, score)
      beta <- beta + delta
      if(sqrt(crossprod(beta - b_old)) < tol) break
    }
    beta
  }
