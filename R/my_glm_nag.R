#` @title Solve generalized linear models with gradient metho, using Adaptive update, Nesterov
#` @description This is the function of generalized linear model with gradient model
#`@export
my_glm_nag <-
  function(X, y, mu_fun, gamma, maxit=25, tol=1e-10) {
    alpha <- 0.5
    beta <- rep(0,ncol(X))
    v <- rep(0,ncol(X))
    for(j in seq_len(maxit)) {
      b_old <- beta
      eta <- X %*% (beta - alpha * v)
      mu <- mu_fun(eta)
      delta <- t(X) %*% (y - mu)
      v <- alpha * v - gamma * delta
      beta <- beta - v
#      if(sqrt(crossprod(beta - b_old)) < tol)
#        break
    }
    beta
  }
