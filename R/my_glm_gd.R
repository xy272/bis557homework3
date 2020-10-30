#` @title Solve generalized linear models with gradient method.
#` @description This is the function of generalized linear model with gradient model
#`@export
my_glm_gd <-
  function(X, y, mu_fun, gamma, maxit=25, tol=1e-10) {
    beta <- rep(0,ncol(X))
    for(j in seq_len(maxit)) {
      b_old <- beta
      eta <- X %*% beta
      mu <- mu_fun(eta)
      delta <- t(X) %*% (y - mu)
      beta <- beta + gamma * delta
    }
    beta
  }
