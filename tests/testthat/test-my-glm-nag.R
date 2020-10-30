context("Testing glm-gd")
test_that("You my_glm_gd function works",{
  n <- 5000; p <- 3
  beta <- c(-1, 0.2, 0.1)
  X <- cbind(1, matrix(rnorm(n * (p- 1)), ncol = p - 1))
  eta <- X %*% beta
  lambda <- exp(eta)
  y <- rpois(n, lambda = lambda)
  beta_hat_nag <- my_glm_nag(X, y, mu_fun = function(eta) exp(eta), gamma=0.0005)
  expect_equivalent(beta_hat_nag[1], beta[1], tolerance = 0.5 )
  expect_equivalent(beta_hat_nag[2], beta[2], tolerance = 0.5 )
  expect_equivalent(beta_hat_nag[3], beta[3], tolerance = 0.5 )
})
