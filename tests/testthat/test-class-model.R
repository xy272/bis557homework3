context("Testing class-model")
test_that("You class_model function works",{

  K = 3
  P = 6

  library(dplyr)
  library(plyr)
  library(ramify)
  d <- penguinsi %>%
      select(species, island, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g, sex) %>%
      mutate(sex = if_else(sex == "male", 1, 0))
  d$species <- revalue(d$species, c(Adelie=0, Gentoo=1, Chinstrap=2))
  d$island <- revalue(d$island, c(Torgersen=0, Biscoe=1, Dream=2))

  n_train <- 300
  n_test <- 44
  X_train <- d[1:n_train,2:7]
  y_train <- d[1:n_train,1]
  X_test <- d[(n_train+1):344,2:7]
  y_test <- d[(n_train+1):344,1]

  X_train <- data.matrix(X_train)
  y_train <- data.matrix(y_train)
  X_test <- data.matrix(X_test)
  y_test <- data.matrix(y_test)
  # coefficients of all K models
  beta_hat <- matrix(rep(0, len=K*(P+1)), nrow=P+1)
  # training coefficients with training data
  X_k <- cbind(1, X_train)
  for (k in seq(1L, K))
  {
    y_k <- as.numeric(y_train == k)
    beta_tmp <- my_glm_nr_logistic(X_k, y_k)
    beta_hat[1:(P+1),k] <- beta_tmp[1:(P+1)]
  }
  # predicting
  prob <- matrix(rep(0, len=n_test*K), nrow=n_test)
  X_k <- cbind(1, X_test)
  for (k in seq(1L, K))
  {
    prob_tmp <- 1 / ( 1 + exp(-X_k %*% beta_hat[1:(P+1),k]))
    prob[1:n_test,k] <- prob_tmp[1:n_test]
  }
  y_pred <- argmax(prob, rows = TRUE)

  bias <- abs(y_test-y_pred)
  bias1 <- sum(bias)


  expect_lt(bias1, 5)
})
