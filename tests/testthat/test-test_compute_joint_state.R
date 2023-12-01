N_persub <- 10000

test_that("No underflow/overflow", {
  xi_C <- compute_joint_state(
    delta = parameters_setting$init_vec,
    Y = dat$y_mat[1, ],
    A = parameters_setting$trans_mat,
    B = parameters_setting$emis_mat,
    X = dat$X_array[, 1:4, 1],
    family = "P"
  )
  expect_equal(any(
    c( is.null(xi_C[, , -N_persub]),
       is.na(xi_C[, , -N_persub])
    )
  ),
  FALSE)
})

test_that("Validation with the C code using the following code", {
  xi_C <- compute_joint_state(
    delta = parameters_setting$init_vec,
    Y = dat$y_mat[1, ],
    A = parameters_setting$trans_mat,
    B = parameters_setting$emis_mat,
    X = dat$X_array[, 1:4, 1],
    family = "P"
  )
  forward_backward_C <- forward_backward(
    delta = parameters_setting$init_vec,
    Y = dat$y_mat[1, ],
    A = parameters_setting$trans_mat,
    B = parameters_setting$emis_mat,
    X = dat$X_array[, 1:4, 1],
    family = "P"
  )
  gamma_C <- compute_state(
    delta = parameters_setting$init_vec,
    Y = dat$y_mat[1, ],
    A = parameters_setting$trans_mat,
    B = parameters_setting$emis_mat,
    X = dat$X_array[, 1:4, 1],
    family = "P"
  )
  llh_C <- compute_loglikelihood(
    delta = parameters_setting$init_vec,
    Y = dat$y_mat[1, ],
    A = parameters_setting$trans_mat,
    B = parameters_setting$emis_mat,
    X = dat$X_array[, 1:4, 1],
    family = "P"
  )
  xi_validate <- array(NA, dim = dim(xi_C))
  
  for (i in 1:(N_persub - 1)) {
    for (j in 1:dim(xi_C)[1]) {
      for (k in 1:dim(xi_C)[2]) {
        eta_working <- dat$X_array[i + 1, 1:4, 1] %*%
          parameters_setting$emis_mat[k, ]
        mu_working <- exp(eta_working)
        xi_validate[j, k, i] <- exp(forward_backward_C$log_alpha[j, i] +
                                      log(parameters_setting$trans_mat[j, k]) +
                                      dpois(dat$y_mat[1, i + 1], mu_working, log = TRUE) +
                                      forward_backward_C$log_beta[k, i + 1] - llh_C)
      }
    }
  }
  for (j in 1:dim(xi_C)[1]) {
    for (k in 1:dim(xi_C)[2]) {
      xi_validate[j, k, N_persub] <- NaN
    }
  }
  
  equal_mat <- array(NA, dim = dim(xi_C))
  
  for (i in 1:(N_persub - 1)) {
    for (j in 1:dim(xi_C)[1]) {
      for (k in 1:dim(xi_C)[2]) {
        if (abs(xi_validate[j, k, i] - xi_C[j, k, i]) > 1e-11) {
          equal_mat[j,k,i] <- FALSE
        } else {
          equal_mat[j,k,i] <- TRUE
        }
      }
    }
  }
  
  equal_mat[,,N_persub] <- TRUE
  expect_equal(all(equal_mat), TRUE)
})
