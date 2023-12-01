N_persub <- 10000

test_that("No underflow/overflow", {
  gamma_C <- compute_state(
    delta = parameters_setting$init_vec,
    Y = dat$y_mat[1, ],
    A = parameters_setting$trans_mat,
    B = parameters_setting$emis_mat,
    X = dat$X_array[, 1:4, 1],
    family = "P"
  )
  expect_equal(any(
    c( is.null(gamma_C), is.na(gamma_C)
    )
  ),
  FALSE)
})

test_that("Validation with the C code using the following code", {
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
  gamma_validate <- matrix(NA,
                           nrow = dim(gamma_C)[1],
                           ncol = dim(gamma_C)[2]
  )
  equal_mat <- matrix(NA, ncol = dim(forward_backward_C$log_alpha)[2],
                      nrow=dim(forward_backward_C$log_alpha)[1])
  
  for (j in 1:dim(forward_backward_C$log_alpha)[2]) {
    for (i in 1:dim(forward_backward_C$log_alpha)[1]) {
      gamma_validate[i, j] <- forward_backward_C$log_alpha[i, j] +
        forward_backward_C$log_beta[i, j] - llh_C
      gamma_validate[i, j] <- exp(gamma_validate[i, j])
      equal_mat[i,j] <- assertthat::are_equal(gamma_validate[i, j], gamma_C[i, j])
    }
  }
  expect_equal(all(equal_mat), TRUE)
})
