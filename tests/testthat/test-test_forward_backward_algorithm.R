N_persub <- 10000

test_that("Output from the forward-backward algorithm should be the same as the forward algorithm", {
  forward_backward_C <- forward_backward(
    delta = parameters_setting$init_vec,
    Y = dat$y_mat[1, ],
    A = parameters_setting$trans_mat,
    B = parameters_setting$emis_mat,
    X = dat$X_array[, 1:4, 1],
    family = "P"
  )
  forward_C <- forward(
    delta = parameters_setting$init_vec,
    Y = dat$y_mat[1, ],
    A = parameters_setting$trans_mat,
    B = parameters_setting$emis_mat,
    X = dat$X_array[, 1:4, 1],
    family = "P"
  )
  
  equal_mat <- matrix(NA, ncol = dim(forward_C)[2],
                      nrow=dim(forward_C)[1])
  for (j in 1:dim(forward_C)[2]) {
    for (i in 1:dim(forward_C)[1]) {
      equal_mat[i,j] <- assertthat::are_equal(forward_C[i, j],
                                              forward_backward_C$log_alpha[i, j])
    }
  }
  expect_equal(all(equal_mat), TRUE)
})

test_that("Output from the forward-backward algorithm should be the same as the backward algorithm", {
  forward_backward_C <- forward_backward(
    delta = parameters_setting$init_vec,
    Y = dat$y_mat[1, ],
    A = parameters_setting$trans_mat,
    B = parameters_setting$emis_mat,
    X = dat$X_array[, 1:4, 1],
    family = "P"
  )
  backward_C <- backward(
    delta = parameters_setting$init_vec,
    Y = dat$y_mat[1, ],
    A = parameters_setting$trans_mat,
    B = parameters_setting$emis_mat,
    X = dat$X_array[, 1:4, 1],
    family = "P"
  )
  
  equal_mat <- matrix(NA, ncol = dim(backward_C)[2],
                      nrow=dim(backward_C)[1])
  for (j in 1:dim(backward_C)[2]) {
    for (i in 1:dim(backward_C)[1]) {
      equal_mat[i,j] <- assertthat::are_equal(backward_C[i, j],
                                              forward_backward_C$log_beta[i, j])
    }
  }
  expect_equal(all(equal_mat), TRUE)
})

test_that("No underflow/overflow", {
  forward_backward_C <- forward_backward(
    delta = parameters_setting$init_vec,
    Y = dat$y_mat[1, ],
    A = parameters_setting$trans_mat,
    B = parameters_setting$emis_mat,
    X = dat$X_array[, 1:4, 1],
    family = "P"
  )
  expect_equal(any(
    c( is.null(forward_backward_C$log_alpha), is.na(forward_backward_C$log_alpha),
       is.null(forward_backward_C$log_beta), is.na(forward_backward_C$log_beta)
       )
  ),
  FALSE)
})

test_that("Backward times forward should be getting the same llh-likelihood as the llh function", {
  forward_backward_C <- forward_backward(
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
  
  forward_backward_prod <- matrix(NA,
                                  nrow = dim(forward_backward_C$log_alpha)[1],
                                  ncol = dim(forward_backward_C$log_alpha)[2]
  )
  equal_mat <- rep(NA, ncol = dim(forward_backward_C$log_alpha)[2])
  
  for (j in 1:dim(forward_backward_prod)[2]) {
    for (i in 1:dim(forward_backward_prod)[1]) {
      forward_backward_prod[i, j] <- forward_backward_C$log_alpha[i, j] +
        forward_backward_C$log_beta[i, j]
    }
    max_forward_backward_C <- max(forward_backward_prod[, j])
    llh_forward_backward <- max_forward_backward_C +
      log(sum(exp(forward_backward_prod[, j] - max_forward_backward_C)))
    equal_mat[j] <- assertthat::are_equal(llh_forward_backward, llh_C)
  }
  expect_equal(all(equal_mat), TRUE)
})
