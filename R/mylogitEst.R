#' mylogitEst
#'
#' Find the estimate of mylogit function
#'
#' @param X input X
#' @param y_mat input y
#' @param p number of parameter
#' @param criteria criteria for convergence
#' @param max.iter maximum iteration
#'
#' @keywords internal
#' @noRd
mylogitEst = function(X, y_mat, p, criteria, max.iter) {
  beta = matrix(0, p+1, 1) # initialize beta
  eps = 1 # initial criteria
  iter = 0 # initial iteration
  # loop to converge beta
  while (eps >= criteria || iter < max.iter) {
    prob = 1/(1+exp(-X %*% beta)) # X is n by p+1, beta is p+1 by 1
    X_tilde = as.numeric(prob*(1-prob))*X
    beta_new = beta + solve(t(X) %*% X_tilde) %*% (t(X) %*% (y_mat - prob))
    eps = max(abs(beta_new - beta)) # compute criteria
    beta = beta_new # update beta
    iter =  iter + 1 # update run_loop
  }
  return(beta)
}
