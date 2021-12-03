#'mylogit
#'
#'mylogit is used to fit a logistic regression model.
#'
#'@import stats
#'
#'@param formula a symbolic description of the model to be fitted.
#'@param data a data frame containing the variables in the model.
#'
#'@return a list containing the following components:
#'
#'@examples
#'mylogit(formula = supp ~ len + dose, data = ToothGrowth)
#'
#'@export
#'
mylogitEst = function(X, y, criteria, break_loop) {
  y_num = as.matrix(ifelse(y == level[1], 0, 1)) # set y into 0/1
  beta = matrix(0, p+1, 1) # initialize beta
  eps = 1 # initial criteria
  run_loop = 0 # initial running loop
  # loop to converge beta
  while (eps >= criteria || run_loop < break_loop) {
    prob = 1/(1+exp(-X %*% beta)) # X is n by p+1, beta is p+1 by 1
    X_tilde = as.numeric(prob*(1-prob))*X
    beta_new = beta + solve(t(X) %*% X_tilde) %*% (t(X) %*% (y_num - prob))
    eps = max(abs(beta_new - beta)) # compute criteria
    beta = beta_new # update beta
    run_loop =  run_loop + 1 # update run_loop
  }
  return(beta)
}

mylogit.default = function(formula, data, criteria = 0.001, break_loop = 10000) {
  # extra x and y
  mf = model.frame(formula=formula, data=data)
  X = model.matrix(attr(mf, "terms"), data=mf)
  y = model.response(mf)
  n = dim(X)[1]
  p = dim(X)[2] - 1
  level = levels(y)
  est = mylogitEst(X, y, criteria, break_loop)

  est$fitted.values = 1/(1+exp(-X %*% coef))
  est$residuals = y - est$fitted.values

}
