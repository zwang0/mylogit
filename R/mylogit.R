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
mylogitEst = function(X, y_mat, p, criteria, break_loop) {
  beta = matrix(0, p+1, 1) # initialize beta
  eps = 1 # initial criteria
  run_loop = 0 # initial running loop
  # loop to converge beta
  while (eps >= criteria || run_loop < break_loop) {
    prob = 1/(1+exp(-X %*% beta)) # X is n by p+1, beta is p+1 by 1
    X_tilde = as.numeric(prob*(1-prob))*X
    beta_new = beta + solve(t(X) %*% X_tilde) %*% (t(X) %*% (y_mat - prob))
    eps = max(abs(beta_new - beta)) # compute criteria
    beta = beta_new # update beta
    run_loop =  run_loop + 1 # update run_loop
  }
  return(beta)
}

mylogit = function(formula, data, format="Simple") {
  # extra x and y
  mf = model.frame(formula=formula, data=data)
  X = model.matrix(attr(mf, "terms"), data=mf)
  y = model.response(mf)
  y_mat = as.matrix(ifelse(y == levels(y)[1], 0, 1)) # set y into 0/1
  n = dim(X)[1]
  p = dim(X)[2] - 1
  level = levels(y)
  # control criteria
  criteria = 1e-05
  break_loop = 1e05
  # compute estimate
  coef = mylogitEst(X, y_mat, p, criteria, break_loop)
  # compute stats indicators
  yhat = 1/(1+exp(-X %*% coef))
  residuals = (y_mat - yhat) / sqrt(yhat*(1-yhat))
  cov.mat = solve(t(X) %*% (as.numeric(yhat*(1-yhat))*X))
  std.err = sqrt(diag(cov.mat))
  # deviance =
  # null.deviance =
  r2 = 1 - deviance / null.deviance
  call = match.call()
  df = n-p-1
  predict.class = ifelse(yhat < 0.5, level[1], level[2])
  predict.error = 1 - sum(y == predict.class)/n
  output = list(call = call,
                coefficients = coef,
                fitted.values = yhat,
                cov = cov.mat,
                std.err = std.err,
                deviance = deviance,
                null.deviance = null.deviance,
                r2 = r2,
                df = df,
                level = level,
                predict.class = predict.class,
                predict.error)
  if(format=="Simple") {
    print("simple")
  } else if (format=="Summary") {
    print("summary")
  }
  invisible(output)
}

