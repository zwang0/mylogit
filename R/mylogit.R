#'mylogit
#'
#'mylogit is used to fit a logistic regression model.
#'
#'@param formula a symbolic description of the model to be fitted.
#'@param data a data frame containing the variables in the model.
#'@param format output format. "brief" for brief output, "detailed" for detailed output.
#'
#'@return a list containing the following components:
#'
#'@examples
#'mylogit(formula = supp ~ len + dose, data = ToothGrowth)
#'
#'@import stats datasets
#'
#'@export
mylogit = function(formula, data, format="brief") {
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
  max.iter = 1e05

  # compute estimate
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
  coef = beta

  # compute stats indicators
  yhat = 1/(1+exp(-X %*% coef))
  loglik = sum(log(yhat)*as.numeric(y_mat) + log(1-yhat)*(1- as.numeric(y_mat)))
  loglik_null = sum(log(mean(y_mat))*as.numeric(y_mat) +
                      log(1-mean(y_mat))*(1- as.numeric(y_mat)))
  residuals = (y_mat - yhat) / sqrt(yhat*(1-yhat))
  cov.mat = solve(t(X) %*% (as.numeric(yhat*(1-yhat))*X))
  std.err = sqrt(diag(cov.mat))
  deviance = -2*loglik
  null.deviance = -2*loglik_null
  # AIC =
  r2 = 1 - deviance / null.deviance
  call = match.call()
  null.df = n-1
  df = n-p-1
  predict.class = ifelse(yhat < 0.5, level[1], level[2])
  predict.acc = sum(y == predict.class)/n
  output = list(call = call,
                coefficients = as.data.frame(t(coef)),
                fitted.values = yhat,
                cov = cov.mat,
                std.err = std.err,
                deviance = deviance,
                null.deviance = null.deviance,
                r2 = r2,
                null.df = null.df,
                df = df,
                level = level,
                predict.class = predict.class,
                predict.accuracy = predict.acc)
  if(format == "brief") {
    cat("Call:\n")
    print(output$call, digits=4)
    cat("\nCoefficients:\n")
    print(output$coefficients, row.names=FALSE, digits=4)
    cat("\nDegrees of Freedom:\n")
    cat(output$null.df, " Total (i.e. Null); ", output$df, " Residual")
  } else if (format == "detailed") {
    z_val = unlist(output$coefficients / output$std.err)
    p_val = (1 - pnorm(abs(z_val))) * 2
    df = data.frame(coef, output$std.err, z_val, p_val)
    colnames(df) = c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
    # print
    cat("Call:\n")
    print(output$call, digits=4)
    cat("\nCoefficients:\n")
    print(df, digits=4)
    cat("\n    Null deviance: ", null.deviance, " on ", null.df, " degrees of freedom" )
    cat("\nResidual deviance: ", deviance, " on ", df, " degrees of freedom")
  }
  invisible(output)
}

