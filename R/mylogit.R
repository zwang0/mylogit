#'mylogit
#'
#'mylogit is used to fit a logistic regression model.
#'
#'@param formula a symbolic description of the model to be fitted.
#'@param data a data frame containing the variables in the model.
#'@param format output format. \emph{brief} for brief output, \emph{detailed} for detailed output.
#'
#'@return a list containing the following components:
#'@return \item{coefficients}{coefficients of logistic regression model}
#'@return \item{fitted.values}{fitted values for input dataset}
#'@return \item{cov}{covariance matrix}
#'@return \item{std.err}{standard error}
#'@return \item{deviance}{deviance}
#'@return \item{null.deviance}{null deviance}
#'@return \item{r2}{r-square, 1 - (deviance/null.deviance)}
#'@return \item{AIC}{AIC score}
#'@return \item{null.df}{degrees of freedom of null model}
#'@return \item{df}{degrees of freedom of logistic model}
#'@return \item{level}{the levels of response variable}
#'@return \item{predict.class}{class prediction for input dataset}
#'@return \item{predict.accuracy}{accuracy of class prediction}
#'@return \item{formula}{formula of logistic model}
#'@return \item{data}{input dataset}
#'
#'@examples
#'logit = mylogit(formula = supp ~ len + dose, data = ToothGrowth)
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
  max.iter = 1e04

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
  AIC = -2*loglik + 2*(p+1)
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
                AIC = AIC,
                null.df = null.df,
                df = df,
                level = level,
                predict.class = predict.class,
                predict.accuracy = predict.acc,
                formula = formula,
                data = data)
  if(format == "brief") {
    cat("Call:\n")
    print(output$call)
    cat("\nCoefficients:\n")
    print(round(output$coefficients, 4), row.names=FALSE)
    cat("\nDegrees of Freedom:\n")
    cat(output$null.df, " Total (i.e. Null); ", output$df, " Residual")
    cat("\n    Null deviance: ", round(null.deviance, 4), " on ", null.df, " degrees of freedom" )
    cat("\nResidual deviance: ", round(deviance, 4), " on ", df, " degrees of freedom")
    cat("\nR-squared: ", round(r2, 4))
    cat("\nAIC: ", round(AIC, 4))
  } else if (format == "detailed") {
    z_val = unlist(output$coefficients / output$std.err)
    p_val = (1 - pnorm(abs(z_val))) * 2
    coef.df = data.frame(coef, output$std.err, z_val, p_val)
    colnames(coef.df) = c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
    # print
    cat("Call:\n")
    print(output$call)
    cat("\nCoefficients:\n")
    print(round(coef.df, 4))
    cat("\n    Null deviance: ", round(null.deviance, 4), " on ", null.df, " degrees of freedom" )
    cat("\nResidual deviance: ", round(deviance, 4), " on ", df, " degrees of freedom")
    cat("\nR-squared: ", round(r2, 4))
    cat("\nAIC: ", round(AIC, 4))
    cat("\nPrediction Class accuracy: ", round(predict.acc, 4))
  }
  invisible(output)
}
