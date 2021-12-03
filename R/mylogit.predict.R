#'mylogit.predict
#'
#'mylogit.predict is used to make a class prediction of logistic model.
#'
#'@param model a logistic model fitted by mylogit function.
#'@param data a data frame containing the variables in the model.
#'@param print TRUE, print out prediction class; FALSE, as default, no printing.
#'
#'@return a list containing the following components:
#'@return \item{yhat}{fitted values}
#'@returm \item{predict.class}{predicted class}
#'
#'@examples
#'logit = mylogit(formula = supp ~ len + dose, data = ToothGrowth)
#'logit.pred = mylogit.predict(logit)
#'
#'@import stats
#'
#'@export
mylogit.predict = function(model, data=NULL, print=FALSE) {
  # make the prediction of class
  if(is.null(data)) {
    data = model$data
  }
  mf = model.frame(formula=model$formula, data=data)
  X = model.matrix(attr(mf, "terms"), data=mf)
  coef = as.matrix(t(model$coefficients))
  level = model$level
  yhat = 1/(1+exp(-X %*% coef))
  predict.class = ifelse(yhat < 0.5, level[1], level[2])
  output = list(yhat = as.vector(yhat),
                predict.class = as.vector(predict.class))
  if(print) {
    print(output$predict.class)
  }
  invisible(output)
}
