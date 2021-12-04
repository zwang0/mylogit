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
#'@return \item{predict.class}{predicted class}
#'
#'@examples
#'logit = mylogit(formula = supp ~ len + dose, data = ToothGrowth)
#'mylogit.predict(logit, print=TRUE)
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
  coef = as.matrix(model$coefficients)
  level = model$level
  yhat = 1/(1+exp(-X %*% coef))
  predict.class = ifelse(yhat < 0.5, level[1], level[2])
  yhat = as.vector(yhat)
  names(yhat) = 1:length(yhat)
  predict.class = as.vector(predict.class)
  output = list(predict.value = yhat,
                predict.class = predict.class)
  if(print) {
    print(output$predict.class)
  }
  invisible(output)
}
