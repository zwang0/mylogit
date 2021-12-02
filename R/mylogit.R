#'mylogit
#'
#'mylogit is used to fit a logistic regression model.
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
mylogit = function(formula, data) {
  # extra x and y
  mf = model.frame(formula=formula, data=data)
  X = model.matrix(attr(mf, "terms"), data=mf)
  y = model.response(mf)
  n = length(y)
  level = levels(y)
  y_num = ifelse(y == level[1], 1, 0) # set y into 1/0
  beta = matrix(0, n, 2) # initialize beta

  while (eps < 0.01)
  p = exp(beta %*% x)

  return(y)
}
