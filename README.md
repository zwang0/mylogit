# mylogit

<!-- badges: start -->
[![R-CMD-check](https://github.com/zwang0/mylogit/workflows/R-CMD-check/badge.svg)](https://github.com/zwang0/mylogit/actions)
[![codecov](https://codecov.io/gh/zwang0/mylogit/branch/main/graph/badge.svg?token=6P7GAuJ8X3)](https://codecov.io/gh/zwang0/mylogit)
<!-- badges: end -->

This is a github repository for R package *mylogit*. It is used for course BIOSTATS625.

## Introduction

*mylogit* used for fitting logistic regression model. Specifically, the response
variable has two classes. There are two functions in this package:

- *mylogit()*: Fit a logistic regression model with different printing style
- *mylogit.predict()*: Predict the value and class of new data set based on fitted logistic regression model
You may have a brief tutorial about function usage in `vignette`.

## Install
To install the package:
```{r}
devtools::install_github("zwang0/mylogit", build_vignettes = T)
```
## Usage
```{r}
library(mylogit)
n <- nrow(ToothGrowth) # number of observations
data.train = ToothGrowth[1:floor(n*0.7), ] # training dataset
data.test = ToothGrowth[floor(n*0.7)+1:n, ] # testing dataset
myfit = mylogit(supp ~ len, data.train, "detailed") # fit logistic model
myfit.pred = mylogit.predict(myfit, data.test) # make prediction
```
