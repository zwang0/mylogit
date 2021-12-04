test_that("mylogit works", {
  # run mylogit
  myfit1 = mylogit(supp~len+dose, ToothGrowth, format="Nothing")
  # run glm logit fit
  rfit1 = glm(supp~len+dose, ToothGrowth, family=binomial)
  # test models with different input format
  myfit2 = mylogit(supp~len, ToothGrowth, format="Nothing")
  myfit3 = mylogit(supp~len*dose, ToothGrowth, format="Nothing")
  rfit2 = glm(supp~len, ToothGrowth, family=binomial)
  rfit3 = glm(supp~len*dose, ToothGrowth, family=binomial)
  # fits
  myfits = list(myfit1, myfit2, myfit3)
  rfits = list(rfit1, rfit2, rfit3)

  # set tolerance
  tol = 1e-5

  for (i in 1:3) {
    myfit = myfits[[i]]
    rfit = rfits[[i]]

    # test mylogit function
    expect_equal(myfit$coefficients, rfit$coefficients, tolerance=tol)
    expect_equal(myfit$deviance, rfit$deviance, tolerance=tol)
    expect_equal(myfit$null.deviance, rfit$null.deviance, tolerance=tol)
    expect_equal(myfit$AIC, rfit$aic, tolerance=tol)
    expect_equal(myfit$fitted.values, rfit$fitted.values, tolerance=tol)
    expect_equal(myfit$df, rfit$df.residual)
    expect_equal(myfit$null.df, rfit$df.null)

    # test mylogit.predict function
    TG_level = levels(ToothGrowth$supp)
    myfit.pred = mylogit.predict(myfit)
    rfit.pred = predict(rfit, type="response")
    rfit.class.pred = as.vector(ifelse(rfit.pred < 0.5, TG_level[1], TG_level[2]))

    expect_equal(myfit.pred$predict.class, rfit.class.pred)
    expect_equal(myfit.pred$predict.class, myfit$predict.class)
    expect_equal(myfit.pred$yhat, myfit$fitted.values, tolerance=tol)
    expect_equal(myfit.pred$yhat, rfit.pred, tolerance=tol)

  }
})
