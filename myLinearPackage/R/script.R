#' create a linear regression model
#'
#' This function returns the parameters of the linear regression model for the given data
#'
#' @param Y a vector of outcomes
#' @param X a matrix of covariates
#' @param sub indices of the rows to be used in the analisys
#' @return a list with two vectors:
#'              coef - coefficients from the linear regression formula
#'              pvals - p-values corresponding to the coefficients
#' @export
#' @examples
#' myLinearRegression(data$outcome, data[1:4], 1:100)

myLinearRegression <-  function(Y, X, sub){
  vars <- X[sub,]
  if (length(vars)<5) {
      print("Here is the plot")
      print(GGally::ggpairs(vars))
  }
  else {
      warning("Too many variables to plot")
  }
  data <- data.frame(cbind(Y, vars))
  model <- lm(Y~., data)
  summ <- summary(model)
  return(list(coef=summ$coefficients[,1], pvals=summ$coefficients[,4]))
}


