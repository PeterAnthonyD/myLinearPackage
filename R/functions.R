#' Create a linear model between y and x(s). For data with less than five (5) x variables, a scattergram of all variables will be created.
#'
#' This is truly a great and much-needed function
#'
#' @param x A vector or matrix of data to predict y
#' @param y A vector or matrix of numbers
#' @param sub The number of rows in the data
#' @return Scattergram of variables. Coeffecients and p-values of linear model.
#' @export
#' @examples
#' myLinearRegression(x, y)

myLinearRegression <- function(x=NULL, y=NULL , sub=NULL){
  x<-as.data.frame(x)
  theData<-as.data.frame(cbind(y,x))
  if (ncol(theData) < 7) {
    p<-ggpairs(data = theData, columns = 1:ncol(theData), title = "Scatterplots of Variables")
  } else {
    p<-ggplot(theData) + labs(title="Too Many Variables")
  }
  theRegression<-lm(y~ ., data = theData)
  variables<-summary(theRegression)$coefficients
  print(variables)
  p
}
