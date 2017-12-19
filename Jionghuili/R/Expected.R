#'Expected value of X or X and Y
#'
#'@param f function
#'@param rv random variables
#'
#'
#' betaPDF <- function(x) {
#' ifelse(0 < x & x < 1, 2*x, 0)}
#' x1 = data.frame(oneDsample(f = betaPDF, N=10000, lb = 0, ub = 1))
#' g1 = function(x){x^2}
#' Evalue(g1,x1)
#'
#'
#'




Exv = function (f,rv) {
  len = length(rv)
  if (len==2) {
    meanxy=mean(f(rv$x,rv$y))
    return(meanxy)
  }
  else if (len == 1) {
    meanx=mean(f(rv))
    return(meanx)
  }

}


#'Probability
#'
#'@param cd the given conditions
#'@param data the samples obtained from rejection sampling
#'
#'
#' f <- function(x) {ifelse(0 < x & x < 1, 4*x^3, 0)}
#' x1 = oneDsample(f,10000)
#' c1 = function(x){x<0.4}
#' Gprob(c1,x1)


Gprob <-function(cd,data)
  {
  len = length(data)
  if (len == 2){
    meanxy = mean(cd(data$x,data$y))
    return(meanxy)
  }
  else if (len == 1){
    meanx = mean(cd(data))
    return(meanx)
 }
