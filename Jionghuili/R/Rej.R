#' Sample from X given pdf
#'
#' @param f the given fucntions e.g. f = function(x) {dunif(x, -100, 100)}
#' @param n the sample size
#' @param lb lower bound  default -50
#' @param up uper bound defult 50
#'
#'
#' return samples
#'
#'
#'
#'
#'
#' f = function(x) {ifelse(0 < x & x < 1, 4*x^3, 0)}
#' a = rej(f,10000,0,2)
#' ggplot(a,aes(x)) + geom_density() + stat_function(fun = f, color = "red")
#'




rej = function (f, n, lb = -50, ub = 50)
  {
  a=lb
  b=ub
  i= 0
  potentialSamples=c()
  while (i < n) {
    potentialSample = max(replicate(10000,runif(1, a, b)))
    ifelse(runif(1, 0, potentialSample) < f(potentialSample), potentialSample, NA)
    potentialSamples=c(potentialSamples, potentialSample)
    i = i + 1
    }
  return (data.frame(x=potentialSamples))
  }




