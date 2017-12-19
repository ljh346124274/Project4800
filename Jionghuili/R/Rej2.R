#' Sample from (X, Y ) given the joint density function
#'
#'
#' @param f joint pdf
#' @param N size
#' @param lbx lower bound x
#' @param ubx uper bound x
#' @param lby lower bound y
#' @param uby uper bound y
#'
#'
#'
#'
#'
#'
#'
#'
#' f <- function(x, y) x + y
#'
#'


rej2 = function (f,N, lbx=-100, ubx= 100, lby=-100, uby=100,...){
  sample1 = max(replicate(10000,f(c(runif(1,lbx,ubx),runif(1,lby,uby)))))
  potentionalSamples=c()
  i=0
  while (i<n){
    potentionalSample = c(runif(1,lbx,ubx),runif(1,lby,uby))
    ifelse (runif(1000,0,sample1)<f(potentionalSample)){
      potentionalSamples=c(potentionalSamples,potentionalSample)
      i = i + 1
    }
    return(data.frame(x=potentionalSamples[1],y=potentionalSamples[2]))

  }
}
