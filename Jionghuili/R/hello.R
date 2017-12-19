#' what does the function do: Sample Rejection
#'
#'This function implements single variabel rejection sampling for rvs with bounded support and which have have bounded pdf.
#'
#' @param f the pdf that we are sampling from
#' @param n the number of attempted samples.
#' @param l lower bound of support of f
#' @param u upper bound of support of f
#'
#' @return A vector containing samples from pdf
#' @export
#'


pdfRS <- function(f,n){
  if (abs(integrate(f,-Inf,Inf)$val-1)>0.000001){
    stop("Error:the function given is not a pdf.")
  }
else{
      T <- runif(1,min=0, max=1 )
      U <- runif(1,min=0, max=1)
      if(U*M <= f(T))
      {
        OK <- 1
        RN <- c(RN,T)
      }
    }
  }
  return(RN)
}

sampled <- data.frame(proposal = runif(50000,0,1))
sampled$targetDensity <- dbeta(sampled$proposal, 3,6)

maxDens = max(sampled$targetDensity, na.rm = T)
sampled$accepted = ifelse(runif(50000,0,1) < sampled$targetDensity / maxDens, TRUE, FALSE)
