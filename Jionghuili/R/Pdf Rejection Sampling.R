#' what does the function do: Sample Rejection
#'
#'This function implements single variabel rejection sampling for rvs with bounded support and which have have bounded pdf.
#'
#' @param f the pdf that we are sampling from
#'
f.x <- function(x) {
  + if (x >= 0 && x < 0.25)
    + 8 * x
  + else if (x >= 0.25 && x <= 1)
    + 8/3 - 8 * x/3
  + else 0
  + }

g.x <- function(x) {
  + if (x >= 0 && x <= 1)
    + 1
  + else 0
  + }

M <- 3
m <- 10000
n.draws <- 0
draws <- c()
x.grid <- seq(0, 1, by = 0.01)

while (n.draws < m) {
  + x.c <- runif(1, 0, 1)
  + accept.prob <- f.x(x.c)/(M * g.x(x.c))
  + u <- runif(1, 0, 1)
  + if (accept.prob >= u) {
    +     draws <- c(draws, x.c)
    +     n.draws <- n.draws + 1
    +     }
  + }
