#' Title
#'
#' @param n the sample size
#' @param iter number of iterations
#' @importFrom graphics hist
#' @importFrom stats runif
#'
#' @return returns the histogram of the sums
#' @export
#'
#' @examples myclt(n=5, iter=1000)
myclt <- function(n, iter) {
  y <- runif(n * iter, 0, 5) # A
  data <- matrix(y, nrow = n, ncol = iter, byrow = TRUE) # B
  sm <- apply(data, 2, sum) # C
  hist(sm)
  sm
}
