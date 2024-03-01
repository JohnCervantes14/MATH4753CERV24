#' Title
#'
#' @param mu represents the mean
#' @param sigma the standard deviation
#' @param a probability endpoint, P(Y <= a)
#' @param x NULL
#' @importFrom graphics curve
#' @importFrom graphics polygon
#' @importFrom stats dnorm
#' @importFrom stats pnorm
#'
#' @return a list containing mu, sigma, and area
#' @export
#'
#'
#' @examples myncurve(mu=4, sigma=2, a=6)
myncurve = function(mu, sigma, a, x = NULL){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  xcurve=seq(mu-3*sigma,a,length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)

  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="Blue")

  area=round(pnorm(a,mean=mu,sd=sigma),4)

  list(mu = mu, sigma = sigma, area=area)
}
