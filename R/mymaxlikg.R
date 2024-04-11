#' mymaxlikg
#'
#' @param lfun the function to be used
#' @param theta the variable to find the likelihood
#' @importFrom graphics axis
#'
#' @return plot of the max likelihood and probability of a 'head'
#' @export
#'
#' @examples mymaxlikg(theta=seq(0,1,length=10000),
#' lfun=function(theta){log(dbinom(2,prob=theta,size=6)) + log(dbinom(4,prob=theta,size=10))})
mymaxlikg=function(lfun="logbin2",theta) { # default log lik is a combination bin
  nth=length(theta)  # nu. of valuse used in theta
  thmat=matrix(theta,nrow=nth,ncol=1,byrow=TRUE) # Matrix of theta
  z=apply(thmat,1,lfun) # z holds the log lik values
  zmax=max(which(z==max(z)))  # finding the INDEX of the max lik
  plot(theta,exp(z),type="l") # plot of lik
  abline(v=theta[zmax],col="Blue")   #  verical line through max
  axis(3,theta[zmax],round(theta[zmax],4))  # one tick on the third axis
  theta[zmax]   # theta corresponding to max lik
}

