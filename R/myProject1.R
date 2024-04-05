#' myProject1
#'
#' @param N the number of seats available
#' @param gamma the probability of being overbooked
#' @param p the probability of a show
#' @importFrom graphics abline
#' @importFrom graphics points
#' @importFrom stats pbinom
#' @importFrom stats uniroot
#' @return a list of the optimal tickets for normal and discrete distributions
#' @export
#'
#' @examples ntickets(N=400, gamma=0.02, p=0.95)
ntickets <- function(N,gamma,p) {

  #Function to plot Objective vs n for discrete case
  dObjective <- function(xVal, Nf=N, pVal=p, gammaVal=gamma){
    1-gammaVal-pbinom(Nf,xVal,pVal,lower.tail = TRUE, log.p = FALSE)
  }

  #Set x axis based on amount of seats
  x <- N:(N+(N/10))

  #get y values using the function
  y1 <- dObjective(xVal=x)

  # for loop to find the first positive value to give us the root
  j <- 0
  for(i in seq_along(y1)){
    if(y1[i] >= 0){
      j<-i
      break;
    }
  }
  root <- x[j]

  #plot the x and y values with type b to connect with lines and points
  plot(x,y1,type="b", main = paste("Objective Vs n to find optimal tickets sold
                          (", root, ") N =", N, ", gamma =", gamma, " Discrete"))

  #set the horizontal line to 0 and vertical line to the root
  abline(h = 0, v = root, col = "red")

  #Build the discrete points
  discrete_points <- seq(min(x), max(x), by = 1)
  points(discrete_points, dObjective(discrete_points, N, p, gamma), col = "blue", pch = 19)

  #Function to plot of Objective vs n for continuous case
  cObjective <- function(xVal, Nf=N, pVal=p, gammaVal=gamma){
    1-gammaVal-pnorm(Nf+0.5,xVal*pVal,sqrt(xVal*pVal*(1-pVal)),lower.tail = TRUE, log.p = FALSE)
  }

  #set y values by calling the function
  y <- cObjective(xVal=x)

  #find the root using uniroot
  root2 <- uniroot(cObjective, interval = c(min(x), max(x)))$root

  #plot the x values and y values for the continuous plot
  plot(x,y,type="l", main = paste("Objective Vs n to find optimal tickets sold
                      (", root2, ") N =", N, ", gamma =", gamma, " Continuous"))
  #set the horizontal line to 0 and vertical line to root
  abline(h = 0, v = root2, col = "blue")

  #print out the variables
  nameList <- print(list(nd=root, nc=root2, N=N, p=p, gamma=gamma))
}
