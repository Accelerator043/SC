#' @importFrom graphics segments lines par
#' @importFrom stats cov qf cor
#' @title Confidence ellipse
#' @description Plot the confidence ellipse of binary normal distribution data
#' @param xdata the data frame
#' @param alpha the fold number of cross-validation
#' @return the plot of the confidence ellipse
#' @examples
#' \dontrun{
#'     ce(ChickWeight[,1:2], 0.95)
#' }
#' @export

ce<-function(xdata,alpha){
  if(ncol(xdata)!=2) stop("Only for bivariate normal")
  n<-nrow(xdata)
  xbar<-colMeans(xdata)
  S<-cov(xdata)
  es<-eigen(S)
  e1<-es$vec %*% diag(sqrt(es$val))
  r1<-sqrt(qf(alpha,2,n-2))*sqrt(2*(n-1)/(n*(n-2)))
  theta<-seq(0,2*pi,len=250)
  v1<-cbind(r1*cos(theta), r1*sin(theta))
  pts<-t(xbar-(e1%*%t(v1)))
  plot(pts,type="l",main="Confidence Region for Bivariate Normal",asp=1)
  segments(0,xbar[2],xbar[1],xbar[2],lty=2) # highlight the center
  segments(xbar[1],0,xbar[1],xbar[2],lty=2)
  
  th2<-c(0,pi/2,pi,3*pi/2,2*pi)   #adding the axis
  v2<-cbind(r1*cos(th2), r1*sin(th2))
  pts2<-t(xbar-(e1%*%t(v2)))
  segments(pts2[3,1],pts2[3,2],pts2[1,1],pts2[1,2],lty=3)  
  segments(pts2[2,1],pts2[2,2],pts2[4,1],pts2[4,2],lty=3)
}