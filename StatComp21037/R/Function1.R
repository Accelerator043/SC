#' @importFrom stats lm predict runif rnorm rgamma pbeta rexp rchisq dt qt rbinom rbeta pt uniroot integrate
#' @importFrom graphics hist
#' @importFrom utils read.table
#' @importFrom MASS mvrnorm
#' @import bootstrap
#' @importFrom boot boot.ci boot
#' @title A cross-validation function
#' @description A cross-validation function using R
#' @param df the data frame
#' @param k the fold number of cross-validation
#' @param seed the specific number for set.seed()
#' @param formula.str the formula of the linear regression model like y ~ x
#' @return the estimated error of the cross-validation
#' @examples
#' \dontrun{
#'     X <- cv(k = 5, seed = 123, formula.str = "lpsa ~ lcavol + lweight")
#' }
#' @export

cv<-function(df,k=5,seed=NULL,formula.str){
  set.seed(seed)
  foldid <-sample(rep(1:k, length = 97))
  cv.err<-vector(length=k)
  nameid<-vector(mode="character")
  nameid=unlist(strsplit(formula.str,"[[:space:]]|[[:punct:]]"))
  nameid=nameid[nameid!=""]
  l=length(nameid)
  for (i in 1:k){
    df.cv.train = df[foldid!=i,]
    df.cv.test = df[foldid==i,]
    lm.cv=lm(formula.str,data=df)
    yhat=predict(lm.cv,df.cv.test[,nameid[2:l]])
    cv.err[i]=mean((yhat-df.cv.test$lpsa)^2)
  }
  cv.err.estimate <- mean(cv.err)
  return(cv.err.estimate)
}