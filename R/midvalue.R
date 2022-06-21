#'@title Middle values
#'@description Middle value for triangular or trapezoidal membership function
#'@param x A data set in the form of document-term matrix
#'@return Middle values for the input data set x.
#'@examples
#'x<-matrix(c(12,9,14,11,21,16,15,24,20,17,14,11),nrow=4)
#'midvalue(x)
#'#[1] 16.5 13.0 14.5 17.5
#'@export
midvalue<-function(x){
  mv<-c()
  for (i in 1:nrow(x)){
    mv[i]<-(min(x[i,])+max(x[i,]))/2
  }
  return(mv)
}
