#'@title Mean values
#'@description Mean values of the data set for gaussian membership function
#'@param x A data set in the form of document-term matrix
#'@return Mean values for individual row of the input data set X.
#'@examples
#'x<-matrix(c(12,9,14,11,21,16,15,24,20,17,14,11),nrow=4)
#'mn(x)
#'#[1] 17.66667 14.00000 14.33333 15.33333
#'@export
mn<-function(x){
  mv<-c()
  for (i in 1:nrow(x)){
    mv[i]<-mean(as.numeric(x[i,]))
  }
  return(mv)
}
