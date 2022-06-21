#'@title Standard deviation values
#'@description Standard deviation of the data set for gaussian membership function
#'@param x A data set in the form of document-term matrix
#'@return Standard deviation values for individual row of the input data set X.
#'@examples
#'x<-matrix(c(12,9,14,11,21,16,15,24,20,17,14,11),nrow=4)
#'std(x)
#'#[1] 4.9328829 4.3588989 0.5773503 7.5055535
#'@import stats
#'@export
std<-function(x){
  sv<-c()
  for (i in 1:nrow(x)){
    sv[i]<-sd(as.numeric(x[i,]))
  }
  return(sv)
}
