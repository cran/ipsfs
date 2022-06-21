#'@title IFS similarity measure simY
#'@description IFS similarity measure values using simY computation technique with membership, and non-membership values of two objects or set of objects.
#'@param ma IFS membership values for the data set x computed using either triangular or trapezoidal or guassian membership function
#'@param na IFS non-membership values for the data set x computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param mb IFS membership values for the data set y computed using either triangular or trapezoidal or guassian membership function
#'@param nb IFS non-membership values for the data set y computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param k A constant value, considered as 1
#'@return The IFS similarity values of data set y with data set x
#'@references J. Ye. Cosine similarity measures for intuitionistic fuzzy sets and their applications. Mathematical and computer modelling, 53(1-2):91 - 97, 2011.
#'@examples
#'x<-matrix(c(12,9,14,11,21,16,15,24,20,17,14,11),nrow=4)
#'y<-matrix(c(11,21,6),nrow=1)
#'a<-mn(x)
#'b<-std(x)
#'a1<-mn(y)
#'b1<-std(y)
#'lam<-0.5
#'ma<-memG(a,b,x)
#'na<-nonmemS(ma,lam)
#'mb<-memG(a1,b1,y)
#'nb<-nonmemS(mb,lam)
#'k<-1
#'simY(ma,na,mb,nb,k)
#'#[1] 0.9024655 0.8950394 0.9898896 0.9898896
#'@export
simY<-function(ma,na,mb,nb,k){
  c<-matrix(0,nrow=nrow(ma),ncol=ncol(ma))
  for (i in 1:nrow(ma)) {
    for(j in 1:ncol(ma))
      c[i,j]<-((ma[i,j]*mb[k,j])+(na[i,j]*nb[k,j]))/(sqrt((ma[i,j]^2)+(na[i,j]^2))*(sqrt((mb[k,j]^2)+(nb[k,j]^2))))
  }
  for(j in 1:ncol(c)){
    sum<-(1/ncol(c))*rowSums(c)
  }
  sum
}
