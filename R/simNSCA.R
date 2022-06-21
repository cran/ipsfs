#'@title IFS similarity measure simNSCA
#'@description IFS similarity measure values using simNSCA computation technique with membership, and non-membership values of two objects or set of objects.
#'@param ma IFS membership values for the data set x computed using either triangular or trapezoidal or guassian membership function
#'@param na IFS non-membership values for the data set x computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param mb IFS membership values for the data set y computed using either triangular or trapezoidal or guassian membership function
#'@param nb IFS non-membership values for the data set y computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param k A constant value, considered as 1
#'@return The IFS similarity values of data set y with data set x
#'@references R. T. Ngan, B. C. Cuong, M. Ali, et al. H-max distance measure of intuitionistic fuzzy sets in decision making. Applied Soft Computing, 69:393 - 425, 2018.
#'@examples
#'x<-matrix(c(12,9,14,11,21,16,15,24,20,17,14,11),nrow=4)
#'y<-matrix(c(11,21,6),nrow=1)
#'a<-mn(x)
#'b<-std(x)
#'y<-matrix(c(11,24,21,12,6,11,15,21),nrow=1)
#'a1<-mn(y)
#'b1<-std(y)
#'lam<-0.5
#'ma<-memG(a,b,x)
#'na<-nonmemS(ma,lam)
#'mb<-memG(a1,b1,y)
#'nb<-nonmemS(mb,lam)
#'k<-1
#'simNSCA(ma,na,mb,nb,k)
#'#[1] 0.6928792 0.6934970 0.8754130 0.8754130
#'@export
simNSCA<-function(ma,na,mb,nb,k){
  c<-matrix(0,nrow=nrow(ma),ncol=ncol(ma))
  for (i in 1:nrow(ma)) {
    for(j in 1:ncol(ma))
      c[i,j]<-(abs(ma[i,j]-mb[k,j])+abs(na[i,j]-nb[k,j])+abs(max(ma[i,j],nb[k,j])-max(mb[k,j],na[i,j])))
  }
  for(j in 1:ncol(c)){
    sum=1-((1/(3*ncol(c)))*rowSums(c))
  }
  sum
}
