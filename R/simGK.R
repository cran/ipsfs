#'@title IFS similarity measure simGK
#'@description IFS similarity measure values using simGK computation technique with membership, and non-membership values of two objects or set of objects.
#'@param ma IFS membership values for the data set x computed using either triangular or trapezoidal or guassian membership function
#'@param na IFS non-membership values for the data set x computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param mb IFS membership values for the data set y computed using either triangular or trapezoidal or guassian membership function
#'@param nb IFS non-membership values for the data set y computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param k A constant value, considered as 1
#'@return The IFS similarity values of data set y with data set x
#'@references H. Garg and K. Kumar. Distance measures for connection number sets based on set pair analysis and its applications to decision-making process. Applied Intelligence, 48(10):3346 - 3359, 2018.
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
#'simGK(ma,na,mb,nb,k)
#'#[1] 0.1523230 0.1534360 0.6786289 0.6786289
#'@export
simGK<-function(ma,na,mb,nb,k){
  c<-matrix(0,nrow=nrow(ma),ncol=ncol(ma))
  for (i in 1:nrow(ma)) {
    for(j in 1:ncol(ma))
      c[i,j]<-1-(abs((ma[i,j]*(1-na[i,j]))-(mb[k,j]*(1-nb[k,j])))+abs((1-(ma[i,j]*(1-na[i,j]))-(na[i,j]*(1-ma[i,j])))-(1-(mb[k,j]*(1-nb[k,j]))-(nb[k,j]*(1-mb[k,j]))))+abs((na[i,j]*(1-ma[i,j]))-(nb[k,j]*(1-mb[k,j]))))
  }
  c
  for(j in 1:ncol(c)){
    sum=(1/ncol(c))*abs(rowSums(c))
  }
  sum
}
