#'@title IFS similarity measure simSY
#'@description IFS similarity measure values using simSY computation technique with membership,non-membership, and hesitancy values of two objects or set of objects.
#'@param ma IFS membership values for the data set x computed using either triangular or trapezoidal or guassian membership function
#'@param na IFS non-membership values for the data set x computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param ha IFS hesitancy values for the data set x
#'@param mb IFS membership values for the data set y computed using either triangular or trapezoidal or guassian membership function
#'@param nb IFS non-membership values for the data set y computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param hb IFS hesitancy values for the data set y
#'@param k A constant value, considered as 1
#'@return The IFS similarity values of data set y with data set x
#'@references L. Shi and J. Ye. Study on fault diagnosis of turbine using an improved cosine similarity measure for vague sets. Journal of Applied Sciences, 13(10):1781 - 1786, 2013.
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
#'ha<-hmemIFS(ma,na)
#'mb<-memG(a1,b1,y)
#'nb<-nonmemS(mb,lam)
#'hb<-hmemIFS(mb,nb)
#'k<-1
#'simSY(ma,na,mb,nb,ha,hb,k)
#'#[1] 0.8982202 0.8904059 0.9890627 0.9890627
#'@export
simSY<-function(ma,na,mb,nb,ha,hb,k){
  c<-matrix(0,nrow=nrow(ma),ncol=ncol(ma))
  for (i in 1:nrow(ma)) {
    for(j in 1:ncol(ma))
      c[i,j]<-((ma[i,j]*mb[k,j])+(na[i,j]*nb[k,j])+(ha[i,j]*hb[k,j]))/(sqrt((ma[i,j]^2)+(na[i,j]^2)+(ha[i,j]^2))*(sqrt((mb[k,j]^2)+(nb[k,j]^2)+(hb[k,j]^2))))
  }
  for(j in 1:ncol(c)){
    sum<-(1/ncol(c))*rowSums(c)
  }
  sum
}
