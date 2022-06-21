#'@title PFS similarity measure simP
#'@description PFS similarity measure values using simP computation technique with membership, and non-membership values of two objects or set of objects.
#'@param ma PFS membership values for the data set x computed using either triangular or trapezoidal or guassian membership function
#'@param na PFS non-membership values for the data set x computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param mb PFS membership values for the data set y computed using either triangular or trapezoidal or guassian membership function
#'@param nb PFS non-membership values for the data set y computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param p Lp norm values for measuring the p-norm distance between x and y, values range from 1 to 5
#'@param a Level of uncertainty values, values range from 1 to 10
#'@param b Level of uncertainty values, values range from 1 to 10
#'@param t Level of uncertainty values, values range from 1 to 10
#'@param k A constant value, considered as 1
#'@return The PFS similarity values of data set y with data set x
#'@references X. Peng. New similarity measure and distance measure for pythagorean fuzzy set. Complex & Intelligent Systems, 5(2):101 - 111, 2019.
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
#'a<-2
#'b<-2
#'p<-2
#'t<-2
#'k<-1
#'simP(ma,na,mb,nb,a,b,p,t,k)
#'#[1] 0.7007663 0.6879639 0.8834981 0.8834981
#'@export
simP<-function(ma,na,mb,nb,a,b,p,t,k){
  c<-matrix(0,nrow=nrow(ma),ncol=ncol(ma))
  for (i in 1:nrow(ma)) {
    for(j in 1:ncol(ma))
      c[i,j]<-(abs(((t+1-a)*(ma[i,j]^2-mb[k,j]^2))-(a*(na[i,j]^2-nb[k,j]^2)))^p)+(abs(((t+1-b)*(na[i,j]^2-nb[k,j]^2))-(b*(ma[i,j]^2-mb[k,j]^2)))^p)
  }
  for(j in 1:ncol(c)){
    sum<-1-((rowSums(c)/(2*ncol(c)*((t+1)^p)))^(1/p))
  }
  sum
}
