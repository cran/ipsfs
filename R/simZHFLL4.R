#'@title PFS similarity measure simZHFLL4
#'@description PFS similarity measure values using simZHFLL4 computation technique with membership, and non-membership values of two objects or set of objects.
#'@param ma PFS membership values for the data set x computed using either triangular or trapezoidal or guassian membership function
#'@param na PFS non-membership values for the data set x computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param ha PFS hesitancy values for the data set x
#'@param mb PFS membership values for the data set y computed using either triangular or trapezoidal or guassian membership function
#'@param nb PFS non-membership values for the data set y computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param hb PFS hesitancy values for the data set y
#'@param k A constant value, considered as 1
#'@return The PFS similarity values of data set y with data set x
#'@references Q. Zhang, J. Hu, J. Feng, A. Liu, and Y. Li. New similarity measures of pythagorean fuzzy sets and their applications. IEEE Access, 7:138192 - 138202, 2019.
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
#'ha<-hmemPFS(ma,na)
#'mb<-memG(a1,b1,y)
#'nb<-nonmemS(mb,lam)
#'hb<-hmemPFS(mb,nb)
#'k<-1
#'simZHFLL4(ma,na,mb,nb,ha,hb,k)
#'#[1] 0.4742565 0.4823949 0.7745995 0.7745995
#'@export
simZHFLL4<-function(ma,na,mb,nb,ha,hb,k){
  c<-matrix(0,nrow=nrow(ma),ncol=ncol(ma))
  for (i in 1:nrow(ma)) {
    for(j in 1:ncol(ma))
      c[i,j]<-(2^(1-((1/2)*(abs(ma[i,j]^2-mb[k,j]^2)+abs(na[i,j]^2-nb[k,j]^2)+abs(ha[i,j]^2-hb[k,j]^2)))))-1
  }
  for(j in 1:ncol(c)){
    sum<-(1/ncol(c))*rowSums(c)
  }
  sum
}

