#'@title PFS similarity measure simWW2
#'@description PFS similarity measure values using simWW2 computation technique with membership,non-membership, and hesitancy values of two objects or set of objects.
#'@param ma PFS membership values for the data set x computed using either triangular or trapezoidal or guassian membership function
#'@param na PFS non-membership values for the data set x computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param ha PFS hesitancy values for the data set x
#'@param mb PFS membership values for the data set y computed using either triangular or trapezoidal or guassian membership function
#'@param nb PFS non-membership values for the data set y computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param hb PFS hesitancy values for the data set y
#'@param k A constant value, considered as 1
#'@return The PFS similarity values of data set y with data set x
#'@references G.Wei and Y.Wei. Similarity measures of pythagorean fuzzy sets based on the cosine function and their applications. International Journal of Intelligent Systems, 33(3):634 - 652, 2018.
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
#'simWW2(ma,na,mb,nb,ha,hb,k)
#'#[1] 0.7061971 0.6841839 0.9511029 0.9511029
#'@export
simWW2<-function(ma,na,mb,nb,ha,hb,k){
  c<-matrix(0,nrow=nrow(ma),ncol=ncol(ma))
  for (i in 1:nrow(ma)) {
    for(j in 1:ncol(ma))
      c[i,j]<-((ma[i,j]^2*mb[k,j]^2)+(na[i,j]^2*nb[k,j]^2)+(ha[i,j]^2*hb[k,j]^2))/(sqrt((ma[i,j]^4)+(na[i,j]^4+ha[i,j]^4))*(sqrt((mb[k,j]^4)+(nb[k,j]^4+hb[k,j]^4))))
  }
  for(j in 1:ncol(c)){
    sum<-(1/ncol(c))*rowSums(c)
  }
  sum
}
