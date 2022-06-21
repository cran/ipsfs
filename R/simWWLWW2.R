#'@title SFS similarity measure simWWLWW2
#'@description SFS similarity measure values using simWWLWW2 computation technique with membership,non-membership, indeterminacy membership, and refusal membership values of two objects or set of objects.
#'@param ma SFS membership values for the data set x computed using either triangular or trapezoidal or guassian membership function
#'@param na SFS non-membership values for the data set x computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param ia SFS indeterminacy membership values for the data set x
#'@param ra SFS refusal membership values for the data set x
#'@param mb SFS membership values for the data set y computed using either triangular or trapezoidal or guassian membership function
#'@param nb SFS non-membership values for the data set y computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param ib SFS indeterminacy membership values for the data set y
#'@param rb SFS refusal membership values for the data set y
#'@param k A constant value, considered as 1
#'@return The SFS similarity values of data set y with data set x
#'@references G. Wei, J. Wang, M. Lu, J. Wu, and C. Wei. Similarity measures of spherical fuzzy sets based on cosine function and their applications. IEEE Access, 7:159069 - 159080, 2019.
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
#'ia<-imemSFS(ma,na)
#'ra<-rmemSFS(ma,na,ia)
#'mb<-memG(a1,b1,y)
#'nb<-nonmemS(mb,lam)
#'ib<-imemSFS(mb,nb)
#'rb<-rmemSFS(mb,nb,ib)
#'k<-1
#'simWWLWW2(ma,na,mb,nb,ia,ib,ra,rb,k)
#'#[1] 0.7092608 0.6874359 0.9519182 0.9519182
#'@export
simWWLWW2<-function(ma,na,mb,nb,ia,ib,ra,rb,k){
  c<-matrix(0,nrow=nrow(ma),ncol=ncol(ma))
  for (i in 1:nrow(ma)) {
    for(j in 1:ncol(ma))
      c[i,j]<-((ma[i,j]^2*mb[k,j]^2)+(na[i,j]^2*nb[k,j]^2)+(ia[i,j]^2*ib[k,j]^2)+(ra[i,j]^2*rb[k,j]^2))/(sqrt((ma[i,j]^4)+(na[i,j]^4)+(ia[i,j]^4)+(ra[i,j]^4))*(sqrt((mb[k,j]^4)+(nb[k,j]^4)+(ib[k,j]^4)+(rb[k,j]^4))))
  }
  for(j in 1:ncol(c)){
    sum<-(1/ncol(c))*rowSums(c)
  }
  sum
}
