#'@title SFS similarity measure simSGFDK6
#'@description SFS similarity measure values using simSGFDK6 computation technique with membership,non-membership, indeterminacy membership, and refusal membership values of two objects or set of objects.
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
#'@references S. A. S. Shishavan, F. K. Gundogdu, E. Farrokhizadeh, Y. Donyatalab, and C. Kahraman. Novel similarity measures in spherical fuzzy environment and their applications. Engineering Applications of Artificial Intelligence, 94:103837, 2020.
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
#'simSGFDK6(ma,na,mb,nb,ia,ib,ra,rb,k)
#'#[1] 0.6563487 0.6334610 0.7893601 0.7893601
#'@export
simSGFDK6<-function(ma,na,mb,nb,ia,ib,ra,rb,k){
  c<-matrix(0,nrow=nrow(ma),ncol=ncol(ma))
  for (i in 1:nrow(ma)) {
    for(j in 1:ncol(ma))
      c[i,j]<-((ma[i,j]-mb[k,j])^2+(na[i,j]-nb[k,j])^2+(ia[i,j]-ib[k,j])^2+(ra[i,j]-ra[k,j])^2)
  }
  for(j in 1:ncol(c)){
    sum<-exp(-sqrt((1/ncol(c))*(rowSums(c))))
  }
  sum
}

