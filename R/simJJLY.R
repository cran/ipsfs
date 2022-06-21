#'@title IFS similarity measure simJJLY
#'@description IFS similarity measure values using simJJLY computation technique with membership,non-membership, and hesitancy values of two objects or set of objects.
#'@param ma IFS membership values for the data set x computed using either triangular or trapezoidal or guassian membership function
#'@param na IFS non-membership values for the data set x computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param ha IFS hesitancy values for the data set x
#'@param mb IFS membership values for the data set y computed using either triangular or trapezoidal or guassian membership function
#'@param nb IFS non-membership values for the data set y computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param hb IFS hesitancy values for the data set y
#'@param k A constant value, considered as 1
#'@return The IFS similarity values of data set y with data set x
#'@references Q. Jiang, X. Jin, S.-J. Lee, and S. Yao. A new similarity/distance measure between intuitionistic fuzzy sets based on the transformed isosceles triangles and its applications to pattern recognition. Expert Systems with Applications, 116:439–453, 2019.
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
#'simJJLY(ma,na,mb,nb,ha,hb,k)
#'#[1] 0.7239098 0.7245767 0.8981760 0.8981760
#'@export
simJJLY<-function(ma,na,mb,nb,ha,hb,k){
  c<-matrix(0,nrow=nrow(ma),ncol=ncol(ma))
  for (i in 1:nrow(ma)) {
    for(j in 1:ncol(ma))
      c[i,j]<-abs(((2*(ma[i,j]*hb[k,j])-(mb[k,j])*ha[i,j])-(4*(ma[i,j]-mb[k,j])))/(4-(ha[i,j]*hb[k,j])))+abs(((4*(na[i,j]-nb[k,j]))+(2*(na[i,j]*hb[k,j])-(nb[k,j]*ha[i,j]))+(2*(ha[i,j]-hb[k,j])))/(4-(ha[i,j]*hb[k,j])))
  }
  for(j in 1:ncol(c)){
    sum<-1-(1/(2*ncol(c))*rowSums(c))
  }
  sum
}
