#'@title IFS similarity measure simBA
#'@description IFS similarity measure values using simBA computation technique with membership, and non-membership of two objects or set of objects.
#'@param ma IFS membership values for the data set x computed using either triangular or trapezoidal or guassian membership function
#'@param na IFS non-membership values for the data set x computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param mb IFS membership values for the data set y computed using either triangular or trapezoidal or guassian membership function
#'@param nb IFS non-membership values for the data set y computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param p Lp norm values for measuring the p-norm distance between x and y, values range from 1 to 5
#'@param t Level of uncertainty values, values range from 1 to 10
#'@param k A constant value depends upon the number of rows in the y data set.
#'@return The IFS similarity values of data set y with data set x
#'@references F. E. Boran and D. Akay. A biparametric similarity measure on intuitionistic fuzzy sets with applications to pattern recognition. Information sciences, 255:45 - 57, 2014.
#'@examples #When data set y consist of only one row use k=1
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
#'p<-2
#'t<-2
#'k<-1
#'simBA(ma,na,mb,nb,p,t,k)
#'#0.7072291 0.6947466 0.8919850 0.8919850
#'
#'#When data set y having more than one rows
#'#use k = the number of rows of data set y
#'x<-matrix(c(12,9,14,11,21,16,15,24,20,17,14,11),nrow=4)
#'y<-matrix(c(11,24,21,12,6,11),nrow=2)
#'a<-mn(x)
#'b<-std(x)
#'a1<-mn(y)
#'b1<-std(y)
#'lam<-0.5
#'ma<-memG(a,b,x)
#'na<-nonmemS(ma,lam)
#'mb<-memG(a1,b1,y)
#'nb<-nonmemS(mb,lam)
#'p<-2
#'t<-2
#'sim<-c()
#'for(k in 1:nrow(y)){sim<-rbind(sim,simBA(ma,na,mb,nb,p,t,k))}
#'sim
#'#       [,1]      [,2]      [,3]      [,4]
#'#[1,] 0.7072291 0.6947466 0.8919850 0.8919850
#'#[2,] 0.9410582 0.9843247 0.7380007 0.7380007
#'@export
simBA<-function(ma,na,mb,nb,p,t,k){
  c<-matrix(0,nrow=nrow(ma),ncol=ncol(ma))
  for (i in 1:nrow(ma)) {
    for(j in 1:ncol(ma))
      c[i,j]<-(abs((t*(ma[i,j]-mb[k,j]))-(na[i,j]-nb[k,j]))^p)+(abs((t*(na[i,j]-nb[k,j]))-(ma[i,j]-mb[k,j]))^p)
  }
  for(j in 1:ncol(c)){
    sum<-1-((rowSums(c)/(2*ncol(c)*((t+1)^p)))^(1/p))
  }
  sum
}


