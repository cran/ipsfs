% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simZ.R
\name{simZ}
\alias{simZ}
\title{PFS similarity measure simZ}
\usage{
simZ(ma, na, mb, nb, ha, hb, k)
}
\arguments{
\item{ma}{PFS membership values for the data set x computed using either triangular or trapezoidal or guassian membership function}

\item{na}{PFS non-membership values for the data set x computed using either Sugeno and Terano's  or Yager's non-membership function}

\item{mb}{PFS membership values for the data set y computed using either triangular or trapezoidal or guassian membership function}

\item{nb}{PFS non-membership values for the data set y computed using either Sugeno and Terano's  or Yager's non-membership function}

\item{ha}{PFS hesitancy values for the data set x}

\item{hb}{PFS hesitancy values for the data set y}

\item{k}{A constant value, considered as 1}
}
\value{
The PFS similarity values of data set y with data set x
}
\description{
PFS similarity measure values using simZ computation technique with membership,non-membership, and hesitancy values of two objects or set of objects.
}
\examples{
x<-matrix(c(12,9,14,11,21,16,15,24,20,17,14,11),nrow=4)
y<-matrix(c(11,21,6),nrow=1)
a<-mn(x)
b<-std(x)
a1<-mn(y)
b1<-std(y)
lam<-0.5
ma<-memG(a,b,x)
na<-nonmemS(ma,lam)
ha<-hmemPFS(ma,na)
mb<-memG(a1,b1,y)
nb<-nonmemS(mb,lam)
hb<-hmemPFS(mb,nb)
k<-1
simZ(ma,na,mb,nb,ha,hb,k)
#[1] 0.6128632 0.6335697 0.7722389 0.7722389
}
\references{
X. Zhang. A novel approach based on similarity measure for pythagorean fuzzy multiple criteria group decision making. International Journal of Intelligent Systems, 31(6):593 - 611, 2016.
}
