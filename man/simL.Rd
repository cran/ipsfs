% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simL.R
\name{simL}
\alias{simL}
\title{IFS similarity measure simL}
\usage{
simL(ma, na, mb, nb, ha, hb, p, k)
}
\arguments{
\item{ma}{IFS membership values for the data set x computed using either triangular or trapezoidal or guassian membership function}

\item{na}{IFS non-membership values for the data set x computed using either Sugeno and Terano's  or Yager's non-membership function}

\item{mb}{IFS membership values for the data set y computed using either triangular or trapezoidal or guassian membership function}

\item{nb}{IFS non-membership values for the data set y computed using either Sugeno and Terano's  or Yager's non-membership function}

\item{ha}{IFS hesitancy values for the data set x}

\item{hb}{IFS hesitancy values for the data set y}

\item{p}{Lp norm values for measuring the p-norm distance between x and y, values range from 1 to 5}

\item{k}{A constant value, considered as 1}
}
\value{
The IFS similarity values of data set y with data set x
}
\description{
IFS similarity measure values using simL computation technique with membership,non-membership, and hesitancy values of two objects or set of objects.
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
ha<-hmemIFS(ma,na)
mb<-memG(a1,b1,y)
nb<-nonmemS(mb,lam)
hb<-hmemIFS(mb,nb)
k<-1
p<-2
simL(ma,na,mb,nb,ha,hb,p,k)
#[1] 0.7022635 0.6896045 0.8890488 0.8890488
}
\references{
H.-W. Liu. New similarity measures between intuitionistic fuzzy sets and between elements. Mathematical and Computer Modelling, 42(1-2):61 - 70, 2005.
}
