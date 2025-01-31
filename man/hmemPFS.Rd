% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hmemPFS.R
\name{hmemPFS}
\alias{hmemPFS}
\title{Pythagorean hesitancy membership function}
\usage{
hmemPFS(m, nm)
}
\arguments{
\item{m}{PFS membership values computed using either triangular or trapezoidal or guassian membership function}

\item{nm}{PFS non-membership values computed using either Sugeno and Terano's  or Yager's non-membership function}
}
\value{
PFS hesistancy values
}
\description{
Pythagorean hesitancy membership values with membership and non-membership values as input
}
\examples{
x<-matrix(c(12,9,14,11,21,16,15,24,20,17,14,11),nrow=4)
a<-mn(x)
b<-std(x)
m<-memG(a,b,x)
lam<-0.5
nm<-nonmemS(m,lam)
hmemPFS(m,nm)
#       [,1]      [,2]      [,3]
#[1,] 0.7651357 0.5875871 0.4417361
#[2,] 0.7649349 0.4302263 0.5953393
#[3,] 0.5213768 0.7658251 0.5213768
#[4,] 0.5213768 0.7658251 0.5213768
}
