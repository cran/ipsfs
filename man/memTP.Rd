% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/memTP.R
\name{memTP}
\alias{memTP}
\title{Trapezoidal membership function}
\usage{
memTP(a, b, c, d, x)
}
\arguments{
\item{a}{Leftfooting value of the data set x}

\item{b}{Leftshoulder value of the data set x}

\item{c}{Rightshoulder value of the data set x}

\item{d}{Rightfooting value of the data set x}

\item{x}{A data set in the form of document-term matrix}
}
\value{
Trapezoidal membership values for the input data set x.
}
\description{
Trapezoidal membership function with leftfooting, leftshoulder, rightshoulder, rightfooting, and data set
}
\examples{
x<-matrix(c(12,9,14,11,21,16,15,24,20,17,14,11),nrow=4)
a<-leftfootfinding(x,5)
mid<-midvalue(x)
b<-leftshoulderfinding(2.5,mid)
c<-rightshoulderfinding(mid,2.5)
d<-rightfootfinding(x,5)
memTP(a,b,c,d,x)
#       [,1]      [,2]      [,3]
#[1,] 0.5000000 0.6666667 0.8333333
#[2,] 0.7272727 0.8888889 0.6666667
#[3,] 1.0000000 1.0000000 1.0000000
#[4,] 0.2000000 0.2000000 0.2000000
}
