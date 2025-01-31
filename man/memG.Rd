% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/memG.R
\name{memG}
\alias{memG}
\title{Gaussian membership function}
\usage{
memG(a, b, x)
}
\arguments{
\item{a}{Mean values of individual rows of the data set x}

\item{b}{Standard deviation values of individual rows of the data set x}

\item{x}{A data set in the form of document-term matrix}
}
\value{
Gaussian membership values for the input data set x.
}
\description{
Gaussian membership function with mean, standard deviation, and data set
}
\examples{
x<-matrix(c(12,9,14,11,21,16,15,24,20,17,14,11),nrow=4)
a<-mn(x)
b<-std(x)
memG(a,b,x)
#       [,1]      [,2]      [,3]
#[1,] 0.5169457 0.7958771 0.8941586
#[2,] 0.5179406 0.9000876 0.7891159
#[3,] 0.8464817 0.5134171 0.8464817
#[4,] 0.8464817 0.5134171 0.8464817
}
