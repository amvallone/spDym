\name{stz}
\alias{stz}
\title{Standarize a vextor or a matrix}
\description{
Rerurn a numerical vetor standarize to a  0 mean and standrat desviation of 1 districution.
}
\usage{
stz(x)
}
\arguments{
  \item{x}{a numeric vector}
}
\value{
A numeric vector containing the standarization of \code{x}
}
\author{
Andres Vallone
}
\examples{
set.seed(100)
data<-rnorm(1000,2,5)
data.stz<-stz(data)
summary(cbind(data,data.stz))
}
