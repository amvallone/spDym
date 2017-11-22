\name{print.mkvm}
\alias{print.mkvm}
\title{Print the first order Markov`s transition matrix}
\description{
 print the first order Markov`s transition matrix comptuing with \code{mokv()} function.
}
\usage{
 \method{print}{mkvm}(x,digits=4,\dots)
}
\arguments{
\item{x}{A \code{mkvm} object}
\item{digits}{Integer inidcating the numbers of digit to print}
\item{\dots}{Other argument for \code{print()} function}
}
\author{
Andres Vallone
}
\examples{
set.seed(100)
z<-matrix(sample(c(1:2500),625,replace=TRUE),25,25)
m<-mkv(z)
print(m)
}
