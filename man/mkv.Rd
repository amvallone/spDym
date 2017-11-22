\name{mkv}
\alias{mkv}
\title{First order Markov`s transition matrix}
\description{
Using the maximun likehood estimation compute the Fisrt order Markov transition matrix
}
\usage{
mkv(m,classes=5,fixed=FALSE,type=7,prob=TRUE,...)
}
\arguments{
\item{m}{a n*t matrix conteaining the sequence of states of the variable.}
\item{classes}{a integer inidcating the numbers of classe for the discretization or a vector indicating the numbers of cut in the x for the discretization}
\item{fixed}{Logical. If TRUE, the \code{breaks} are calculed over the entire \eqn{latex}{n*t} pool, otherwise the \code{breaks} are calculed over each period of time.}
\item{type}{
in case of the \code{classes} argument is defined in the [0,1] range, \code{type} is an integer between 1 and 9 selecting one of the nine quantile algorithms to be used. See \code{\link{discret}} for more information.}
\item{prob}{Logial, Should the transition be expressed as probabilities?. Defalut is TRUE.}
\item{\dots}{further arguments passed to or from other methods.}
}
\details{
blablala
}
\value{
Return a \code{mkvm} object.
}
\author{
Andres Vallone
}
\examples{
set.seed(100)
g<-matrix(sample(c(1:2500),625,replace=TRUE),25,25)
m<-mkv(g)
print(m)
}
