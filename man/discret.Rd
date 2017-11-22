\name{discret}
\alias{discret}
\title{Discretizes a continuos variable}
\description{Discretizes a variable in \eqn{latex}{n} states.}
\usage{
discret(x, classes=5, type = 7,...)
}
\arguments{
\item{x}{a numeric vector}
\item{classes}{a integer inidcating the numbers of classe for the discretization or a vector indicating the numbers of cut in the x for the discretization. See Details for more information.}
\item{type}{
an integer between 1 and 9 selecting one of the nine quantile algorithms to be used. See \code{\link{quantile}} for more information.}
\item{\dots}{further arguments passed to or from other methods.}
}
\details{
\code{classes} argument may be express as an integer indication the numebr of classes for the discretization, the cuts are calculated using \code{cut} function setting the \code{breaks} arguments to get the specific number of classes. Also is posibble set the cuts inturcing a sequnce of number in [0,1] or by a vector containing specific values of \code{x} to generate the cut.
}
\value{
A \code{numeric} vector of \eqn{latex}{n} classes defined in \code{classes}
}
\author{
Andres Vallone
}
\examples{
set.seed(100)
x<-rnorm(100)
d.x<-discret(x)
table(d.x)
}
