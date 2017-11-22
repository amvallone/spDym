\name{level.mkv}
\alias{level.mkv}
\title{Levelplot of a first order transition matrix}
\description{
Plot the probabilities of the first orden transition matrix for an \code{mkv} object.
}
\usage{
  level.mkv(m,col=c("gray","red"),xlab="Final Period",ylab="Initial Period",title=NULL,
            cell.size=8,axis.size=16,tick.size=14,\dots)
}
\arguments{
\item{m}{a matrix of \eqn{latex}{n x n} or a \code{mkv} object.}
\item{col}{a color vector containing two colours specified as \code{c("low"","high"")}. By Default the \code{low} color is \code{"gray"} and the \code{high} color is \code{"red"}}
\item{xlab}{a \code{character} object conataining the X acis label}
\item{ylab}{a \code{character} object conataining the Y acis label}
\item{title}{a \code{character} object conataining the plot title. By default title=NULL}
\item{cell.size}{an \code{interfer} inidcating the cell text size}
\item{axis.size}{an \code{interfer} inidcating the axis title font size}
\item{tick.size}{an \code{interfer} inidcating the axis ticks font size}
\item{\dots}{Other arguments for \code{ggplot()},\code{theme()} and \code{geom_text()} functions.}
}
\details{
A level plot based in a \code{ggplot()} graph. The level plot show the probabilities of transition form state i to the state j in gradual color from \code{"low"} color to \code{"high"} color, also show the probability of the transition in the (i,j) cell. If the matrix or the \code{mkv} are transitions, the matrix is transformed into a probabilities transition matrix by dividing the row by the row sum. Different \code{ggplot} theme can be creating by setting up a new \code{theme()} object.
}
\value{
A level plot of showing the probabilities of transitions between the differents classes.
}
\author{
Andres Vallone
}
\examples{
set.seed(100)
z<-matrix(sample(c(1:2500),625,replace=TRUE),25,25)
m<-mkv(z,prob=FALSE)
level.mkv(m)
}

