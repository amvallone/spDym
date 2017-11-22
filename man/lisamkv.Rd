\name{lisamkv}
\alias{lisamkv}
\title{
Computa the LISA markov transition matrix
}
\description{
Compute the join posibilities of transition between a spatial unit and his neighbours.}
\usage{
lisamkv(x,W)
}
\arguments{
  \item{x}{a n x t matrix of n saptial units and t periods of time}
  \item{W}{a \code{listw} object conatining the neigbourhood relationship of the spatial units.}
}
\details{
for later.....}
\value{
Return a \code{mkvlisa} object  with two components%%  If it is a LIST, use
  \item{move}{a matrix containing the movement sequence os each spatial unit}
  \item{lisamatrix}{a matrix containing the transitions between the different Moran`s scatterplot quadrants}
 \item{p.lisamatrix }{a matrix containing the probabilities of transitions between the different Moran`s scatterplot quadrants}

}
\references{
Rey, S. J., and Janikas, M. V. (2006). STARS: space-time analysis of regional systems. Geographical analysis, 38(1), 67-86.
}
\author{
Andres Vallone \email{avallone@ucn.cl}
}


\seealso{
\code{\link{mkv},}\code{\link{level.mkv}},\code{\link[spdep]{lag.listw}}
}
\examples{
data(usjoin)
income<-usjoin[,3:dim(usjoin)[2]]
lisamkv.us<-lisamkv(income,W)
print(lisamkv.us$p.lisamatrix)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

