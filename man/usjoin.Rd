\name{USjoin}
\alias{USjoin}
\docType{data}
\title{
US Regional Incomes}
\description{
A \code{dataframe} containing the per-capita income for the lower 48 US states in the priod 1929-2009dir(
. This data set also include  \code{states48.gal} a spatial weigth matrix in a \code{nb} object and \code{W} a \code{listw} object cantaining the list of neighboors for the 48 states.
}
\usage{data("USjoin")}
\format{
  A data frame whith 48 US states.
  \describe{
    \item{\coden{name}}{a \code{character} vector with the name of the state}
    \item{STATE_FIPS}{a state index for mapping functions}
    \item{\code{1929} to \code{2009}} {integer vector conatainin per capita income of the state in each period}
  }
}
\source{
The dataset is sxtracted for the PySAL project web site \url{https://github.com/pysal/pysal/tree/master/pysal/examples/us_income}
}

\examples{
data(USjoin)
## maybe str(USjoin) ; plot(USjoin) ...
}
\keyword{datasets}
