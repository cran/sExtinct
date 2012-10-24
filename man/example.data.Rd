\name{example.data}
\alias{example.data}
\docType{data}
\title{
Example data
}
\description{
A fictional data set of historic sighting events.
}
\usage{data(example.data)}
\format{
  A data frame with 8 observations on the following 2 variables.
  \describe{
    \item{\code{years}}{a numeric vector of the years sights have or have not occured at. Sorted from earliest at the top to latest at the bottom. Restricted to one row per year.}
    \item{\code{sightings}}{a numeric vector of the number of sighting events (if any) that have occured in that year.}
  }
}

\examples{
data(example.data)
example.data
}
\keyword{datasets}
