\name{Solow1993.eq2}
\alias{Solow1993.eq2}
\alias{Solow2005.eq7}
\alias{Burgman}
\title{Intermediate level functions to calculate Burgman, Solow1993.eq2, Solow2005.eq7
}
\description{
A collection of functions requiring, in addition to the normal intermediate level inputs of data, alpha and data.out, a test.year value. The test.year (typically set as the current year) is the date that p values of persistance are calculated up to. 
}
\usage{
Burgman(sightingdata, alpha, test.year, data.out)
Solow1993.eq2(sightingdata, alpha, test.year, data.out)
Solow2005.eq7(sightingdata, alpha, test.year, data.out)
}

\arguments{
  \item{sightingdata}{
A data.frame with two columns, the first containing the year at which a sighting has (or as not) occured, the second a column of the number of sightings that have occured in each year. Can be gaps in the data (which are filled in by the function as 0 sightings within that year). Column names are not restricted. Data must be ranked from oldest at the top, to most recent at the bottom, and restricted to a single row per year. See example.data. 
}
  \item{alpha}{
A 1-alpha confidence interval is calculated for a given value of alpha, typically set as 0.05 (i.e. a 95\%CI).}
  \item{test.year}{
A year which chance of persistance is calculated up until, typically set as the current year (i.e. test whether the species is currently predicted to be extant or extinct).
}
  \item{data.out}{
A TRUE/FALSE argument where TRUE produces the predicted chance of persistance at each time step (where applicable), and FALSE gives a simple output with the predicted date of extinction only. 
}
}

\references{
Burgman, M. A., Grimson, R. C., and Ferson, S. (1995). Inferring Threat from Scientific Collections. Conservation Biology, 9(4), 923-928. 
Solow (1993). Inferring extinction from sighting data. Ecology, 74(3), 962-964.
Solow (2005). Inferring extinction from a sighting record. Mathematical Biosciences, 195(1), 47-55.
}
\author{
Christopher Clements

}
\note{
This is a work in progress, to submit a new (to this package) sighting based method for inferring prediction please contact the author (details can be found on www.chrisclementsresearch.co.uk)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Solow1993.eq2.fun}}
}
\examples{
data(example.data)
Burgman(example.data, alpha=0.05, test.year=2012, data.out=FALSE)

}

