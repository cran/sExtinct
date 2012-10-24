\name{Solow1993.eq2.fun}
\alias{Solow1993.eq2.fun}
\alias{Solow2005.eq7.fun}
\alias{Burgman.fun}
\title{
Low level functions for Burgman, Solow1993.eq2, Solow2005.eq7
}
\description{
Low level functions on which intermediate level functions are based. Included here for advanced users only. Functions take only a data.frame with two columns, the first containing the year at which a sighting has (or as not) occured, the second a column of the number of sightings that have occured in each year. There must not be missing values in the data frames (i.e. years where sightings=0 must also be included). Functions will calculate the p-value of persistance for the final year in the data frame. To calculate the p-value for the present day the data.frame must contain all the years since the last sighting event with 0 sightings (this is automatically added by the intermediate level functions).
}
\usage{
Burgman.fun(dd)
Solow1993.eq2.fun(dd)
Solow2005.eq7.fun(dd)
}

\arguments{
  \item{dd}{
A data.frame with two columns, the first containing the year at which a sighting has (or as not) occured, the second a column of the number of sightings that have occured in each year. Gaps in sighting history must be filled, see description above.Column names are not restricted. Data must be ranked from oldest at the top, to most recent at the bottom, and restricted to a single row per year.  
}
}

\references{
Burgman, M. A., Grimson, R. C., & Ferson, S. (1995). Inferring Threat from Scientific Collections. Conservation Biology, 9(4), 923-928. 
Solow. (1993). Inferring extinction from sighting data. Ecology, 74(3), 962-964.
Solow. (2005). Inferring extinction from a sighting record. Mathematical Biosciences, 195(1), 47-55.
}
\author{
Christopher Clements
}

\examples{
years <- c(1950:2012)
sightings <- c(1,0,3,4,0,1,0,1,(c(rep(0, 55))))
example.data.for.simple.functions<-data.frame(years, sightings)

Solow2005.eq7.fun(example.data.for.simple.functions)
}