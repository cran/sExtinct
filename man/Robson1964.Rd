\name{Robson1964}
\alias{Robson1964}
\alias{Strauss89}
\alias{Robson1964.fun}
\alias{Strauss89.fun}
\alias{Strauss89full.fun}
\title{Robson1964, Strauss89}
\description{
Sighting based estimators of extinction that will calculate p values for persistance at each year after the last sighting event of a species. Unlike Burgman, Solow1993.eq2, Solow2005.eq7 these predictors do not need a test.year.}
\usage{
Robson1964(data, alpha, data.out)
Strauss89(data, alpha, data.out)
}

\arguments{
  \item{data}{
A data.frame with two columns, the first containing the year at which a sighting has (or as not) occured, the second a column of the number of sightings that have occured in each year. Column names are not restricted. Data msut be ranked from oldest at the top, to most recent at the bottom, and restricted to a single row per year. See example.data. 
}
  \item{alpha}{
A 1-alpha confidence interval is calculated for a given value of alpha, typically set as 0.05 (i.e. a 95\%CI).}
  \item{data.out}{
A year which chance of persistance is calculated up until, typically set as the current year (i.e. test whether the species is currently predicted to be extant or extinct).
}
}
\references{
Robson, D. S., & Whitlock, J. H. (1964). Estimation of a Truncation Point. Biometrika, 31(1/2), 33-39.

Strauss, D., & Sadler, P. M. (1989). Classical confidence intervals and Bayesian probability estimates for ends of local taxon ranges. Mathematical Geology, 21(4), 411-427.
}
\author{
Christopher Clements
}
\examples{
	data(example.data)
    Robson1964(example.data, alpha=0.05, data.out=FALSE)
  }

