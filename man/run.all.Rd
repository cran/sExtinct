\name{run.all}
\alias{run.all}

\title{
Run all extinction estimators 
}
\description{
A function that runs all the sighting based extinction predictors within this package. The function can produce diagnostic plots (utilising the package lattice) and produce simple, and detailed data outputs. 
}
\usage{
run.all(data, alpha, test.year, data.out, plot)
}

\arguments{
  \item{data}{
A data.frame with two columns, the first containing the year at which a sighting has (or as not) occured, the second a column of the number of sightings that have occured in each year. Column names are not restricted. Data msut be ranked from oldest at the top, to most recent at the bottom, and restricted to a single row per year. See \code{\link{example.data}} 
}
  \item{alpha}{
A 1-alpha confidence interval is calculated for a given value of alpha, typically set as 0.05 (i.e. a 95\%CI).
}
  \item{test.year}{
A year which chance of persistance is calculated up until, typically set as the current year (i.e. test whether the species is currently predicted to be extant or extinct).
}
  \item{data.out}{
A TRUE/FALSE argument where TRUE produces the predicted chance of persistance at each time step (where applicable), and FALSE gives a simple output with the predicted date of extinction only. 
}
  \item{plot}{
A TRUE/FALSE argument where TRUE produces a diagnostic plot showing the probability of persistance estimated by each method, where applicable, along with the alpha level set.
}
}

\value{
If data.out=TRUE, a data.frame of probability of persistance at each time step, for each method, except OLE which produces a point estimate of extinction only.

If data.out=FALSE, a data.frame of the predicted date of extinction only, produced by each method. NOTE: upper and lower CIs are only calculated OLE, and will be filled in with NAs for the other estimators. 
}
\references{
Burgman, M. A., Grimson, R. C., & Ferson, S. (1995). Inferring Threat from Scientific Collections. Conservation Biology, 9(4), 923-928. 

Roberts, D. L., & Solow, A. R. (2003). Flightless birds: When did the dodo become extinct? Nature, 426(6964), 245. 

Robson, D. S., & Whitlock, J. H. (1964). Estimation of a Truncation Point. Biometrika, 31(1/2), 33-39.

Solow. (1993). Inferring extinction from sighting data. Ecology, 74(3), 962-964.
 
Solow. (2005). Inferring extinction from a sighting record. Mathematical Biosciences, 195(1), 47-55.

Strauss, D., & Sadler, P. M. (1989). Classical confidence intervals and Bayesian probability estimates for ends of local taxon ranges. Mathematical Geology, 21(4), 411-427. 
}
\author{
Christopher Clements
}

\examples{
data(example.data)
run.all(example.data, alpha=0.05, test.year=2012, data.out=FALSE, plot=FALSE)
}