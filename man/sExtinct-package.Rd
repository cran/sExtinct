\name{sExtinct-package}
\alias{sExtinct-package}
\alias{sExtinct}
\docType{package}
\title{sExtinct}
\description{
sExtinct combines several sighting based estimators of historical extinction. 
}
\details{
\tabular{ll}{
Package: \tab sExtinct\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-10-25\cr
License: \tab GPL-2 | GPL-3\cr
}

This package brings together several sighting-based estimators of species loss. These estimators use historical sighting events to calculate the probability of a species persistance. The esitimators can be run simultaneously (using  \code{\link{run.all}}), or individually (see help files). Care should be taken if using low level functions (functions ending in .fun). New users should use intermediate level functions (\code{\link{Robson1964}}, \code{\link{Solow1993.eq2}}, \code{\link{OLE}}) which correct data before running the estimators. 

This is a work in progress, and additional estimators are sought. Please contact author (see below).

}
\author{
Package written by Christopher Clements <c.clements@outlook.com>.

Code for functions was contributed by:

Ben Collen

Gene Hunt

Tracy Rout


Additional code take from the Appendix of McPherson & Myers (2009)

}
\references{
Burgman, M. A., Grimson, R. C., & Ferson, S. (1995). Inferring Threat from Scientific Collections. Conservation Biology, 9(4), 923-928. 

Roberts, D. L., & Solow, A. R. (2003). Flightless birds: When did the dodo become extinct? Nature, 426(6964), 245. 

Robson, D. S., & Whitlock, J. H. (1964). Estimation of a Truncation Point. Biometrika, 31(1/2), 33-39.

Solow. (1993). Inferring extinction from sighting data. Ecology, 74(3), 962-964.
 
Solow. (2005). Inferring extinction from a sighting record. Mathematical Biosciences, 195(1), 47-55.

Strauss, D., & Sadler, P. M. (1989). Classical confidence intervals and Bayesian probability estimates for ends of local taxon ranges. Mathematical Geology, 21(4), 411-427. 

}

\keyword{ package }
\seealso{
\code{\link{run.all}}
\code{\link{Robson1964}}
\code{\link{Solow1993.eq2}}
\code{\link{OLE}}
}
\examples{
data(example.data)
run.all(example.data, 0.05, 2012, FALSE, FALSE)
}