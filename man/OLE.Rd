\name{OLE}
\alias{OLE}
\alias{OLE.fun}
\title{Optimal linear estimation}
\description{
A function that calculates the predcted date of extinction using the optimal lienar estimation technique proposed by Roberts and Solow (2003) and Solow (2005). Differs from other techniques in this package as it requires only data to produce a point estimate of the date of extinction (not dependant on 1-alpha CI), and uses alpha to calculate the upper and lower 1-alpha CI. 
}
\usage{
OLE(sightingdata, alpha)
}

\arguments{
  \item{sightingdata}{
A data.frame with two columns, the first containing the year at which a sighting has (or as not) occured, the second a column of the number of sightings that have occured in each year. Column names are not restricted. Data msut be ranked from oldest at the top, to most recent at the bottom, and restricted to a single row per year. See example.data. 
}
  \item{alpha}{
A 1-alpha confidence interval is calculated for a given value of alpha, typically set as 0.05 (i.e. a 95\% CI). OLE produces an upper and lower confidence interval estimate, the lower obviously bound by the last sighting event. See Clements et al. for a discussion on the use of 95\% CI's produced by OLE.
}
}

\references{
Clements, C. F., Worsfold, N., Warren, P., Collen, B., Blackburn, T., Clark, N., & Petchey, O. L. Experimentally testing an extinction estimator: Solows Optimal Linear Estimation model. Journal of Animal Ecology, In press.

Roberts, D. L., & Solow, A. R. (2003). Flightless birds: When did the dodo become extinct? Nature, 426(6964), 245. 

Solow. (2005). Inferring extinction from a sighting record. Mathematical Biosciences, 195(1), 47-55.
}
\author{Christopher Clements}

\examples{
	data(example.data)
    OLE.fun(example.data, alpha=0.05)
}
 