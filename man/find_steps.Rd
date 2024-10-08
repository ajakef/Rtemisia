% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fix_calibrations.R
\name{find_steps}
\alias{find_steps}
\title{Identify possible calibration steps}
\usage{
find_steps(x, y)
}
\arguments{
\item{x}{Input x values}

\item{y}{Input y values}
}
\value{
list of step x values, beginning and end y values, and step distance
}
\description{
Identify possible calibration steps
}
\examples{
steps = find_steps(1:100, c(rep(0, 30), rep(1, 70)))
}
