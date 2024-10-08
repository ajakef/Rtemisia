% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_data.R
\name{read_data}
\alias{read_data}
\title{Read data files}
\usage{
read_data(filenames)
}
\arguments{
\item{filenames}{string(s) including paths and filenames (wildcards permitted)}
}
\value{
Data frame including date-time and sensor readings
}
\description{
Reads one or more data files, concatenates them, and returns a data frame.
}
\examples{
read_data()
}
