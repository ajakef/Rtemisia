\name{read_one_file}
\alias{read_one_file}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read a single data file
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read_one_file(filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
%%     ~~Describe \code{filename} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (filename) 
{
    header = c("sec_of_file", "CO2_ppm_0", "temp_0", "RH_0", 
        "CO2_ppm_1", "temp_1", "RH_1", "CO2_ppm_2", "temp_2", 
        "RH_2", "CO2_ppm_3", "temp_3", "RH_3", "CO2_ppm_4", "temp_4", 
        "RH_4", "CO2_ppm_5", "temp_5", "RH_5", "year", "month", 
        "date", "hour", "min", "sec", "lat", "lon", "HDOP", "satellites", 
        "batt_voltage", "dummy")
    x = read.table(filename, skip = 5, sep = ",", col.names = header)[, 
        1:30]
    x$time = as.POSIXlt(paste0(x$year, "-", x$month, "-", x$date, 
        " ", x$hour, ":", x$min, ":", x$sec), tz = "GMT")
    return(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
