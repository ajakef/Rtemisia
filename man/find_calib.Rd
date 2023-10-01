\name{find_calib}
\alias{find_calib}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
find_calib(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
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
function (x) 
{
    step_x = vector("list", 6)
    print("For each trace, left-click once on each side of the calibration step, if there is one, or right-click once if there is no step")
    for (i in 1:6) {
        data = x[[paste0("CO2_ppm_", i - 1)]]
        plot(x$time, data, type = "l")
        l = locator(2)$x
        print(l)
        if (length(l) == 2) {
            data_trim = runmed(data, 51)[(x$time > min(l)) & 
                (x$time < max(l))]
            w = which(abs(diff(data_trim)) == max(abs(diff(data_trim))))[1]
            step_x[[i]] = x$time[which((x$time > min(l)) & x$time < 
                max(l))[w]]
            print(step_x[i])
        }
    }
    plot_CO2(x)
    for (i in 1:6) {
        lines(c(step_x[[i]], step_x[[i]]), c(0, 1e+06), col = i, 
            lty = "dotted")
    }
    return(step_x)
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
