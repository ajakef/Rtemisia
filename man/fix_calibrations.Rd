% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fix_calibrations.R
\name{fix_calibrations}
\alias{fix_calibrations}
\title{Correct calibration steps for all sensors}
\usage{
fix_calibrations(
  df,
  output_file = NULL,
  corrections_file = NULL,
  key_pattern = "CO2"
)
}
\arguments{
\item{df}{Data frame to plot (output of [read_data()])}

\item{output_file}{.csv file to write as output (default none)}

\item{corrections_file}{.RData file to save corrections (default none)}

\item{key_pattern}{search key to identify channels to correct (e.g., CO2_ppm_[01])}
}
\value{
data frame with calibration steps corrected, optionally with output files written
}
\description{
Data may include approximately weekly "steps" where the CO2 reading changes 
abruptly as part of the sensor's self-calibration routine. For underground
recording, this is undesirable and must be corrected using this function.
Step detections involve manual review using the function's basic GUI to
prevent false positives from being counted.
}
\details{
This function uses a very basic GUI to accept or reject detected
steps. For each step, left-click the plot to accept it, or right-click to
reject. Reject rates of 50% or more are plausible and not cause for concern.
After all detections for a sensor are reviewed, the original and
corrected data will be plotted; if you made any mistakes, you can reject it
by right clicking (and re-do the review for that sensor), or left-click to
accept the corrected data and move on to the next sensor. This is only needed
for CO2 data, not for temperature or humidity. Note that ctrl-c does not
work immediately in interactive mode; it takes effect after the next right
click.
}
\examples{
fix_calibrations()
}
