% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aquatroll.R
\name{read_aquatroll200_file}
\alias{read_aquatroll200_file}
\title{Read a raw Aqua TROLL 200 data file}
\usage{
read_aquatroll200_file(filename, min_timestamp = NULL, quiet = FALSE)
}
\arguments{
\item{filename}{Fully-qualified filename of a raw Aqua TROLL 200 dataset
from a Campbell datalogger}

\item{min_timestamp}{Minimum timestamp to read, character;
function will skip down in the data until approximately this time}

\item{quiet}{Print diagnostic messages? Logical}
}
\value{
A \code{\link[tibble]{tibble}} with the data.
}
\description{
This function uses
\code{\link[readr]{read_csv}} to parse the file into a data frame.
}
\examples{
fn <- system.file("PNNL_21_WaterLevel200.dat", package = "compasstools")
read_aquatroll200_file(fn)
}
\seealso{
\code{\link{process_aquatroll_dir}}
}
\author{
Stephanie Pennington
}
