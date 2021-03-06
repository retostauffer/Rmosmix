% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DWD_MOSMIX.R
\name{write_ascii}
\alias{write_ascii}
\alias{write_ascii.dwdforecast}
\title{Store DWD MOSMIX Forecasts As ASCII File}
\usage{
write_ascii(x, ...)

\method{write_ascii}{dwdforecast}(x, file = NULL, dir = "DWDMOS",
  parameter = NULL, ...)
}
\arguments{
\item{x}{an object of class \code{dwdforecast} as returned by
\code{\link{get_forecasts}}.}

\item{...}{forwarded to S3 methods (of main interest:
\code{write_ascii.dwdforecast}).}

\item{file}{\code{NULL} or \code{character} (name of the output file).
If \code{NULL} the output file name will be created automatically
based on the meta information of the object \code{x}.}

\item{dir}{\code{character}, name of the output directory.}

\item{parameter}{default is \code{NULL}. A vector of parameter names
can be provided if only a subset of \code{x} should be written
into the output file.}
}
\description{
Writes a \code{dwdforecast} object (see \code{\link{get_forecasts}})
object into an ASCII file.
}
\details{
Method to store DWD MOSMIX forecasts as ASCII.
The data will be written into the \code{dir} directory. Please onte
that this directory has to be existing. If not, the script will stop.

If a \code{file} name is provided this user defined name will be used
to store the data. If no \code{file} argument is provided the method
takes care of the output structure/file names as follows:
\itemize{
     \item Create a folder inside \code{dir} for each station.
     \item Create one file per DWD MOSMIX initialization time.
     \item Existing files will be replaced.
}
The default output file name format is \code{DWDMOS_<YYYYmmddHHMMSS>_<station>.dat}.

TODO: I should ignore the \code{dir} if \code{file} is provided.
}
\author{
Reto Stauffer

Reto Stauffer
}
