% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DWD_MOSMIX.R
\name{get_files_available}
\alias{get_files_available}
\title{Check Available Forecast Files and Check if Action Required}
\usage{
get_files_available(type = "L", stn = NULL, outdir = "DWDMOS")
}
\arguments{
\item{type}{either \code{L} or \code{S} (see 'Details' section).}

\item{stn}{\code{NULL} or name of the station. Must match the
station naming of the DWD if specified (see \code{\link{get_station_information}}).
See 'Details' section for more information.}

\item{outdir}{name of the output directory where the already-processed
forecasts are stored (see \code{\link{write_ascii}}).}
}
\value{
Returns a \code{data.frame} with initialization times (IssueTime),
name of the XML file plus the full URL, local file name, and a check whether
or not a file has been processed already.
}
\description{
Part of the automation process. This function checks the opendata
server for available files and checks whether this specific forecast
has alreday been processed (checking for existing ASCII files on
the local disc; uses the naming rules of \code{\link{write_ascii}}!).
}
\details{
The DWD provides two sets of MOSMIX forecasts labeled as \code{L}
and \code{S}. \code{L}-forecasts are available four times a day and XML files
for individual stations are available on the opendata server. \code{S}-forecasts
become available 24 times a day and only one XML file is avialable on the
opendata server containing the forecasts for all stations (few thousand stations).

TODO: requires more detailed description.
}
\examples{
# Latest L-type DWD MOSMIX forecast file for Innsbruck Airport
url <- "https://opendata.dwd.de/weather/local_forecasts/mos/MOSMIX_L/single_stations/11120/kml/MOSMIX_L_LATEST_11120.kmz"

# Download and extract file
kmz   <- tempfile("mosmix_demo_", fileext = ".kmz")
check <- download.file(url, kmz)
if ( inherits(check, "try-error") ) stop("Problems downloading the file!")
kml   <- unzip(kmz)

# Parsing the unzipped kml file (XML format)
# And extract required datetime and meta information.
doc <- XML::xmlParse(kml)
datetime <- get_datetime(doc)
meta     <- get_meta_info(doc)
stations <- get_station_information(doc)

# Extracting forecasts now
fcst <- get_forecasts("11120", doc, datetime, meta)
write_ascii(fcst, dir = ".")
write_ascii(fcst, file = "foo.dat", dir = ".")

# Remove kmz and kml file
file.remove(kmz, kml)
}
\author{
Reto Stauffer
}
