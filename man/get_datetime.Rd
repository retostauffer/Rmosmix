% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DWD_MOSMIX.R
\name{get_datetime}
\alias{get_datetime}
\title{Extract DWD MOSMIX Datetime Information}
\usage{
get_datetime(doc)
}
\arguments{
\item{doc}{an \code{XMLInternalDocument} object as returned by
\code{xmlParse} (XML package).}
}
\value{
Returns a vector of \code{POSIXt} time stamps.
}
\description{
Extracts the date and time information for which the forecasts
will be valid.
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
doc <- XML::xmlParse(kml)

# Extracting date and time information.
datetime <- get_datetime(doc)
print(datetime)

# Remove kmz and kml file
file.remove(kmz, kml)

}
\seealso{
get_station_information, get_meta_info, get_datetime.
}
\author{
Reto Stauffer
}
