

#' Load DWD MOS Parameter Description
#'
#' An XML file on the opendata server provides information about
#' the forecasted elements.
#'
#' @param ... unused.
#'
#' @return Returns a \code{data.frame} with three columns containing
#' the name of the parameter (\code{name}), the unit (\code{unit}),
#' and a description of the parameter (\code{description}).
#'
#' @examples
#' # Loading information
#' desc <- parameter_description()
#' print(head(desc))
#'
#' @export
#' @author Reto Stauffer
#' @import XML
#' @importFrom utils download.file
# TODO: UTF-8 handling (mainly for the units)
parameter_description <- function(...) {

    url <- "https://opendata.dwd.de/weather/lib/MetElementDefinition.xml"

    # Downloading file
    tmpfile <- tempfile()
    download.file(url, tmpfile, method = "curl", quiet = TRUE)

    # Parse XML file
    doc <- xmlParse(tmpfile)
    nodes <- xpathApply(doc, "//MetElement")

    # Extracting required information
    fun <- function(x) {
        name <- xpathSApply(x, "ShortName", xmlValue)
        unit <- xpathSApply(x, "UnitOfMeasurement", xmlValue)
        desc <- xpathSApply(x, "Description", xmlValue)
        data.frame(name = name, unit = unit, description = desc)
    }
    res <- do.call(rbind, lapply(nodes, fun))

    free(doc)
    # Remove temporary file and return data.frame with the 
    # required information.
    file.remove(tmpfile)
    return(res)

}


#' Extract DWD MOSMIX Forecast Meta Information
#'
#' The DWD MOSMOX forecasts are provided as XML files. The "header"
#' of the XML file contains some information about the MOSMIX model,
#' initialization time, and so far and so on. This method extracts
#' the meta information.
#'
#' @param doc an \code{XMLInternalDocument} object as returned by
#'    \code{xmlParse} (XML package).
#' @param x an object of class \code{dwdmeta} as returned by
#'    \code{\link{get_meta_info}}.
#' @param ... forwarded to S3 methods. Unused for now.
#'
#' @details Extracts the following information from the XML file:
#' \itemize{
#'      \item \code{Issuer} who created these forecasts.
#'      \item \code{ProductID}
#'      \item \code{GeneratingProcess} 
#'      \item \code{IssueTime} the initialization time (most important
#'            information). Will also be used when creating the output files
#'            (see \code{\link{write_ascii}}).
#'      \item \code{ReferencedModel} the NWP models the MOS is based on.
#' }
#'
#' @seealso get_station_information, get_meta_info, get_datetime.
#'
#' @return Returns an object of class \code{dwdmeta}.
#'
#' @examples
#' # Latest L-type DWD MOSMIX forecast file for Innsbruck Airport
#' url <- "https://opendata.dwd.de/weather/local_forecasts/mos/MOSMIX_L/single_stations/11120/kml/MOSMIX_L_LATEST_11120.kmz"
#'
#' # Download and extract file
#' kmz   <- tempfile("mosmix_demo_", fileext = ".kmz")
#' check <- download.file(url, kmz)
#' if ( inherits(check, "try-error") ) stop("Problems downloading the file!")
#' kml   <- unzip(kmz)
#'
#' # Parsing the unzipped kml file (XML format)
#' doc <- XML::xmlParse(kml)
#'
#' # Extracting meta information
#' meta <- get_meta_info(doc)
#' print(meta)
#'
#' # Remove kmz and kml file
#' file.remove(kmz, kml)
#'
#' @export
#' @import XML
#' @author Reto Stauffer
get_meta_info <- function(doc) {
    fun <- function(doc, path) paste(xpathApply(doc, path, xmlValue), sep = ", ")
    keys <- c("Issuer", "ProductID", "GeneratingProcess", "IssueTime")
    res <- structure(lapply(keys, function(x, doc) fun(doc, sprintf("//dwd:ProductDefinition/dwd:%s", x)), doc = doc),
                     names = keys)
    getmodels <- function(doc) {
        tmp <- xpathApply(doc, "//dwd:ProductDefinition/dwd:ReferencedModel/dwd:Model")
        tmp <- lapply(tmp, function(x) { x <- as.list(xmlAttrs(x)); data.frame(init = x$referenceTime, name = x$name) })
        tmp <- do.call(rbind, tmp)
        tmp$init <- as.POSIXct(strptime(tmp$init, "%Y-%m-%dT%H:%M:%SZ"))
        tmp
    }
    res$ReferencedModel <- getmodels(doc)
    res$IssueTime       <- as.POSIXct(strptime(res$IssueTime, "%Y-%m-%dT%H:%M:%S.000Z"))
    class(res) <- c("dwdmeta")
    return(res)
}


#' Extract DWD MOSMIX Station Information
#'
#' Extracts the station/location information from the XML files.
#'
#' @param doc an \code{XMLInternalDocument} object as returned by
#'    \code{xmlParse} (XML package).
#'
#' @return Returns a \code{SpatialPointsDataFrame} object with
#' station location information (the coordinates), name and
#' description of the location plus the altitude of the location.
#'
#' @seealso get_station_information, get_meta_info, get_datetime.
#'
#' @examples
#' # Latest L-type DWD MOSMIX forecast file for Innsbruck Airport
#' url <- "https://opendata.dwd.de/weather/local_forecasts/mos/MOSMIX_L/single_stations/11120/kml/MOSMIX_L_LATEST_11120.kmz"
#'
#' # Download and extract file
#' kmz   <- tempfile("mosmix_demo_", fileext = ".kmz")
#' check <- download.file(url, kmz)
#' if ( inherits(check, "try-error") ) stop("Problems downloading the file!")
#' kml   <- unzip(kmz)
#'
#' # Parsing the unzipped kml file (XML format)
#' doc <- XML::xmlParse(kml)
#'
#' # Extracting station information
#' # NOTE that, for this example, we are downloading the latest forecasts
#' # for one station (Innsbruck, 11120). Thus, the station information
#' # returned by get_station_information will also only contain this
#' # one station.
#' stations <- get_station_information(doc)
#' print(stations)
#'
#' # Remove kmz and kml file
#' file.remove(kmz, kml)
#'
#' @export
#' @import XML
#' @importFrom sp CRS SpatialPointsDataFrame
#' @author Reto Stauffer
get_station_information <- function(doc) {
    name <- xpathSApply(doc, "//kml:Placemark/kml:name", xmlValue)
    desc <- xpathSApply(doc, "//kml:Placemark/kml:description", xmlValue)
    pos  <- xpathSApply(doc, "//kml:Placemark/kml:Point/kml:coordinates", xmlValue)
    fun  <- function(x)
        structure(data.frame(t(eval(parse(text = sprintf("c(%s)", x))))), names = c("lon", "lat", "alt"))
    pos  <- do.call(rbind, lapply(pos, fun))

    res <- cbind(data.frame(name = name, desc = desc), pos)
    SpatialPointsDataFrame(subset(res, select = c(lon,lat)), data = subset(res, select = -c(lon,lat)),
                           proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0 +no_defs"))

}

#' @importFrom utils globalVariables
globalVariables(c("lon", "lat"))

#' Extract DWD MOSMIX Datetime Information 
#'
#' Extracts the date and time information for which the forecasts
#' will be valid.
#'
#' @param doc an \code{XMLInternalDocument} object as returned by
#'    \code{xmlParse} (XML package).
#'
#' @return Returns a vector of \code{POSIXt} time stamps.
#'
#' @seealso get_station_information, get_meta_info, get_datetime.
#'
#' @examples
#' # Latest L-type DWD MOSMIX forecast file for Innsbruck Airport
#' url <- "https://opendata.dwd.de/weather/local_forecasts/mos/MOSMIX_L/single_stations/11120/kml/MOSMIX_L_LATEST_11120.kmz"
#'
#' # Download and extract file
#' kmz   <- tempfile("mosmix_demo_", fileext = ".kmz")
#' check <- download.file(url, kmz)
#' if ( inherits(check, "try-error") ) stop("Problems downloading the file!")
#' kml   <- unzip(kmz)
#'
#' # Parsing the unzipped kml file (XML format)
#' doc <- XML::xmlParse(kml)
#'
#' # Extracting date and time information.
#' datetime <- get_datetime(doc)
#' print(datetime)
#'
#' # Remove kmz and kml file
#' file.remove(kmz, kml)
#'
#' @export
#' @import XML
#' @author Reto Stauffer
get_datetime <- function(doc) {
    tmp <- sapply(xpathApply(doc, "//dwd:ForecastTimeSteps/dwd:TimeStep"), xmlValue)
    strptime(tmp, "%Y-%m-%dT%H:%M:%S.000Z")
}



#' Extract DWD MOSMIX Forecast Data
#'
#' This is the reason this package exists. Extracts the DWD MOSMIX
#' forecasts for a specific station/location.
#'
#' @param station \code{character} or \code{factor} containing the
#'    name of the station (e.g., \code{"11120"}, \code{"06660"}).
#'    The name has to match the name as provided by the XML file
#'    (see \code{\link{get_station_information}}).
#' @param doc an \code{XMLInternalDocument} object as returned by
#'    \code{xmlParse} (XML package).
#' @param datetime vector of time stamps for which the forecasts
#'     are valid as returned by \code{\link{get_datetime}}.
#' @param meta object of class \code{dwdmeta} as returned by
#'     \code{\link{get_meta_info}}.
#' @param parameter can be \code{NULL}, \code{character}, or a
#'     vector of \code{character}. If not set (\code{parameter = NULL})
#'     all forecast parameters will be returned containing data.
#'     If set, only a subset will be returned (see 'Details' section).
#' @param as.zoo logical, default \code{TRUE}. If \code{FALSE} a
#'     \code{data.frame} instead of a \code{zoo} object will be returned.
#' @param x an object of class \code{dwdforecast} as returned by
#'     \code{\link{get_forecasts}}.
#' @param ... forwarded to S3 methods. Unused for now.
#'
#' @details Extracts the forecasted values for a given location.
#' If \code{as.zoo = FALSE} a \code{data.frame} will be returned
#' containing the date/time for which the forecasts are valid
#' in the first column (\code{datetime}).
#'
#' If \code{as.zoo = TRUE} a \code{zoo} object is returned. In this
#' case the \code{datetime} column (see above) will be removed.
#' 
#' The \code{parameter} input argument allows to subset the data.
#' If \code{parameter = NULL} all parameters will be returned
#' which provide at least one valid value. Columns containing
#' missing values only will be removed automatically.
#' \code{parameter} can be used to subset the data. Only the user
#' specified columns will be returned. Note that this could result
#' in an empty object if no values are available given the
#' \code{parmaeter} specification!
#'
#'
#' @seealso get_station_information, get_meta_info, get_datetime, parameter_description.
#'
#' @return Returns a \code{data.frame} or \code{zoo} object
#' (depending on input \code{as.zoo}) with the DWD MOSMIX forecasts.
#'
#'
#' @examples
#' # Latest L-type DWD MOSMIX forecast file for Innsbruck Airport
#' url <- "https://opendata.dwd.de/weather/local_forecasts/mos/MOSMIX_L/single_stations/11120/kml/MOSMIX_L_LATEST_11120.kmz"
#'
#' # Download and extract file
#' kmz   <- tempfile("mosmix_demo_", fileext = ".kmz")
#' check <- download.file(url, kmz)
#' if ( inherits(check, "try-error") ) stop("Problems downloading the file!")
#' kml   <- unzip(kmz)
#'
#' # Parsing the unzipped kml file (XML format)
#' # And extract required datetime and meta information.
#' doc <- XML::xmlParse(kml)
#' datetime <- get_datetime(doc)
#' meta     <- get_meta_info(doc)
#' stations <- get_station_information(doc)
#'
#' # Extracting forecasts now
#' # - parameter = NULL:    extracts all parameters
#' # - as.zoo = TRUE:       returns a time series object.
#' fcst1 <- get_forecasts("11120", doc, datetime, meta)
#' print(fcst1)
#' print(class(fcst1))
#' print(head(fcst1[,1:5]))
#'
#' # Extracting forecasts now
#' # - parameter = NULL:    extracts all parameters
#' # - as.zoo = FALSE:      returns a data.frame
#' fcst2 <- get_forecasts("11120", doc, datetime, meta, as.zoo = FALSE)
#' print(fcst2)
#' print(class(fcst2))
#' print(head(fcst2[,1:5]))
#'
#' # Extracting forecasts now
#' # - parameter is set:    return 2m temperature and 2m dewpoint
#' #                        temperature only (see parameter_description method)
#' # - as.zoo = TRUE:       returns a time series object.
#' fcst3 <- get_forecasts("11120", doc, datetime, meta, parameter = c("TTT", "Td"))
#' print(fcst3)
#' print(class(fcst3))
#' print(head(fcst3))
#' zoo::plot.zoo(fcst3 - 273.15, screen = 1, col = c("red", "green"),
#'      xlab = "date/time", ylab = "TTT/Td [degrees Celsius]",
#'      main = "DWD MOSMIX Forecasts for Station Innsbruck Airport (11120)")
#'
#' # Remove kmz and kml file
#' file.remove(kmz, kml)
#'
#' @export
#' @import XML
#' @import zoo
#' @author Reto Stauffer
# TODO: meta/datetime should be extracted internally, that should not take too long!
# TODO: station handling should be nicer, currently it has to be this string but we could
# also accept integers and check if we can find forecast data or not (and warn if not).
get_forecasts <- function(station, doc, datetime, meta, parameter = NULL, as.zoo = TRUE) {
    stopifnot(inherits(station, c("character", "factor")))
    station <- as.character(station)
    stopifnot(length(station) == 1)
    cat(sprintf("Processing station %s\n", station))
    path  <- sprintf("//kml:Placemark[kml:name = %s]//dwd:Forecast", station)
    nodes <- xpathApply(doc, path)#, xmlValue)
    elem  <- sapply(nodes, function(x) as.character(xmlAttrs(x)["elementName"]))
    # No forecasts? Return NULL
    if ( length(elem) == 0 ) return(NULL)
    fun <- function(x) {
        x <- xmlValue(x);
        x <- regmatches(x, gregexpr("[\\S]+", x, perl = TRUE))[[1]]
        if ( sum(! grepl("^(-|[0-9\\.]+)$", x)) > 0 )
            stop("Unexpected element in: %s\n", paste(x, collapse = " "))
        x[grep("^-$", x)] <- NA
        as.numeric(x)
    }
    fcst <- lapply(nodes, fun)
    fcst <- structure(as.data.frame(do.call(cbind, fcst)), names = elem)
    fcst <- cbind(data.frame(datetime = datetime), fcst)
    # Drop columns with no data 
    idx <- which(apply(fcst, 2, function(x) sum(!is.na(x)) == 0))
    if ( length(idx) > 0 ) fcst <- fcst[,-idx]
    # Subsetting if required
    if ( ! is.null(parameter) ) {
        idx <- which(names(fcst) %in% c("datetime", parameter))
        fcst <- fcst[,idx]
    }
    if ( as.zoo ) fcst <- zoo::zoo(subset(fcst, select = -c(datetime)), fcst$datetime)
    class(fcst) <- c("dwdforecast", class(fcst))
    attr(fcst, "meta")    <- meta
    # Fetching station information again
    path <- sprintf("//kml:Placemark[kml:name = %s]", station)
    attr(fcst, "name")        <- xpathSApply(doc, sprintf("%s/%s", path, "kml:name"), xmlValue)
    attr(fcst, "description") <- xpathSApply(doc, sprintf("%s/%s", path, "kml:description"), xmlValue)
    attr(fcst, "coordinates") <- xpathSApply(doc, sprintf("%s/%s", path, "kml:Point/kml:coordinates"), xmlValue)
    return(fcst)
}


#' @rdname get_meta_info
#' @export
#' @author Reto Stauffer
print.dwdmeta <- function(x, ...) {
    res <- c()
    for ( n in names(x) ) {
        if ( n == "ReferencedModel" ) {
            res <- c(res, "ReferencedModel(s)")
            for ( i in 1:nrow(x$ReferencedModel) ) {
                res <- c(res, sprintf("- %s, %s",
                         strftime(x$ReferencedModel$init[i], "%Y-%m-%d %H:%M:%S UTC"),
                         x$ReferencedModel$name[i]))
            }
        } else if ( inherits(x[[n]], c("Date", "POSIXt")) ) {
            res <- c(res, sprintf("%s: %s", n, strftime(x[[n]], "%Y-%m-%d %H:%M:%S UTC")))
        } else {
            res <- c(res, sprintf("%s: %s", n, x[[n]]))
        }
    }
    class(res) <- "dwdmeta.print"
    res
}
print.dwdmeta.print <- function(x) cat(paste(x, collapse = "\n"), "\n")


#' @rdname get_forecasts
#' @author Reto Stauffer
#' @import zoo
#' @export
print.dwdforecast <- function(x, ...) {
    cat("DWD MOS Forecast Object\n\n")
    meta <- attr(x, "meta")
    print(print(attr(x, "meta"))); cat("\n")
    cat("Object is of class zoo:  ", zoo::is.zoo(x), "\n")
    if ( zoo::is.zoo(x) ) { tmp <- strftime(range(zoo::index(x)), "%Y-%m-%d %H:%M:%S") }
    else                  { tmp <- strftime(range(x$datetime),    "%Y-%m-%d %H:%M:%S") }
    cat("Station identifier:      ", attr(x, "name"), "\n")
    cat("Station description:     ", attr(x, "description"), "\n")
    cat("Station location:        ", attr(x, "coordinates"), "(lon/lat/alt)\n")
    cat("First forecast for:      ", tmp[1L], "\n")
    cat("Last forecast for:       ", tmp[2L], "\n")
    cat("Number forecasts (time): ", nrow(x), "\n")
    cat("Number of parameters:    ", ncol(x), "\n")
}


#' Store DWD MOSMIX Forecasts As ASCII File
#'
#' Writes a \code{dwdforecast} object (see \code{\link{get_forecasts}})
#' object into an ASCII file.
#'
#' @param x an object of class \code{dwdforecast} as returned by
#'      \code{\link{get_forecasts}}.
#' @param file \code{NULL} or \code{character} (name of the output file).
#'      If \code{NULL} the output file name will be created automatically
#'      based on the meta information of the object \code{x}.
#' @param dir \code{character}, name of the output directory.
#' @param parameter default is \code{NULL}. A vector of parameter names
#'      can be provided if only a subset of \code{x} should be written
#'      into the output file.
#' @param ... forwarded to S3 methods (of main interest:
#'      \code{write_ascii.dwdforecast}).
#'
#' @details Method to store DWD MOSMIX forecasts as ASCII.
#' The data will be written into the \code{dir} directory. Please onte
#' that this directory has to be existing. If not, the script will stop.
#'
#' If a \code{file} name is provided this user defined name will be used
#' to store the data. If no \code{file} argument is provided the method
#' takes care of the output structure/file names as follows:
#' \itemize{
#'      \item Create a folder inside \code{dir} for each station.
#'      \item Create one file per DWD MOSMIX initialization time.
#'      \item Existing files will be replaced.
#' }
#' The default output file name format is \code{DWDMOS_<YYYYmmddHHMMSS>_<station>.dat}.
#'
#' TODO: I should ignore the \code{dir} if \code{file} is provided.
#'
#' @export
#' @author Reto Stauffer
write_ascii <- function(x, ...) UseMethod("write_ascii")

#' @rdname write_ascii
#' @author Reto Stauffer
#' @import zoo
#' @importFrom utils write.table
#' @export
write_ascii.dwdforecast <- function(x, file = NULL, dir = "DWDMOS", parameter = NULL, ...) {

    # Stop if output dir does not exist
    stopifnot(dir.exists(dir))

    # Keep meta information
    meta      <- as.vector(print(attr(x, "meta")))
    itime     <- attr(x, "meta")$IssueTime
    stn_name  <- attr(x, "name")
    stn_desc  <- attr(x, "description")
    stn_coord <- attr(x, "coordinates")

    # Ouptut file name
    if ( is.null(file) ) {
        dir <- sprintf("%s/%s", dir, stn_name)
        if ( ! dir.exists(dir) ) dir.create(dir)
        file <- sprintf("%s/DWDMOS_%s_%s.dat", dir, strftime(itime, "%Y%m%d%H%M%S"), attr(x, "name"))
    }
    cat(sprintf("Write DWD MOS Forecasts into \"%s\"\n", file))

    # Convert datetime information to unix time stamp (integer)
    # and convert to data.frame (if is.zoo)
    if ( zoo::is.zoo(x) ) {
        x <- cbind(data.frame(timestamp = as.numeric(zoo::index(x))), as.data.frame(x))
        rownames(x) <- NULL
    } else {
        x <- cbind(data.frame(timestamp = as.numeric(x$datetime)), x)
        x$datetime <- NULL
    }

    # Subsetting data?
    if ( ! is.null(parameter) ) {
        idx <- which(names(x) %in% c("timestamp", parameter))
        x <- x[,idx]
    }

    # Print header information
    write(sprintf("# %s", meta), file = file, append = FALSE)
    write(sprintf("# Station identifier:  %s", stn_name), file = file, append = TRUE)
    write(sprintf("# Station description: %s", stn_desc), file = file, append = TRUE)
    write(sprintf("# Station location:    %s (lon/lat/alt)", stn_coord), file = file, append = TRUE)
    suppressWarnings(
        write.table(x, file = file, append = TRUE, quote = FALSE, sep = ";",
                    col.names = TRUE, row.names = FALSE)
    )
}


#' Check Available Forecast Files and Check if Action Required
#'
#' Part of the automation process. This function checks the opendata
#' server for available files and checks whether this specific forecast
#' has alreday been processed (checking for existing ASCII files on
#' the local disc; uses the naming rules of \code{\link{write_ascii}}!).
#' 
#' @param type either \code{L} or \code{S} (see 'Details' section).
#' @param stn \code{NULL} or name of the station. Must match the
#'     station naming of the DWD if specified (see \code{\link{get_station_information}}).
#'     See 'Details' section for more information.
#' @param outdir name of the output directory where the already-processed
#'     forecasts are stored (see \code{\link{write_ascii}}).
#'
#' @details The DWD provides two sets of MOSMIX forecasts labeled as \code{L}
#' and \code{S}. \code{L}-forecasts are available four times a day and XML files
#' for individual stations are available on the opendata server. \code{S}-forecasts
#' become available 24 times a day and only one XML file is avialable on the
#' opendata server containing the forecasts for all stations (few thousand stations).
#'
#' TODO: requires more detailed description.
#'
#' @return Returns a \code{data.frame} with initialization times (IssueTime),
#' name of the XML file plus the full URL, local file name, and a check whether
#' or not a file has been processed already.
#'
#' @examples
#' # Latest L-type DWD MOSMIX forecast file for Innsbruck Airport
#' url <- "https://opendata.dwd.de/weather/local_forecasts/mos/MOSMIX_L/single_stations/11120/kml/MOSMIX_L_LATEST_11120.kmz"
#'
#' # Download and extract file
#' kmz   <- tempfile("mosmix_demo_", fileext = ".kmz")
#' check <- download.file(url, kmz)
#' if ( inherits(check, "try-error") ) stop("Problems downloading the file!")
#' kml   <- unzip(kmz)
#'
#' # Parsing the unzipped kml file (XML format)
#' # And extract required datetime and meta information.
#' doc <- XML::xmlParse(kml)
#' datetime <- get_datetime(doc)
#' meta     <- get_meta_info(doc)
#' stations <- get_station_information(doc)
#'
#' # Extracting forecasts now
#' fcst <- get_forecasts("11120", doc, datetime, meta)
#' write_ascii(fcst, dir = ".")
#' write_ascii(fcst, file = "foo.dat", dir = ".")
#'
#' # Remove kmz and kml file
#' file.remove(kmz, kml)
#' @export
#' @author Reto Stauffer
#' @importFrom utils download.file
get_files_available <- function(type = "L", stn = NULL, outdir = "DWDMOS") {

    type <- match.arg(type, c("L", "S"))

    # Temporary file for index.html which will be downloaded in a second.
    tmpfile <- tempfile()

    # MOSMIX_L (4 times a day, are available as single_station files. Thus,
    # 'stn' has to be provided if type = "L"!
    if ( type == "L" ) {
        # Only for numeric station identifiers
        stopifnot(inherits(stn, c("integer", "numeric")))

        # Read URL, deparse information.
        idxurl <- sprintf("https://opendata.dwd.de/weather/local_forecasts/mos/MOSMIX_L/single_stations/%1$05d/kml/", stn)
        check <- try(download.file(idxurl, tmpfile, method = "curl", quiet = TRUE))
        if ( inherits(check, "try-error") )
            stop("Problems reaching opendata.dwd.de index file to check for available files!")
        files <- readLines(tmpfile)
        files <- regmatches(files, regexpr("MOSMIX_L_[0-9]{10}_[0-9a-zA-Z]+\\.kmz", files))
        times <- regmatches(files, regexpr("[0-9]{10}", files))
        times <- as.POSIXct(strptime(times, "%Y%m%d%H"))

        # Check if local file exists.
        datfiles <- sprintf("%1$s/%2$05d/DWDMOS_%3$s_%2$05d.dat", outdir, stn, strftime(times, "%Y%m%d%H%M%S"))
        datcheck <- file.exists(datfiles)

        res <- data.frame(datetime = times, dstfile = datfiles, processed = datcheck,
                          srcfile = files, url = sprintf("%s/%s", idxurl, files))

        # Subet and return
        return(subset(res, processed == FALSE))

    } else {


        # Read URL, deparse information.
        idxurl = "https://opendata.dwd.de/weather/local_forecasts/mos/MOSMIX_S/all_stations/kml/"
        check <- try(download.file(idxurl, tmpfile, method = "curl", quiet = TRUE))
        if ( inherits(check, "try-error") )
            stop("Problems reaching opendata.dwd.de index file to check for available files!")
        files <- readLines(tmpfile)
        files <- regmatches(files, regexpr("MOSMIX_S_[0-9]{10}_[0-9]+\\.kmz", files))
        times <- regmatches(files, regexpr("[0-9]{10}", files))
        times <- as.POSIXct(strptime(times, "%Y%m%d%H"))

        datfiles <- list.files(outdir, recursive = TRUE)
        datcheck <- sapply(times, function(x, datfiles)
                           sum(grepl(sprintf("_%s_.*\\.dat$", strftime(x, "%Y%m%d%H%M%S")), datfiles)) > 0,
                           datfiles = datfiles)
        res <- data.frame(datetime = times, processed = datcheck,
                          srcfile = files, url = sprintf("%s/%s", idxurl, files))

        # Subet and return
        return(subset(res, processed == FALSE))
    }

}

#' @importFrom utils globalVariables
globalVariables(c("processed"))

