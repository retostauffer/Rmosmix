# -------------------------------------------------------------------
# - NAME:        automation.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2019-01-09
# -------------------------------------------------------------------
# - DESCRIPTION: Some tests. This script can be used as a template
#                for an automation script. 
# -------------------------------------------------------------------
# - EDITORIAL:   2019-01-09, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2019-01-09 18:35 on marvin
# -------------------------------------------------------------------

    library("mosmix")

# -------------------------------------------------------------------
# L-type forecasts: XML files are available for each station
# individually. Thus, we only have to download a few kilobytes of
# data. L-type forecasts become available four times a day.
# -------------------------------------------------------------------

    # List of required stations (user defines station list)
    #  6660: Zuerich
    # 11120: Innsbruck Airport
    # 11035: Vienna, Hohe Warte
    stations <- c(6660, 11120, 11035)

    # Manually create the output folder if not yet existing.
    dir.create("DWDMOS_L", showWarnings = FALSE)

    # Processing L-type forecasts (station-by-station)
    cat("\n\nProcessing L-type DWD MOSMIX forecasts\n")

    # Looping over the user-specified set of stations
    for ( stn in stations ) {

        # Check for available files on the DWD server
        files <- get_files_available("L", stn, outdir = "DWDMOS_L")

        # No files we have not yet processed? Continue.
        if ( nrow(files) == 0 ) next
    
        # Else loop over all available files and process the data.
        for ( i in 1:nrow(files) ) {

            cat(sprintf("* Downloading \"%s\"\n", as.character(files$srcfile[i])))

            # Downloading the data set
            kmz <- tempfile(pattern = "DWDMOS_L_", fileext = ".kmz")
            check   <- try(download.file(as.character(files$url[i]), kmz, quiet = TRUE))
            if ( inherits(check, "try-error") )
                warning(sprintf("Problems downloading data for %d, skip.", stn))

            # Unzip and parse the XML file
            kml      <- unzip(kmz)
            doc      <- xmlParse(kml)

            # Extracting meta information and datetime information
            meta     <- get_meta_info(doc)
            datetime <- get_datetime(doc)

            # Extract forecasts for this station and write to ASCII file.
            fcst <- get_forecasts(sprintf("%05d", stn), doc, datetime, meta)
            write_ascii(fcst, dir = "DWDMOS_L")

            # Remove the downloaded kmz and extracted kml file.
            file.remove(kmz, kml)
        }
    }


# -------------------------------------------------------------------
# S-type forecasts become available 24 times a day. Only one "global"
# XML file is available. Thus, we do have to download the full data
# set and extract the required stations from it.
# -------------------------------------------------------------------

    # List of required stations (user defines station list)
    #  6660: Zuerich
    # 11120: Innsbruck Airport
    # 11035: Vienna, Hohe Warte
    stations <- c(6660, 11120, 11035)

    # Manually create the output folder if not yet existing.
    dir.create("DWDMOS_S", showWarnings = FALSE)

    # Processing S-type forecasts
    cat("\n\nProcessing S-type DWD MOSMIX forecasts\n")

    # Find latest MOS forecast on opendata.dwd.de
    # As this takes a while: only take the last two entries!
    files <- get_files_available("S", outdir = "DWDMOS_S")
    files <- tail(files, 2)

    # Looping over all not-yet-processed files (if there are any),
    # download the XML file, extract information, done.
    for ( i in 1:nrow(files) ) {

        cat(sprintf("* Downloading \"%s\"\n", as.character(files$srcfile[i])))

        # Downloading the data set
        kmz <- tempfile(pattern = "DWDMOS_S_", fileext = ".kmz")
        check   <- try(download.file(as.character(files$url[i]), kmz, quiet = TRUE))
        if ( inherits(check, "try-error") )
            warning(sprintf("Problems downloading data for %d, skip.", stn))

        # Unzip and parse the XML file
        kml      <- unzip(kmz)
        doc      <- xmlParse(kml)

        # Extracting meta information and datetime information
        meta     <- get_meta_info(doc)
        datetime <- get_datetime(doc)

        # Looping over the stations, extract forecasts and save into
        # the output directory as specified by "dir".
        for ( stn in stations ) {
            fcst <- get_forecasts(sprintf("%05d", stn), doc, datetime, meta)
            write_ascii(fcst, dir = "DWDMOS_S")
        }

        # Remove the kmz and kml file ...
        file.remove(kmz, kml)
    }









