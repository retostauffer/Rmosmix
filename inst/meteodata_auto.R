# -------------------------------------------------------------------
# - NAME:        automation.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2019-01-09
# -------------------------------------------------------------------
# - DESCRIPTION: Implementation on meteo-data.
# -------------------------------------------------------------------
# - EDITORIAL:   2019-01-09, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2019-01-09 18:53 on marvin
# -------------------------------------------------------------------

    library("mosmix")

    # Hotfix for R 3.1
    dir.exists <- function(x, ...) file.exists(x, ...)

    #    coordinates  name                 desc alt
    #  (8.53, 47.48) 06670              ZUERICH 436
    #  (16.37, 48.2) 11034            WIEN/CITY 171
    # (16.37, 48.25) 11035      WIEN/HOHE WARTE 195
    # (16.57, 48.12) 11036             WIEN FL. 183
    #  (13.3, 52.47) 10381        BERLIN-DAHLEM  58
    # (16.42, 48.12) 11040        WIEN/UNTERLAA 201
    # (13.32, 52.57) 10382         BERLIN-TEGEL  37
    #  (13.5, 52.63)  G002          BERLIN-BUCH  60
    #  (13.4, 52.47) 10384     BERLIN-TEMPELHOF  50
    # (11.35, 47.27) 11120        INNSBRUCK FL. 581
    # (13.57, 52.55)  G005       BERLIN-MARZAHN  60
    # (13.52, 52.38) 10385   BERLIN-SCHOENEFELD  47
    # (12.23, 51.42) 10469   LEIPZIG/SCHKEUDITZ 141
    #  (13.73, 52.4)  G006     BERLIN-KANISWALL  33
    # (13.42, 52.52) 10389         BERLIN-ALEX.  37
    # (12.42, 51.32) 10471              LEIPZIG 141
    #  (8.57, 47.38) 06660 ZUERICH (TOWN/VILLE) 556
    # (16.22, 47.83) 11182      WIENER NEUSTADT 285
    #  (9.17, 47.77) K2787 UEBERLINGEN/BODENSEE 440
    stations <- c(6660, 6670, 11034, 11035, 11036, 10381,
                  11040, 10382, 10384, 11120, 10385, 10469,
                  10389, 10471, 11182)


# -------------------------------------------------------------------
# L-type forecasts: XML files are available for each station
# individually. Thus, we only have to download a few kilobytes of
# data. L-type forecasts become available four times a day.
# -------------------------------------------------------------------

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
            check   <- try(download.file(as.character(files$url[i]), kmz,
                                         method = "curl", quiet = TRUE))
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

    # Processing S-type forecasts
    cat("\n\nProcessing S-type DWD MOSMIX forecasts\n")

    # Find latest MOS forecast on opendata.dwd.de
    # As this takes a while: only take the last two entries!
    files <- get_files_available("S", outdir = "DWDMOS_S")

    # Looping over all not-yet-processed files (if there are any),
    # download the XML file, extract information, done.
    for ( i in 1:nrow(files) ) {

        cat(sprintf("* Downloading \"%s\"\n", as.character(files$srcfile[i])))

        # Downloading the data set
        kmz <- tempfile(pattern = "DWDMOS_S_", fileext = ".kmz")
        check   <- try(download.file(as.character(files$url[i]), kmz,
                                     method = "curl", quiet = TRUE))
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









