

#' Default Plot for DWD MOSMIX Forecasts
#'
#' A default plot routine to check the data.
#'
#' @param x object of class \code{dwdforecasts} as returned by
#'     \code{\link{get_forecasts}}.
#'
#' @export
#' @import graphics
# TODO: Write manual page.
plot.dwdforecast <- function(x, ...) {

    # Drop columns containing missing data only
    idx <- which(apply(x, 2, function(x) sum(!is.na(x)) == 0))
    if ( length(idx) > 0 ) x <- x[,-idx]

    # Make time series strictly regular
    if ( ! is.regular(x, strict = TRUE) )
        x <- merge(x, zoo(,seq(min(index(x)), max(index(x)), deltat(x))))

    # Check availability of the different parameters/columns required
    # for the different panels of the default plotting methods.
    # Panels having no data will simply be ignored.
    check_cols <- function(x, params)
        list(N = sum(params %in% names(x)), params = params)
    check <- list(
            temperature = check_cols(x, c("TTT", "Td", "TN", "TX")),
            sunshine    = check_cols(x, c("SunD1", "N", "Neff")),
            prain       = check_cols(x, c("PPPP", "RR1c")),
            wind        = check_cols(x, c("DD", "FF", "FX1"))
        )

    # Take those where at least one parameter is available
    take <- sapply(check, function(x) x$N > 0)
    if ( sum(take) == 0 ) stop("Cannot create a plot, none of the expected parameters available!")

    # Subplot height
    heights <- list(temperature = 2, wind = 2, prain = 1, sunshine = 1)
    heights <- heights[names(take)[take]]

    par(mar = rep(0.1, 4), oma = c(4.1, 7.1, 2.5, 7.2))
    layout(matrix(1L:sum(take), ncol = 1), heights = heights)

    # Function for the baseplot
    baseplot <- function(xlim, ylim, h = NULL, bg = TRUE, ...) {
        plot(NA, xlim = xlim, ylim = ylim, xaxt = "n", xaxs = "i", bty = "n", ...)
        # xblocks
        if ( bg ) {
            b <- seq(as.POSIXct(as.Date(min(xlim))), as.POSIXct(as.Date(max(xlim))+1), by = 86400)
            for ( i in seq(1, length(b)-1, 2) ) {
                zoo::xblocks(c(b[i], b[i+1]), colors$bg)
            }
            # Vertical lines
            v <- seq(as.POSIXct(as.Date(min(xlim))), as.POSIXct(as.Date(max(xlim))+1), by = 12 * 3600)
            abline(v = v, lty = 2, #ifelse(as.POSIXlt(v)$hour == 0, 2, 4),
                          lwd = ifelse(as.POSIXlt(v)$hour == 0, .5, 0.2))
            # Horizontal lines
            if ( is.null(h) ) h <- pretty(ylim)
            abline(h = h, col = colors$gray, lty = 2)
        }
    }

    # X-Limits
    xlim <- range(index(x))

    # A set of colors I'll use
    colors <- list(red = "#FF0000", green = "green", gray = "gray60",
                   blue = "#0000FF",
                   bg = "gray95", orange = "orange")

    # -----------------------------
    # Temperature plot
    if ( check$temperature$N > 0 ) {
        ylim <- range(x[,check$temperature$params] - 273.15, na.rm = TRUE)
        baseplot(xlim, ylim, h = seq(-50, 50, by = 2.5))
        if ( all(c("TTT", "Td") %in% names(x)) )
            add_polygon(x$TTT - 273.15, lower.limit = x$Td - 273.15, col = "#003300")
        if ( "E_Td" %in% names(x) )
            add_polygon(              x$Td - 273.15 + x$E_Td/2,
                        lower.limit = x$Td - 273.15 - x$E_Td/2,
                        col = "#00FF00")
        if ( "Td" %in% names(x) )
            lines(x$Td  - 273.15, col = colors$green, lwd = 2)
        if ( "E_TTT" %in% names(x) )
            add_polygon(              x$TTT - 273.15 + x$E_TTT/2,
                        lower.limit = x$TTT - 273.15 - x$E_TTT/2,
                        col = "#FF0000")
        if ( "TTT" %in% names(x) )
            lines(x$TTT - 273.15, col = colors$red,, lwd = 2)
        if ( "TX" %in% names(x) )
            add_steps(x$TX - 273.15, width = 43200, col = colors$red)
        if ( "TN" %in% names(x) )
            add_steps(x$TN - 273.15, width = 43200, col = colors$blue)
        # Text and outer box
        # Text and outer box
        mtext(side = 2, line = 4, "temperatures\n[degrees C]")
        box()
    }

    # -----------------------------
    # Sunshine duration
    if ( check$sunshine$N > 0 ) {
        baseplot(xlim, c(0,60), h = seq(10, 50, by = 10), yaxs = "i")
        if ( "N" %in% names(x) )
            add_polygon(x$N * .6, col = "#303030")
        if ( "Neff" %in% names(x) )
            add_polygon(x$Neff * .6, col = "#303030")
        if ( "SunD1" %in% names(x) )
            add_bars(x$SunD1 / 60, col = colors$orange)
        if ( "SunD" %in% names(x) ) {
            tmp <- na.omit(x$SunD)
            text(as.numeric(index(tmp)) - 43200, 60, col = 1,
                 sprintf("%.1fh/d", tmp / 1440), pos = 1)
        }
        if ( any(c("N", "Neff") %in% names(x)) ) {
            mtext(side = 4, line = 4, "total/effective cloud\ncover [percent]")
            axis(side = 4, at = seq(20, 80, by = 20) * .6,
                 labels = seq(20, 80, by = 20))
        }
        # Text and outer box
        mtext(side = 2, line = 4, "sunshine duration\n[minutes/hour]")
        box()
    }

    # -----------------------------
    # Sunshine duration
    if ( check$prain$N > 0 ) {
        if ( "RR1c" %in% names(x) ) {
            baseplot(xlim, ylim = c(0, max(max(x$RR1c, na.rm = TRUE) * 1.05, 2.5)), yaxs = "i")
            add_bars(x$RR1c, col = colors$blue)
            mtext(side = 4, line = 4, "surface pressure\n[hetcopascal]")
        }
        if ( "PPPP" %in% names(x) ) {
            if ( "RR1c" %in% names(x) ) par(new = TRUE)
            baseplot(xlim, ylim = range(x$PPPP, na.rm = TRUE) / 100, yaxt = "n",
                     bg = FALSE)
            axis(side = 4)
            lines(x$PPPP / 100, lwd = 2)
            mtext(side = 2, line = 4, "precipitation sum\n[km m^-2 / h]")
        }
        # Adding the box
        box()
    }


    # -----------------------------
    # Sunshine duration
    if ( check$wind$N > 0 ) {
        if ( any(c("FF", "FX1") %in% names(x)) ) {
            ylim <- c(0, max(max(range(x[,grep("^(FF|FX1)$", names(x))], na.rm = TRUE) * 1.05), 5))
            baseplot(xlim, ylim, yaxs = "i")
            if ( "FX1" %in% names(x) )
                lines(x$FX1, col = colors$red, lwd = 2)
            if ( "FF" %in% names(x) )
                add_polygon(x$FF, col = colors$blue, lwd = 2)
            box()
            # Adding legend
            if ( all(c("FF", "FX1") %in% names(x)) ) {
                ylab <- "wind speed and maximum\nhourly wind gust [m/s]"
            } else if ( "FF" %in% names(x) ) {
                ylab <- "wind speed\n[m/s]"
            } else {
                ylab <- "maximum hourly\nwind gust [m/s]"
            }
            mtext(side = 2, line = 4, ylab)
        }

        if ( "DD" %in% names(x) ) {
            if ( any(c("FF", "FX1") %in% names(x)) ) par(new = TRUE)
            baseplot(xlim, ylim = c(0, 360), yaxt = "n",
                     bg = ! any(c("FF", "FX1") %in% names(x)))
            points(x$DD, pch = 19, col = rgb(0,0,0,0.8))
            axis(side = 4, at = seq(90, 270, by = 90))

            if ( "E_DD" %in% names(x) ) {
                at <- as.numeric(index(x))
                y0 <- as.numeric(x$DD) - 0.5 * as.numeric(x$E_DD)
                y1 <- as.numeric(x$DD) + 0.5 * as.numeric(x$E_DD)
                segments(at, y0, at, y1,           , col = colors$gray)
                segments(at, y0 + 360, at, y1 + 360, col = colors$gray)
                segments(at, y0 - 360, at, y1 - 360, col = colors$gray)
            }

            mtext(side = 4, line = 4, "wind direction\n[degrees]")
        }

        box()
    }

    # Adding axis
    idx <- as.POSIXct(seq(as.Date(min(xlim)), as.Date(max(xlim)) + 1, by = 1))
    axis(side = 1, at = idx, labels = NA)
    axis(side = 1, at = idx + 43200, labels = strftime(idx, "%b %d"), lwd = 0)

    # Adding title
    if ( "main" %in% names(list(...)) ) { main <- list(...)$main } else {
        main <- sprintf("DWD MOSMIX Forecast, %s (%s)", attr(x, "description"), attr(x, "name"))
    }
    #print(main)
    mtext(side = 3, line = 0.5, font = 2, cex = 1.2, main, outer = TRUE)


}

add_steps <- function(x, width, ...) { 
    # Calculate coordinates for the rectangles
    tmp <- na.omit(x)
    segments(as.numeric(index(x)) - width, as.numeric(x),
             as.numeric(index(x)),         as.numeric(x), ...)
}

add_bars <- function(x, col = "yellow", lower.limit = 0) { 
    # Calculate coordinates for the rectangles
    xup <- as.numeric(index(x))
    xlo <- xup - deltat(x)
    ylo <- rep(lower.limit, length(x))
    yup <- as.numeric(x)
    rect(xlo, ylo, xup, yup, col = col, border = NA)
}



#' Add Polygon to Plot
#'
#' Helper function to plot a filled polygon based on a \code{zoo}
#' time series object with nice missing data handling.
#'
#' @param x an univariate \code{zoo} time series object.
#' @param col character, a hex color. Default is \code{"#ff0000"} (red).
#' @param lower.limit numeric, the lower limit used to plot the polygon.
#'        default is \code{0}.
#' @param lwd line width argument.
#'
#' @examples
#' library("zoo")
#' # Create a time series object
#' set.seed(3)
#' a <- zoo(sin(1:100/80*pi) + 3 + rnorm(100, 0, 0.3), 201:300)
#' 
#' # Plot
#' par(mfrow = c(1,3))
#' plot(a, type = "n", main = "Demo Plot 1",
#'      ylim = c(-1, max(a)+1), xaxs = "i", yaxs = "i")
#' add_polygon(a)
#' 
#' # Specify lower.limit to -1 (lower limit of our ylim),
#' # add different color, change line style.
#' plot(a, type = "n", main = "Demo Plot 2",
#'      ylim = c(-1, max(a)+.2), xaxs = "i", yaxs = "i")
#' add_polygon(a, col = "#00CCBB", lower.limit = -1, lwd = 3)
#' 
#' # Using an "upper limit".
#' plot(a, type = "n", main = "Demo Plot 3",
#'      ylim = c(-1, max(a)+.2), xaxs = "i", yaxs = "i")
#' add_polygon(a, col = "#00BBFF", lower.limit = par()$usr[4L])
#' 
#' # Make a copy and add some missing values
#' b <- a
#' b[2:10]  <- NA
#' b[50:55] <- NA
#' b[70]    <- NA
#' 
#' # Plot
#' par(mfrow = c(1,1))
#' 
#' # Same as "Demo Plot 2" with the time series which
#' # contains missing values (b).
#' plot(b, type = "n", main = "Demo Plot 2 With Missing Values",
#'      ylim = c(-1, max(b, na.rm = TRUE)+.2), xaxs = "i", yaxs = "i")
#' add_polygon(b, col = "#00CCBB", lower.limit = -1, lwd = 3)
#' 
#' @import zoo
#' @import graphics
#' @author Reto Stauffer
#' @export
add_polygon <- function(x, col = "#ff0000", lower.limit = 0, lwd = 1) {
    # Need hex color
    if ( ! grepl("^#[A-Za-z0-9]{6}$",col) ) stop("Sorry, need hex color definition for polygon plots.")
    # All elements NA?
    if ( all( is.na(x) ) ) return(invisible(NULL))
    # Else find valid blocks and plot them. Start with 1
    i <- 1
    if ( ! length(lower.limit) == length(x) ) lower.limit <- rep(lower.limit, length(x))

    while ( i <= length(x) ) {

        if ( all(is.na(x)) ) break
        i1 <- min( which( !is.na(x) ) )
        if ( i1 > 1 ) { x <- x[-c(seq(1,i1-1))]; i1 <- 1 }
        # Else check first NA
        if ( ! any(is.na(x)) ) { i2 <- length(x) } else { i2 <- min( which( is.na(x) ) ) - 1 }
        # Create x/y coordinate vectors for the polygon function
#        p_x <- as.numeric(zoo::index(x[i1:i2])); p_x <- c(p_x,max(p_x),min(p_x))
#        p_y <- c(as.numeric(x[i1:i2]),lower.limit, lower.limit )
        p_x <- as.numeric(zoo::index(x[i1:i2])); p_x <- c(p_x,rev(p_x))
        p_y <- c(as.numeric(x[i1:i2]), rev(lower.limit[i1:i2]))
        # Plotting
        graphics::polygon( p_x, p_y, col = sprintf("%s20",col), border = NA )
        graphics::lines( x[i1:i2],   col = col, lwd = lwd )
        # Remove plotted data from time series and continue
        x <- x[-c(i1:i2)]

    }

}















