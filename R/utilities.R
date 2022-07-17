# Utilities


#' Calculate lines of data to skip
#'
#' @param filename Filename, character
#' @param header_rows Number of header rows, integer
#' @param min_timestamp Minimum timestamp caller is looking for, character
#' @param parse_function Parse function to handle timestamps
#' @param quiet Print diagnostic message? Logical
#' @description This internal function returns the number of lines of a
#' file to skip based on a requested minimum timestamp to read. It does so
#' by 'peeking' at the first and last timestamps of the data, ignoring any
#' header lines. We assume that the timestamps of the
#' data are in ascending order and regular in their spacing.
#' @return The number of lines OF THE FILE (not of the data) to skip.
#' @note For high performance this uses the \code{fpeek} package.
#' @import fpeek
#' @importFrom utils capture.output tail
calculate_skip <- function(filename, header_rows, min_timestamp,
                           parse_function = ymd_hms, quiet = FALSE) {
    if(is.null(min_timestamp)) return(header_rows) # no data skip

    firstline <- tail(capture.output(peek_head(filename, header_rows + 1)), 1)
    lastline <-  capture.output(peek_tail(filename, n = 1))
    nlines <-  fpeek::peek_count_lines(filename) - header_rows

    # Assume the timestamp is the first field and comma-separated
    # So far this is true for all data types; if not in the future,
    # may need to add split_char and field_num parameters
    first_ts <- strsplit(firstline, ",")[[1]][1]
    last_ts <- strsplit(lastline, ",")[[1]][1]
    fts <- parse_function(first_ts)
    lts <- parse_function(last_ts)
    mts <- parse_function(min_timestamp)
    if(is.na(mts)) {
        stop("Could not parse min_timestamp into POSIXct")
    }

    frac <- as.numeric(difftime(mts, fts, units = "secs")) /
        as.numeric(difftime(lts, fts, units = "secs"))
    skip <- as.integer(nlines * frac)
    if(!quiet) {
        message("Skipping ", round(frac * 100, 0), "% of ",
                basename(filename), " (min_timestamp = ", mts, ")")
    }
    if(skip >= nlines) {
        return(-1)
    } else if(skip < 0) {
        return(0 + header_rows)
    } else {
        return(skip + header_rows)
    }
}
