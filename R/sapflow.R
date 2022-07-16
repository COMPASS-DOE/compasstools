
#' Read a raw sapflow data file
#'
#' @param filename Fully-qualified filename of a raw sapflow dataset
#' from a Campbell datalogger
#' @description This function reads a local file of raw sapflow data,
#' extracts the logger number from the header, and uses
#' \code{\link[readr]{read_csv}} to parse the file into a data frame.
#' @author Stephanie Pennington
#' @return A \code{\link[tibble]{tibble}} with the data.
#' @export
#' @importFrom readr read_csv read_lines
#' @import fpeek
#' @seealso \code{\link{process_sapflow_dir}}
read_sapflow_file <- function(filename, min_timestamp = NULL) {

    if(!is.null(min_timestamp)) {
        # peek into the file and calculate skip
        firstdate <- capture.output(peek_head(filename, 5))
        firstdate <- strsplit(tail(x, 1), ",")[[1]][1]
        lastdate <- capture.output(fpeek::peek_tail(filename, n = 1))
        lastdate <- strsplit(lastdate, ",")[[1]][1]

        fd <- ymd_hms(firstdate)
        ld <- ymd_hms(lastdate)
        md <- ymd_hms(min_timestamp)
        frac <- (md - fd) / (ld - md)
        skip <- (fpeek::peek_count_lines(filename) - 4) * frac
    } else {
        skip <- 0
    }
    sdat <- read_lines(filename, skip = skip)
    sdat <- sdat[-3:-4] # remove lines 3 and 4 with unneeded information

    # parse line one to extract logger name
    pnnl_x <- gregexpr("PNNL_", sdat[1])[[1]][1]
    logger_name <- substr(sdat[1], start = pnnl_x, stop = pnnl_x + 6)

    # The "I()" notation is how to read from a string; see help page
    x <- read_csv(I(sdat), skip = 1, col_types = "Tddddddddddddddddd")
    x$Logger <- logger_name
    x
}


#' Read and process a directory of sapflow files
#'
#' @param datadir Directory, either in Dropbox or local
#' @param tz Time zone the data are set to
#' @param dropbox_token Optional Dropbox token
#' @param progress_bar Optional progress bar to call while reading
#' @description Read a directory of sapflow files, either from Dropbox or
#' locally.
#' @return All sapflow files in directory, read and concatenated, with some
#' basic processing done: duplicate rows dropped, time zone set, and reshaped
#' to 'long' form.
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom lubridate ymd_hms
#' @importFrom readr parse_number
#' @seealso \code{\link{read_sapflow_file}}
#' @export
#' @author Ben Bond-Lamberty
process_sapflow_dir <- function(datadir, tz, dropbox_token = NULL, progress_bar = NULL) {

    x <- process_dir(datadir,
                     pattern = "sapflow\\.dat$",
                     read_function = read_sapflow_file,
                     dropbox_token = dropbox_token,
                     progress_bar = progress_bar)

    if(!nrow(x)) return(x)

    # Set to NULL so that R CMD CHECK doesn't generate notes
    TIMESTAMP <- RECORD <- Timestamp <- Port <- Logger <- NULL

    # Do some basic processing:
    # concatenate, set time zone, reshape, clean up some fields
    x %>%
        bind_rows() %>%
        distinct() %>%
        pivot_longer(cols = starts_with("DiffVolt_Avg"),
                     names_to = "Port", values_to = "Value") %>%
        rename(Timestamp = TIMESTAMP,
               Record = RECORD) %>%
        mutate(Timestamp = ymd_hms(Timestamp, tz = tz),
               # extract number from former col name;
               # for example, "DiffVolt_Avg(1)" becomes "1"
               Port = parse_number(Port),
               Logger = parse_number(Logger))
}
