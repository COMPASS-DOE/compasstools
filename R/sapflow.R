
#' Read a raw sapflow data file
#'
#' @param filename Fully-qualified filename of a raw sapflow dataset
#' from a Campbell datalogger
#' @param min_timestamp Minimum timestamp to read, character;
#' function will skip down in the data until approximately this time
#' @description This function reads a local file of raw sapflow data,
#' extracts the logger number from the header, and uses
#' \code{\link[readr]{read_csv}} to parse the file into a data frame.
#' @author Stephanie Pennington
#' @return A \code{\link[tibble]{tibble}} with the data.
#' @export
#' @importFrom readr read_csv read_lines
#' @seealso \code{\link{process_sapflow_dir}}
#' @examples
#' fn <- system.file("PNNL_11_sapflow_1min.dat", package = "compasstools")
#' read_sapflow_file(fn)
read_sapflow_file <- function(filename, min_timestamp = NULL, quiet = FALSE) {

    skip <- calculate_skip(filename, header_rows = 4, min_timestamp, quiet = quiet)
    if(skip == -1) return(tibble()) # entire file can be skipped

    # Parse line one to extract logger name
    dat_header <- read_lines(filename, n_max = 1)
    pnnl_x <- gregexpr("PNNL_", dat_header[1])[[1]][1]
    logger_name <- substr(dat_header[1], start = pnnl_x, stop = pnnl_x + 6)

    # After testing (see https://github.com/COMPASS-DOE/compasstools/issues/12)
    # hard-coding column names and types is by far the fastest approach
    # We have no time zone information, so read the timestamp as character
    x <- read_csv(filename,
                  skip = skip + 4, # add 4 for header
                  col_names = c("Timestamp", "Record", "Statname", "BattV_Avg",
                                paste0("DiffVolt_Avg(", 1:14, ")"),
                                paste0("DiffVolt(", 1:14, ")")),
                  col_types = paste0("c", strrep("d", 31)))
    x$Logger <- logger_name
    x
}


#' Read and process a directory of sapflow files
#'
#' @param datadir Directory, either in Dropbox or local
#' @param tz Time zone the data are set to
#' @param dropbox_token Optional Dropbox token
#' @param progress_bar Optional progress bar to call while reading
#' @param ... Other parameters to be passed to \code{\link{read_sapflow_file}}
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
process_sapflow_dir <- function(datadir, tz,
                                dropbox_token = NULL,
                                progress_bar = NULL,
                                ...) {

    x <- process_dir(datadir,
                     pattern = "sapflow\\.dat$",
                     read_function = read_sapflow_file,
                     dropbox_token = dropbox_token,
                     progress_bar = progress_bar,
                     # other parameters to be passed to read_sapflow_file
                     ...)

    if(!nrow(x)) return(x)

    # Set to NULL so that R CMD CHECK doesn't generate notes
    Timestamp <- Port <- Logger <- NULL

    # Do some basic processing:
    # concatenate, set time zone, reshape, clean up some fields
    x %>%
        distinct() %>%
        pivot_longer(cols = starts_with("DiffVolt_Avg"),
                     names_to = "Port", values_to = "Value") %>%
        mutate(Timestamp = ymd_hms(Timestamp, tz = tz),
               # extract number from former col name;
               # for example, "DiffVolt_Avg(1)" becomes "1"
               Port = parse_number(Port),
               Logger = parse_number(Logger))
}
