
#' Read a raw sapflow data file
#'
#' @param filename Fully-qualified filename, character, of a raw sapflow dataset
#' from a Campbell datalogger
#' @author Stephanie Pennington
#' @return A \code{\link[tibble]{tibble}} with the data.
#' @export
#' @importFrom readr read_csv
#' @seealso \code{\link{process_sapflow_dir}}
read_sapflow_file <- function(filename) {

    sdat <- readLines(filename)
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
