
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

#' Download and read a data file from Dropbox
#'
#' @param filename A Dropbox filename, e.g. returned by \code{drop_dir}
#' @param token A dropbox token
#' @param read_function A function to read the downloaded file with
#' @return A \code{\link[tibble]{tibble}} with the data.
#' @export
#' @author Ben Bond-Lamberty
read_file_dropbox <- function(filename, token, read_function) {
    # We don't want users to need rdrop2 to use this package (i.e. we don't
    # want to put it in DESCRIPTION's Imports:), so check for availability
    if(requireNamespace("rdrop2", quietly = TRUE)) {
        # download to temp file
        tf <- tempfile()
        rdrop2::drop_download(filename, local_path = tf,
                              dtoken = token, overwrite = TRUE)
        read_function(tf)
    } else {
        stop("rdrop2 package is not available")
    }
}


#' Read a directory of sapflow files, either from Dropbox or locally
#'
#' @param datadir Directory, either in Dropbox or local
#' @param tz Time zone the data are set to
#' @param dropbox_token Optional Dropbox token
#' @param progress_bar Optional progress bar to call while reading
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

    local <- is.null(dropbox_token)
    if(local) {
        s_files <- list.files(datadir, pattern = "sapflow\\.dat$", full.names = TRUE)
    } else {
        if(requireNamespace("rdrop2", quietly = TRUE)) {
            # Generate list of 'current' sapflow files
            s_dir <- rdrop2::drop_dir(datadir, dtoken = dropbox_token)
            s_files <- grep(s_dir$path_display, pattern = "sapflow\\.dat$", value = TRUE)
        } else {
            stop("rdrop2 package is not available")
        }
    }

    f <- function(filename, token, total_files) {
        if(!is.null(progress_bar)) progress_bar(1 / total_files)
        # Read file, either locally or from Dropbox
        if(local) {
            read_sapflow_file(filename)
        } else {
            read_file_dropbox(filename, dropbox_token, read_sapflow_file)
        }
    }
    x <- lapply(s_files, f, dropbox_token, length(s_files))
    x <- bind_rows(x)

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
