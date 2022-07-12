
#' Read a raw sapflow data file
#'
#' @param filename Fully-qualified filename, character, of a raw sapflow dataset
#' from a Campbell datalogger
#' @author Stephanie Pennington
#' @return A \code{\link[tibble]{tibble}} with the data.
#' @export
#' @importFrom readr read_csv
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
    if (requireNamespace("rdrop2", quietly = TRUE)) {
        # download to temp file
        tf <- tempfile()
        rdrop2::drop_download(filename, local_path = tf,
                              dtoken = token, overwrite = TRUE)
        read_function(tf)
    } else {
        stop("rdrop2 package is not available")
    }
}

