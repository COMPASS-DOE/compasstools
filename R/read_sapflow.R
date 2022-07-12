# Function to read logger network sapflow data
# TEMPEST
# Stephanie Pennington | Created July 2021

# `filename` is a character path to a raw sapflow dataset from a Campbell data logger
# returns a dataframe



#' Read a raw sapflow data file
#'
#' @param filename Fully-qualified filename, character
#'
#' @return A tibble with the data
#' @export
#' @importFrom readr read_csv
#' @examples
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
