# read_Lx_variable.R

# These two functions share 95% of their code, but they're short


#' Read L1 (Level 1) sensor data files
#'
#' This function reads the COMPASS-FME L1 data files (CSV format)
#' for a single variable, from one or more sites, and returns
#' the compiled data.
#'
#' @param variable Variable name ('research name') to be read, character
#' @param path Path of the L1 dataset, character
#' @param site Optional name of the site(s) of data to read, character
#' @param quiet Print diagnostic information? Logical
#' @importFrom dplyr bind_rows
#' @importFrom readr read_csv
#' @returns A \code{\link[tibble]{tibble}} of L1 data.
#' @export
#' @author BBL
#' @note This function only works for L1 v2-0 (July 2025) and higher.
#' @examples
#' \dontrun{
#' read_L1_variable("gw-tds", site = "TMP")
#' read_L1_variable("gw-tds", c("TMP", "OWC")) # multiple sites
#' read_L1_variable(variable = "gw-tds") # will read all sites' data
#' read_L1_variable(variable = "gw-tds", path = "/path/to/L1/data")
#' }
read_L1_variable <- function(variable, path, site = NULL, quiet = FALSE) {

    if(length(variable) > 1) {
        stop("Only one variable can be read at a time")
    }
    if(is.null(site)) {
        sites <- "[A-Z]*"
    } else {
        sites <- paste0("(", paste(site, collapse = "|"), ")")
    }
    # Construct regular expression to identify files
    regex <- paste0("^", sites, "_[A-Z0-9]+_.*_", variable, "_L1_.*csv$")
    if(!quiet) message(regex)
    files <- list.files(path, pattern = regex, recursive = TRUE)
    if(!quiet) message("Reading ", length(files), " files")

    # The function works fine reading zero files, but this is
    # probably not what the user wants
    if(length(files) == 0) warning("No files found")

    x <- lapply(files, function(f) {
        if(!quiet) message("\t", f)
        read_csv(file.path(path, f), col_types = "ccTccccdcclll")
    })
    bind_rows(x)
}



#' Read L2 (Level 2) sensor data files (Parquet format)
#'
#' This function reads the COMPASS-FME L2 data files (Parquet format)
#' for a single variable, from one or more sites, and returns
#' the compiled data.
#'
#' @param variable Variable name ('research name') to be read, character
#' @param path Path of the L2 dataset, character
#' @param site Optional name of the site(s) of data to read, character
#' @param quiet Print diagnostic information? Logical
#' @importFrom dplyr bind_rows
#' @importFrom arrow read_parquet
#' @returns A \code{\link[tibble]{tibble}} of L2 data.
#' @export
#' @author BBL
#' @note This function only works for L2 v2-0 (July 2025) and higher.
#' @examples
#' \dontrun{
#' read_L2_variable("gw-tds", site = "TMP")
#' read_L2_variable("gw-tds", c("TMP", "OWC")) # multiple sites
#' read_L2_variable(variable = "gw-tds") # will read all sites' data
#' read_L2_variable(variable = "gw-tds", path = "/path/to/L2/data")
#' }
read_L2_variable <- function(variable, path, site = NULL, quiet = FALSE) {

    if(length(variable) > 1) {
        stop("Only one variable can be read at a time")
    }
    if(is.null(site)) {
        sites <- "[A-Z]*"
    } else {
        sites <- paste0("(", paste(site, collapse = "|"), ")")
    }
    # Construct regular expression to identify files
    regex <- paste0("^", sites, "_[A-Z0-9]+_.*_", variable, "_L2_.*parquet$")
    if(!quiet) message(regex)
    files <- list.files(path, pattern = regex, recursive = TRUE)
    if(!quiet) message("Reading ", length(files), " files")

    # The function works fine reading zero files, but this is
    # probably not what the user wants
    if(length(files) == 0) warning("No files found")

    x <- lapply(files, function(f) {
        if(!quiet) message("\t", f)
        read_parquet(file.path(path, f))
    })
    bind_rows(x)
}

#' Search the list of COMPASS-FME sensor variables
#'
#' @param term Search term, case insensitive (character)
#'
#' @returns Variables whose descriptions match the search term.
#' @export
#' @note The search is case-sensitive but definitely not 'smart';
#' use single words only!
#' @examples
#' search_Lx_variables("redox")
#' search_Lx_variables("this will not find any matches")
search_Lx_variables <- function(term) {
    hits <- grep(tolower(term),
                 tolower(compasstools::Lx_variables$description),
                 fixed = TRUE)
    if(length(hits) > 0) {
        compasstools::Lx_variables[hits,]
    } else {
        message("No matches found")
    }
}
