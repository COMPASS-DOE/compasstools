
#' Read a raw TEROS data file
#'
#' @param filename Fully-qualified filename of a raw TEROS dataset
#' from a Campbell datalogger
#' @param min_timestamp Minimum timestamp to read, character;
#' function will skip down in the data until approximately this time
#' @param quiet Print diagnostic messages? Logical
#' @description This function uses
#' \code{\link[readr]{read_csv}} to parse the file into a data frame.
#' @author Stephanie Pennington
#' @return A \code{\link[tibble]{tibble}} with the data.
#' @export
#' @importFrom readr read_csv
#' @seealso \code{\link{process_teros_dir}}
#' @examples
#' fn <- system.file("PNNL_11_Terosdata.dat", package = "compasstools")
#' read_teros_file(fn)
read_teros_file <- function(filename, min_timestamp = NULL, quiet = FALSE) {

    skip <- calculate_skip(filename, header_rows = 4, min_timestamp, quiet = quiet)
    if(skip == -1) return(tibble()) # entire file can be skipped

    # Generate "Teros(1, 1)", "Teros(1, 2)", "Teros(1, 3)", "Teros(2, 1)", etc.
    teros_names <- sapply(1:22, function(x) paste0("Teros(", x, ", ", 1:3, ")"))
    # Note we have no time zone information, so read the timestamp as character
    read_csv(filename, na = "NAN",
             skip = skip + 4, # add 4 for header
             col_names = c("Timestamp", "Record", "Statname",
                           as.vector(teros_names)),
             col_types = paste0("cdc", strrep("d", 66)))
}


#' Read and process a directory of TEROS files
#'
#' @param datadir Directory, either in Dropbox or local
#' @param tz Time zone the data are set to
#' @param dropbox_token Optional Dropbox token
#' @param progress_bar Optional progress bar to call while reading
#' @param ... Other parameters to be passed to \code{\link{read_teros_file}}
#' @description Read a directory of TEROS files, either from Dropbox or
#' locally.
#' @return All TEROS files in directory, read and concatenated, with some
#' basic processing done: duplicate rows dropped, time zone set, reshaped
#' to 'long' form, and information such as data logger ID, data logger
#' channel, and variable number parsed into their separate columns.
#' @import dplyr
#' @importFrom tidyr pivot_longer separate
#' @importFrom lubridate ymd_hms
#' @seealso \code{\link{read_teros_file}}
#' @export
#' @author Ben Bond-Lamberty
process_teros_dir <- function(datadir, tz, dropbox_token = NULL,
                              progress_bar = NULL, ...) {

    x <- process_dir(datadir,
                     pattern = "Terosdata\\.dat$",
                     read_function = read_teros_file,
                     dropbox_token = dropbox_token,
                     progress_bar = progress_bar,
                     ...)

    if(!nrow(x)) return(x)

    # Set to NULL so that R CMD CHECK doesn't generate notes
    Data_Table_ID <- Inst <- Logger <- Statname <- Timestamp <-
        channel <- value <- variable <- NULL

    # Do some basic processing: reshape, set time zone, parse fields
    x %>%
        pivot_longer(starts_with("Teros"), names_to = "channel") %>%
        filter(!is.na(value)) %>%
        # Pull data logger ID out of Statname
        separate(Statname, into = c("Inst", "Logger"), sep = "_" ) %>%
        # Parse the data logger number, channel number, and variable number out of the
        # Statname and Channel columns
        mutate(Logger = as.integer(Logger, fixed = TRUE),
               Timestamp = ymd_hms(Timestamp, tz = tz)) %>%
        select(-Inst) %>%  # unneeded
        # Next, parse channel into the data logger channel and variable number
        separate(channel, into = c("Data_Table_ID", "variable"), sep = ",") %>%
        mutate(Data_Table_ID = as.integer(gsub("Teros(", "", Data_Table_ID, fixed = TRUE)),
               variable = as.integer(gsub(")", "", variable, fixed = TRUE)),
               # Give them sensible names
               variable = case_when(variable == 1 ~ "VWC",
                                    variable == 2 ~ "TSOIL",
                                    variable == 3 ~ "EC"))
}
