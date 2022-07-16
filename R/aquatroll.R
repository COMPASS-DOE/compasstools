
#' Read a raw Aqua TROLL 600 data file
#'
#' @param filename Fully-qualified filename of a raw Aqua TROLL 600 dataset
#' from a Campbell datalogger
#' @description This function uses
#' \code{\link[readr]{read_csv}} to parse the file into a data frame.
#' @author Stephanie Pennington
#' @return A \code{\link[tibble]{tibble}} with the data.
#' @export
#' @importFrom readr read_csv
#' @seealso \code{\link{process_aquatroll_dir}}
read_aquatroll600_file <- function(filename) {
    # Lines 1, 3, and 4 of the Aqua TROLL 600 data files contain sensor metadata
    # that we want to remove. Read the data files into a string vector, remove
    # those lines, and then pass to read_csv()
    rawdata <- readLines(filename)[-c(1, 3, 4)]
    # Note we have no time zone information, so read the timestamp as character
    read_csv(I(rawdata), na = "NAN", col_types = paste0("cdcc", strrep("d", 19)))
}


#' Read a raw Aqua TROLL 200 data file
#'
#' @param filename Fully-qualified filename of a raw Aqua TROLL 200 dataset
#' from a Campbell datalogger
#' @description This function uses
#' \code{\link[readr]{read_csv}} to parse the file into a data frame.
#' @author Stephanie Pennington
#' @return A \code{\link[tibble]{tibble}} with the data.
#' @export
#' @importFrom readr read_csv
#' @seealso \code{\link{process_aquatroll_dir}}
read_aquatroll200_file <- function(filename) {
    # Lines 1, 3, and 4 of the Aqua TROLL 200 data files contain sensor metadata
    # that we want to remove. Read the data files into a string vector, remove
    # those lines, and then pass to read_csv()
    rawdata <- readLines(filename)[-c(1, 3, 4)]
    # Note we have no time zone information, so read the timestamp as character
    read_csv(I(rawdata), na = "NAN", col_types = paste0("cdccddddddddd"))
}


#' Read and process a directory of Aqua TROLL files
#'
#' @param datadir Directory, either in Dropbox or local
#' @param tz Time zone the data are set to
#' @param dropbox_token Optional Dropbox token
#' @param progress_bar Optional progress bar to call while reading
#' @description Read a directory of Aqua TROLL 200 and/or 600 files,
#' either from Dropbox or locally.
#' @return All Aqua TROLL files in directory, read and concatenated, with some
#' basic processing done: duplicate rows dropped, time zone set, reshaped
#' to 'long' form, and information such as data logger ID, data logger
#' channel, and variable number parsed into their separate columns.
#' @import dplyr
#' @importFrom tidyr pivot_longer separate
#' @importFrom lubridate ymd_hms
#' @seealso \code{\link{read_aquatroll200_file}} \code{\link{read_aquatroll600_file}}
#' @export
#' @author Ben Bond-Lamberty
process_aquatroll_dir <- function(datadir, tz, dropbox_token = NULL, progress_bar = NULL) {

    # Set to NULL so that R CMD CHECK doesn't generate notes
    Instrument<- Pressure<- Pressure600 <- RDO_concen600 <- Salinity <-
        Salinity600 <- Statname <- TIMESTAMP <- Temperature <-
        Temperature600 <- Timestamp <- NULL

    # Read 200 and 600 files, make column names consistent, concatenate
    x200 <- process_dir(datadir,
                        pattern = "200\\.dat$",
                        read_function = read_aquatroll200_file,
                        dropbox_token = dropbox_token,
                        progress_bar = progress_bar)
    x200 %>%
        mutate(Instrument = "TROLL200") %>%
        select(Timestamp = TIMESTAMP, Logger_ID = Statname, Temp = Temperature,
               Pressure_psi = Pressure, Salinity, Instrument) ->
        x200

    x600 <- process_dir(datadir,
                        pattern = "600\\.dat$",
                        read_function = read_aquatroll600_file,
                        dropbox_token = dropbox_token,
                        progress_bar = progress_bar)
    x600 %>%
        mutate(Instrument = "TROLL600") %>%
        select(Timestamp = TIMESTAMP, Temp = Temperature600,
               Salinity = Salinity600, DO_mgl = RDO_concen600,
               Pressure_psi = Pressure600, Instrument, Logger_ID = Statname) ->
        x600

    x <- bind_rows(x200, x600)
    if(!nrow(x)) return(x)

    # Do some basic processing: set time zone
    x %>%
        mutate(Timestamp = ymd_hms(Timestamp, tz = tz))
}
