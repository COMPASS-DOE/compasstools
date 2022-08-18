
#' Read a raw Aqua TROLL 600 data file
#'
#' @param filename Fully-qualified filename of a raw Aqua TROLL 600 dataset
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
#' @seealso \code{\link{process_aquatroll_dir}}
#' @examples
#' fn <- system.file("PNNL_23_WaterLevel600.dat", package = "compasstools")
#' read_aquatroll600_file(fn)
read_aquatroll600_file <- function(filename, min_timestamp = NULL, quiet = FALSE) {

    skip <- calculate_skip(filename, header_rows = 4, min_timestamp, quiet = quiet)
    if(skip == -1) return(tibble()) # entire file can be skipped

    # Note we have no time zone information, so read the timestamp as character
    read_csv(filename,
             na = "NAN",
             skip = skip + 4, # add 4 for header
             col_names = c("Timestamp", "Record", "Statname", "Aquatroll_ID(1)",
                           "Barometric_Pressure600", "Temperature600",
                           "Actual_Conductivity600", "Specific_Conductivity600",
                           "Salinity600", "TDS600", "Water_Density600",
                           "Resistivity600", "pH600", "pH_mV600", "pH_ORP600",
                           "RDO_concen600", "RDO_perc_sat600",
                           "RDO_part_Pressure600", "Pressure600", "Depth600",
                           "Temperature_Int600", "Voltage_Ext600", "Battery_Int600"),
             col_types = paste0("cdcc", strrep("d", 19)))
}


#' Read a raw Aqua TROLL 200 data file
#'
#' @param filename Fully-qualified filename of a raw Aqua TROLL 200 dataset
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
#' @seealso \code{\link{process_aquatroll_dir}}
#' @examples
#' fn <- system.file("PNNL_21_WaterLevel200.dat", package = "compasstools")
#' read_aquatroll200_file(fn)
read_aquatroll200_file <- function(filename, min_timestamp = NULL, quiet = FALSE) {

    skip <- calculate_skip(filename, header_rows = 4, min_timestamp, quiet = quiet)
    if(skip == -1) return(tibble()) # entire file can be skipped

    # Note we have no time zone information, so read the timestamp as character
    read_csv(filename,
             na = "NAN",
             skip = skip + 4, # add 4 for header
             col_names = c("Timestamp", "Record", "Statname", "Aquatroll_ID(2)",
                           "Depth", "Temperature", "Actual_Conductivity",
                           "Specific_Conductivity", "Salinity", "TDS",
                           "Water_Density", "Pressure", "Resistivity"),
             col_types = "cdccddddddddd")
}


#' Read and process a directory of Aqua TROLL files
#'
#' @param datadir Directory, either in Dropbox or local
#' @param tz Time zone the data are set to
#' @param dropbox_token Optional Dropbox token
#' @param progress_bar Optional progress bar to call while reading
#' @param ... Other parameters to be passed to \code{\link{read_aquatroll200_file}}
#' or \code{\link{read_aquatroll600_file}}
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
process_aquatroll_dir <- function(datadir, tz, dropbox_token = NULL,
                                  progress_bar = NULL, ...) {

    # Set to NULL so that R CMD CHECK doesn't generate notes
    Instrument <- Pressure<- Pressure600 <- RDO_concen600 <- Salinity <-
        Salinity600 <- Statname <- Temperature <-
        Temperature600 <- Timestamp <- NULL

    # Read 200 and 600 files, make column names consistent, concatenate
    x200 <- process_dir(datadir,
                        pattern = "200\\.dat$",
                        read_function = read_aquatroll200_file,
                        dropbox_token = dropbox_token,
                        progress_bar = progress_bar,
                        # other parameters to be passed to read_aquatroll200_file
                        ...)
    if(nrow(x200)) {
        x200 %>%
            mutate(Instrument = "TROLL200") %>%
            select(Timestamp, Logger_ID = Statname, Temp = Temperature,
                   Pressure_psi = Pressure, Salinity, Instrument) ->
            x200
    }

    x600 <- process_dir(datadir,
                        pattern = "600\\.dat$",
                        read_function = read_aquatroll600_file,
                        dropbox_token = dropbox_token,
                        progress_bar = progress_bar,
                        # other parameters to be passed to read_aquatroll600_file
                        ...)
    if(nrow(x600)) {
        x600 %>%
            mutate(Instrument = "TROLL600") %>%
            select(Timestamp, Temp = Temperature600,
                   Salinity = Salinity600, DO_mgl = RDO_concen600,
                   Pressure_psi = Pressure600, Instrument, Logger_ID = Statname) ->
            x600
    }

    x <- bind_rows(x200, x600)
    if(!nrow(x)) return(x)

    # Do some basic processing: set time zone
    x %>%
        mutate(Timestamp = ymd_hms(Timestamp, tz = tz))
}
