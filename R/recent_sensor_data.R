# recent_sensor_data.R

#' Get recent sensor data from a COMPASS site.
#'
#' @param site Site name (CRC, DLG, GWI, OWC, PTR, SWH, or TMP), character
#' @param sensor Sensor name, character
#' @importFrom readr read_csv
#' @importFrom arrow read_parquet
#' @returns Either a table of available data, if no parameters are specified;
#' or the requested dataset based on \code{site} and \code{sensor}.
#' @export
#' @note The returned data have some metadata attached (e.g.,
#' \code{Instrument_ID}, \code{research_name}) but are not identical to
#' what is produced by the sensor data pipeline. In particular, no flagging,
#' unit conversion, or gap-filling is performed.
#' @examples
#' recent_sensor_data() # list available data
#' recent_sensor_data("DLG", "TEROS12")
#' @author BBL
recent_sensor_data <- function(site, sensor) {
    URL <- "https://github.com/COMPASS-DOE/sensor-data-preprocessor/raw/refs/heads/main/processed_data/"
    if(missing(site) || missing(sensor)) {
        read_csv(paste0(URL, "manifest.csv"),
                 show_col_types = FALSE)
    } else {
        url <- paste0()
        read_parquet(paste0(URL, site, "_", sensor, ".parquet"))
    }
}
