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
#' @details
#' The full list of available variables can be founded in the L1 metadata.
#' They include:
#' battery-voltage, solar-voltage, sapflow-3.5cm, sapflow-5cm, redox-5cm,
#' redox-10cm, redox-15cm, redox-20cm, redox-25cm, redox-35cm, redox-55cm,
#' soil-vwc-5cm, soil-temp-5cm, soil-EC-5cm, soil-vwc-10cm, soil-temp-10cm,
#' soil-EC-10cm, soil-vwc-15cm, soil-temp-15cm, soil-EC-15cm,
#' soil-vwc-30cm, soil-temp-30cm, soil-EC-30cm, soil-vwc-40cm,
#' soil-temp-40cm, soil-EC-40cm, soil-vwc-50cm, soil-temp-50cm,
#' soil-EC-50cm, soil-vwc-70cm, soil-temp-70cm, soil-EC-70cm,
#' gw-bar-pressure, gw-temperature, gw-temperature-int, gw-act-cond,
#' gw-spec-cond, gw-salinity, gw-tds, gw-density, gw-resistivity, gw-ph,
#' gw-ph-mv, gw-ph-orp, gw-rdo-conc, gw-perc-sat, gw-part-pressure,
#' gw-pressure-unvented, gw-pressure-vented, gw-depth, gw-voltage-ext,
#' gw-battery, wx-slr-fd15, wx-slr-tf15, wx-rain15, wx-windspeed15,
#' wx-winddir15, wx-winddir-sd115, wx-maxws15, wx-maxws-tmx15,
#' wx-invalid-wind15, wx-tempavg15, wx-tempmax15, wx-temptmx15,
#' wx-tempmin15, wx-temptmn15, wx-vappress15, wx-barpress15, wx-bpmax15,
#' wx-bptmx15, wx-bpmin15, wx-bptmn15, wx-rh15, wx-rht15, wx-vpdmax15,
#' wx-vpdmin15, wx-vpdtmx15, wx-vpdtmn15, wx-tilt-ns15, wx-tilt-we15,
#' wx-strikes15, wx-dist-min15, wx-dist-tmn15, wx-par-den15, wx-par-tot15,
#' wx-rain24, wx-slr-fd24, wx-slr-tf24, wx-winderrors24, wx-maxws24,
#' wx-maxws-tmx24, wx-tempavg24, wx-tempmax24, wx-temptmx24, wx-tempmin24,
#' wx-temptmn24, wx-vappress24, wx-barpress24, wx-bpmax24, wx-bptmx24,
#' wx-bpmin24, wx-bptmn24, wx-rh24, wx-rht24, wx-rhtmax24, wx-rhtmin24,
#' wx-tilt-ns24, wx-vpd24, wx-vpdmax24, wx-vpdmin24, wx-vpdtmx24,
#' wx-vpdtmn24, wx-tilt-we24, wx-par-den24, wx-par-tot24, wx-vp15,
#' wx-vpd15, wx-svp15, wx-minws15, wx-gcrew-rain15, sonde-conductivity,
#' sonde-fdom, sonde-fdom-rfu, sonde-nlf-cond, sonde-odo-sat,
#' sonde-odo-local, sonde-odo-mgl, sonde-pressure, sonde-salinity,
#' sonde-spcond, sonde-wiperpos, sonde-ph, sonde-ph-mv, sonde-temp,
#' sonde-depth, sonde-battery, sonde-cable, sonde-wipercur, sonde-tss,
#' sonde-orp, sonde-tds, sonde-vpos, sonde-turbidity, sonde-turb-raw,
#' sonde-chlorophyl, sonde-chl-rfu, sonde-chl-raw, sonde-chl-cells,
#' sonde-rhodamine, sonde-tal, sonde-bga-pc, sonde-bga-pc-raw.
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
#' @details
#' The full list of available variables can be founded in the L2 metadata.
#' They include:
#' battery-voltage, solar-voltage, sapflow-3.5cm, sapflow-5cm, redox-5cm,
#' redox-10cm, redox-15cm, redox-20cm, redox-25cm, redox-35cm, redox-55cm,
#' soil-vwc-5cm, soil-temp-5cm, soil-EC-5cm, soil-vwc-10cm, soil-temp-10cm,
#' soil-EC-10cm, soil-vwc-15cm, soil-temp-15cm, soil-EC-15cm,
#' soil-vwc-30cm, soil-temp-30cm, soil-EC-30cm, soil-vwc-40cm,
#' soil-temp-40cm, soil-EC-40cm, soil-vwc-50cm, soil-temp-50cm,
#' soil-EC-50cm, soil-vwc-70cm, soil-temp-70cm, soil-EC-70cm,
#' gw-bar-pressure, gw-temperature, gw-temperature-int, gw-act-cond,
#' gw-spec-cond, gw-salinity, gw-tds, gw-density, gw-resistivity, gw-ph,
#' gw-ph-mv, gw-ph-orp, gw-rdo-conc, gw-perc-sat, gw-part-pressure,
#' gw-pressure-unvented, gw-pressure-vented, gw-depth, gw-voltage-ext,
#' gw-battery, wx-slr-fd15, wx-slr-tf15, wx-rain15, wx-windspeed15,
#' wx-winddir15, wx-winddir-sd115, wx-maxws15, wx-maxws-tmx15,
#' wx-invalid-wind15, wx-tempavg15, wx-tempmax15, wx-temptmx15,
#' wx-tempmin15, wx-temptmn15, wx-vappress15, wx-barpress15, wx-bpmax15,
#' wx-bptmx15, wx-bpmin15, wx-bptmn15, wx-rh15, wx-rht15, wx-vpdmax15,
#' wx-vpdmin15, wx-vpdtmx15, wx-vpdtmn15, wx-tilt-ns15, wx-tilt-we15,
#' wx-strikes15, wx-dist-min15, wx-dist-tmn15, wx-par-den15, wx-par-tot15,
#' wx-rain24, wx-slr-fd24, wx-slr-tf24, wx-winderrors24, wx-maxws24,
#' wx-maxws-tmx24, wx-tempavg24, wx-tempmax24, wx-temptmx24, wx-tempmin24,
#' wx-temptmn24, wx-vappress24, wx-barpress24, wx-bpmax24, wx-bptmx24,
#' wx-bpmin24, wx-bptmn24, wx-rh24, wx-rht24, wx-rhtmax24, wx-rhtmin24,
#' wx-tilt-ns24, wx-vpd24, wx-vpdmax24, wx-vpdmin24, wx-vpdtmx24,
#' wx-vpdtmn24, wx-tilt-we24, wx-par-den24, wx-par-tot24, wx-vp15,
#' wx-vpd15, wx-svp15, wx-minws15, wx-gcrew-rain15, sonde-conductivity,
#' sonde-fdom, sonde-fdom-rfu, sonde-nlf-cond, sonde-odo-sat,
#' sonde-odo-local, sonde-odo-mgl, sonde-pressure, sonde-salinity,
#' sonde-spcond, sonde-wiperpos, sonde-ph, sonde-ph-mv, sonde-temp,
#' sonde-depth, sonde-battery, sonde-cable, sonde-wipercur, sonde-tss,
#' sonde-orp, sonde-tds, sonde-vpos, sonde-turbidity, sonde-turb-raw,
#' sonde-chlorophyl, sonde-chl-rfu, sonde-chl-raw, sonde-chl-cells,
#' sonde-rhodamine, sonde-tal, sonde-bga-pc, sonde-bga-pc-raw.
#' @details
#' Derived variables include soil-salinity-Xcm and gw-wl-below-surface.
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
