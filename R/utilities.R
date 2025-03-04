# Utilities


#' Calculate lines of data to skip
#'
#' @param filename Filename, character
#' @param header_rows Number of header rows, integer
#' @param min_timestamp Minimum timestamp caller is looking for, character
#' @param parse_function Parse function to handle timestamps
#' @param quiet Print diagnostic message? Logical
#' @description This internal function returns the number of lines of a
#' file to skip based on a requested minimum timestamp to read. It does so
#' by 'peeking' at the first and last timestamps of the data, ignoring any
#' header lines. We assume that the timestamps of the
#' data are in ascending order and regular in their spacing.
#' @return The number of lines OF THE DATA (not of the file) to skip.
#' If all data should be skipped, a -1 is returned.
#' @note For high performance this uses the \code{fpeek} package.
#' @import fpeek
#' @importFrom utils capture.output tail
calculate_skip <- function(filename, header_rows, min_timestamp,
                           parse_function = ymd_hms, quiet = FALSE) {
    if(is.null(min_timestamp)) return(0) # no data skip

    firstline <- tail(capture.output(peek_head(filename, header_rows + 1)), 1)
    lastline <-  capture.output(peek_tail(filename, n = 1))
    nlines <-  fpeek::peek_count_lines(filename) - header_rows

    # Assume the timestamp is the first field and comma-separated
    # So far this is true for all data types; if not in the future,
    # may need to add split_char and field_num parameters
    first_ts <- strsplit(firstline, ",")[[1]][1]
    last_ts <- strsplit(lastline, ",")[[1]][1]
    fts <- parse_function(first_ts)
    lts <- parse_function(last_ts)
    mts <- parse_function(min_timestamp)
    if(is.na(mts)) {
        stop("Could not parse min_timestamp into POSIXct")
    }

    frac <- as.numeric(difftime(mts, fts, units = "secs")) /
        as.numeric(difftime(lts, fts, units = "secs"))
    skip <- as.integer(nlines * frac)
    if(!quiet) {
        message("Skipping ", skip, " rows (", round(frac * 100, 0), "%) of ",
                basename(filename), " data (min_timestamp = ", mts, ")")
    }
    if(skip >= nlines) {
        return(-1)
    } else if(skip < 0) {
        return(0)
    } else {
        return(skip)
    }
}


#' Scan sub-folders
#'
#' @param root_dir Root directory to start from, character
#' @param file_pattern Regex for files, character
#' @param quiet Be quiet or print diagnostic messages? Logical
#'
#' @return A named list of folder contents; each list object name is the
#' name of the folder, and each object in the list is a vector of
#' fully qualified filenames.
#' @note Does not recurse into sub-folders.
#' @export
#'
#' @examples
#' scan_folders("./")
scan_folders <- function(root_dir, file_pattern = "\\.csv$", quiet = TRUE) {
    if(!dir.exists(root_dir)) {
        stop("Directory doesn't exist!")
    }
    entries <- list.files(root_dir, full.names = TRUE)

    folder_list <- list()
    for(e in entries) {
        if(!quiet) message(e)
        if(dir.exists(e)) { # it's a folder
            files <- list.files(e, pattern = file_pattern, full.names = TRUE)
            if(!quiet) message("\t", length(files), " files")
            if(length(files)) {  # ...with csv files!
                folder_list[[e]] <- files
            }
        }
    }

    return(folder_list)
}


#' A recursive function to print a nicely-formatted directory tree and its files
#'
#' @param dir_list A list of directories
#' @param outfile Optional file to write output to
#' @param prefix Line prefix; used by the function as it recurses
#' @param pattern Optional file pattern
#' @param list_files List files in each directory? TRUE by default
#'
#' @return Nothing; used only for its side effects (i.e. printing).
#' @export
#' @examples
#' \dontrun{
#' list_directories(list("pipeline/Raw/", "pipeline/L0/",
#'                       "pipeline/L1_normalize/",
#                       "pipeline/L1/", "pipeline/L2"))
#' }
list_directories <- function(dir_list, outfile = "", prefix = "",
                             pattern = NULL, list_files = TRUE) {

    for(d in dir_list) {
        # Print the directory name
        cat(paste0(prefix, "|\n"), file = outfile, append = TRUE)
        cat(paste0(prefix, "|- ", basename(d), "/"), "\n", file = outfile, append = TRUE)

        # As we list items, print a vertical pipe except for the last
        if(d == tail(dir_list, 1)) {
            thisprefix <- ""
        } else {
            thisprefix <- "|"
        }

        # Print files in this directory; track but don't print subdirectories
        files <- list.files(d, full.names = TRUE, pattern = pattern)
        subdirs <- list()
        filecount <- 0
        for(f in files) {
            if(dir.exists(f)) {
                subdirs[[f]] <- f
            } else {
                filecount <- filecount + 1
                if(list_files) cat(paste0(prefix, thisprefix, "\t|-"),
                                   basename(f), "\n",
                                   file = outfile, append = TRUE)
            }
        }
        if(!list_files) cat(paste0(prefix, thisprefix, "\t|- (", filecount, " file",
                                   ifelse(filecount == 1, "", "s"), ")\n"),
                            file = outfile, append = TRUE)

        # Now recurse for any subdirectories
        newprefix <- paste0(prefix, "|\t")
        list_directories(subdirs, outfile, prefix = newprefix, list_files = list_files)
    }
}
