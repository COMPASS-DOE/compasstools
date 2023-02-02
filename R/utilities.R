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


# Expand a string: look for patterns like {x,y,z} within a possibly
# larger string, and break them apart at the commas
# So "Hello {A,B2,C}" -> c("Hello A", "Hello B2", "Hello C")
# It also handles numerical sequences: "x{1:3}" -> c("x1", "x2", "x3")
# Comma expansions are performed before colon expansions:
# "{A,B{1:3},C}" -> c("A", "B1", "B2", "B3", "C")

#' Expand a string based on comma or colon patterns
#'
#' @param s String
#' @param expand_comma Look for comma expansions ("{X,Y,...}")? Logical
#' @param expand_colon Look for colon expansions ("{X:Y}")? Logical
#' @param quiet Be quiet or print diagnostic messages? Logical
#' @export
#' @return The expanded string, as a character vector
#' @description look for patterns like "{x,y,z}" within a possibly
# larger string, and break them apart at the commas
# So "Hello {A,B2,C}" -> c("Hello A", "Hello B2", "Hello C")
# It also handles numerical sequences: "x{1:3}" -> c("x1", "x2", "x3")
# Comma expansions are performed before colon expansions:
# "{A,B{1:3},C}" -> c("A", "B1", "B2", "B3", "C")
#' @examples
#' expand_string("{A,B2,C}")
#' expand_string("B0{1:3}")
expand_string <- function(s, expand_comma = TRUE, expand_colon = TRUE, quiet = TRUE) {
    if(!quiet) message(s)
    if(is.na(s)) return(s)

    # Look for 1+ "words" (groups of characters followed by a comma)
    # followed by commas (and perhaps white space), and then a final word
    if(expand_comma) {
        COMMA_PATTERN <- "\\{(.+,)+.+\\}"
        matches <- regexpr(COMMA_PATTERN, s)
        if(matches > 0) {
            subs <- strsplit(regmatches(s, matches), ",")[[1]]
            subs[1] <- gsub("^\\{", "", subs[1]) # get rid of beginning...
            subs[length(subs)] <- gsub("\\}$", "", subs[length(subs)]) # ...and end curly braces
            s <- rep(s, length(subs))
            newmatches <- regexpr(COMMA_PATTERN, s)
            regmatches(s, newmatches) <- trimws(subs)
            # Recurse once to look for possible colon expansions
            s <- unlist(sapply(s, expand_string,
                               expand_comma = FALSE, expand_colon = expand_colon,
                               USE.NAMES = FALSE))
            return(s)
        }
    }

    if(expand_colon) {
        # Look for two numbers separated by a colon, with optional white space
        COLON_PATTERN <- "\\{\\s*\\d+\\s*:\\s*\\d+\\s*\\}"
        matches <- regexpr(COLON_PATTERN, s)
        if(matches > 0) {
            subs <- strsplit(regmatches(s, matches), ":")[[1]]
            subs <- gsub("[\\{\\}]", "", subs) # get rid of curly braces
            subs <- seq.int(from = subs[1], to = subs[2])
            s <- rep(s, length(subs))
            newmatches <- regexpr(COLON_PATTERN, s)
            regmatches(s, newmatches) <- subs
        }
    }

    s
}



#

#' Expand a data frame by looking for string expansion patterns
#'
#' @param df The data frame to expand
#'
#' @return The expanded data frame.
#' @description Expand a data frame: look for patterns like x,y,z within
#' entries and, for that row, replicate it and use expand_string to break
#' apart the x,y,z. Multiple expansions within a row are OK as long as
#' they're the same length.
#' @export
expand_df <- function(df) {
    results <- list()
    for(i in seq_len(nrow(df))) {
        dfr <- df[i,,drop=FALSE]  # row we're working on
        # Figure out max expansion (may be 1) and replicate row that many times
        expand_lens <- sapply(dfr, function(x) length(expand_string(x)))
        new_df <- dfr[rep(1, max(expand_lens)),,drop=FALSE]

        # Can't have mismatches (except for 1-length)
        # E.g. "x,y" in one cell and "x,y,z" in another
        if(length(setdiff(unique(expand_lens), 1)) > 1) {
            stop("Row ", i, " has mismatched expansion entries")
        }

        # For each column, expand its entry string as needed
        if(nrow(new_df) > 1) {
            for(col in seq_along(new_df)) {
                new_df[col] <- expand_string(new_df[[1, col]])
            }
        }
        results[[i]] <- new_df
    }

    bind_rows(results)
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



#' Perform unit conversion based on a data frame of data and units table
#'
#' @param dat Data frame with \code{research_name} and \code{value} columns
#' @param ut Data frame with \code{research_name}, \code{conversion}, and \code{new_unit} columns
#' @param quiet Print progress messages?
#'
#' @return The data frame with new columns \code{value_conv} and \code{units}.
#' @export
#' @importFrom dplyr bind_rows
#'
#' @examples
#' x <- data.frame(value = 1:3, research_name = c("a", "a", "b"))
#' y <- data.frame(research_name = c("a", "b"), conversion = c("x * 1", "(x * 2) - 1"))
#' unit_conversion(x, y)
unit_conversion <- function(dat, ut, quiet = FALSE) {
    dat_conv <- list()

    # Isolate the various research_name entries one by one, find corresponding
    # conversion string, evaluate it against the `value` data column
    for(rn in unique(dat$research_name)) {
        d <- dat[dat$research_name == rn,] # filter
        x <- d$value # this `x` name is crucial; used by conversion strings
        which_ut <- which(ut$research_name == rn)
        # Isolate conversion string
        if(length(which_ut) == 0) {
            conv <- "<not found>"
            d$value_conv <- NA_real_
            d$units <- NA_character_
        } else if(length(which_ut) == 1) {
            conv <- ut$conversion[which_ut]
            # ...and evaluate it
            d$value_conv <- eval(parse(text = conv))
            d$units <- ut$new_unit[which_ut]
        } else {
            stop("Multiple conversions for ", rn)
        }

        if(!quiet) message("\t\tUC: ", rn, " n=", nrow(d), ", conv=", conv)

        dat_conv[[rn]] <- d
    }
    bind_rows(dat_conv)
}
