# spatial.R

#' Find the nearest-neighbor grid squares - TEMPEST
#'
#' @param location Location grid cell ID (A1 to J8), character
#' @param radius Radius desired, 0 or greater (see examples), numeric
#'
#' @returns A vector of grid square IDs in a box of radius \code{radius}
#' @export
#'
#' @examples
#' nearest_neighbor_TMP("C2")
#' nearest_neighbor_TMP("A1") # returns four squares (other are out)
#' nearest_neighbor_TMP("F3", radius = 0) # returns F3
#' nearest_neighbor_TMP("F3", radius = 1) # returns the nine squares around F3
#'
#' # Construct a data frame of neighboring grid squares for a vector of locations
#' locations <- c("C2", "F3")
#' grid_map <- lapply(locations, function(x) data.frame(neighbor = nearest_neighbor_TMP(x)))
#' names(grid_map) <- locations
#' dplyr::bind_rows(grid_map, .id = "location")
nearest_neighbor_TMP <- function(location, radius = 1) {

    # Sanity checking
    stopifnot(length(location) == 1)
    if(!grepl("^[A-J][1-8]$", location)) {
        stop(location, " does not appear to be a TEMPEST grid cell")
    }

    x <- which(LETTERS == substr(location, 1, 1))
    xseq <- seq(x - radius, x + radius)
    # remove x values not in 1-10 (i.e., A-J) range
    xseq <- xseq[xseq > 0 & xseq < 11]

    y <- as.integer(substr(location, 2, 2))
    yseq <- seq(y - radius, y + radius)
    # remove x values not in 1-8 range
    yseq <- yseq[yseq > 0 & yseq < 9]

    # Generate all grid cell combinations
    df <- expand.grid(LETTERS[xseq], yseq)
    return(paste0(df[,1], df[,2]))
}
