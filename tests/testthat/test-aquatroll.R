test_that("read_aquatroll600_file works", {
    filename <- "test_data/PNNL_23_WaterLevel600.dat"
    x <- read_aquatroll600_file(filename)
    y <- readLines(filename)
    expect_s3_class(x, "data.frame")
    expect_equal(nrow(x), length(y) - 4) # 4 header lines in Aqua TROLL 600 files

    # min_timestamp works as expected...

    # timestamp before the file: reads all data
    z <- suppressMessages(
        read_aquatroll600_file(filename, min_timestamp =
                            ymd_hms(min(x$Timestamp[1])) - 1))
    expect_identical(nrow(z), nrow(x))

    # timestamp after the file: skips all data
    z <- suppressMessages(
        read_aquatroll600_file(filename, min_timestamp =
                            ymd_hms(max(x$Timestamp)) + 1))
    expect_identical(nrow(z), 0L)

    # timestamp in file: returns correct amount of data
    mid_timestamp <- x$Timestamp[nrow(x) / 2]
    z <- suppressMessages(
        read_aquatroll600_file(filename, mid_timestamp))
    expect_equal(z, subset(x, x$Timestamp >= mid_timestamp))
})

test_that("read_aquatroll200_file works", {
    filename <- "test_data/PNNL_21_WaterLevel200.dat"
    x <- read_aquatroll200_file(filename)
    y <- readLines(filename)
    expect_s3_class(x, "data.frame")
    expect_equal(nrow(x), length(y) - 4) # 4 header lines in Aqua TROLL 200 files

    # min_timestamp works as expected...

    # timestamp before the file: reads all data
    z <- suppressMessages(
        read_aquatroll200_file(filename, min_timestamp =
                            ymd_hms(min(x$Timestamp[1])) - 1))
    expect_identical(nrow(z), nrow(x))

    # timestamp after the file: skips all data
    z <- suppressMessages(
        read_aquatroll200_file(filename, min_timestamp =
                            ymd_hms(max(x$Timestamp)) + 1))
    expect_identical(nrow(z), 0L)

    # timestamp in file: returns correct amount of data
    mid_timestamp <- x$Timestamp[nrow(x) / 2]
    z <- suppressMessages(
        read_aquatroll200_file(filename, mid_timestamp))
    expect_equal(z, subset(x, x$Timestamp >= mid_timestamp))
})

test_that("process_aquatroll_dir works locally", {
    # Currently this only tests non-Dropbox reading
    x <- process_aquatroll_dir("test_data/", tz = "Europe/London")
    expect_s3_class(x, "data.frame")
    nfiles <- length(list.files("test_data/", pattern = "[2|6]00\\.dat$"))
    expect_identical(length(unique(x$Logger_ID)), nfiles)
    expect_identical(lubridate::tz(x$Timestamp[1]), "Europe/London") # timezone set correctly

    # Handles min_timestamp
    max_ts <- max(x$Timestamp)
    y <- process_aquatroll_dir("test_data/", tz = "EST",
                           min_timestamp = as.character(max_ts - 1),
                           quiet = TRUE)
    expect_gte(min(y$Timestamp), max_ts)
    y <- process_aquatroll_dir("test_data/", tz = "EST",
                           min_timestamp = as.character(max_ts + 1),
                           quiet = TRUE)
    expect_identical(nrow(y), 0L)

    # Handles an empty directory
    x <- process_sapflow_dir(tempdir())
    expect_s3_class(x, "data.frame")
    expect_identical(nrow(x), 0L)
})
