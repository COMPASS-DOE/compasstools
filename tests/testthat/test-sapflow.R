test_that("read_sapflow_file works", {
    filename <- "test_data/PNNL_11_sapflow.dat"
    x <- read_sapflow_file(filename)
    expect_s3_class(x, "data.frame")
    y <- readLines(filename)
    expect_equal(nrow(x), length(y) - 4) # 4 header lines in sapflux files

    # min_timestamp works as expected...

    # timestamp before the file: reads all data
    z <- suppressMessages(
        read_sapflow_file(filename, min_timestamp =
                              ymd_hms(min(x$Timestamp[1])) - 1))
    expect_identical(nrow(z), nrow(x))

    # timestamp after the file: skips all data
    z <- suppressMessages(
        read_sapflow_file(filename, min_timestamp =
                              ymd_hms(max(x$Timestamp)) + 1))
    expect_identical(nrow(z), 0L)

    # timestamp in file: returns correct amount of data
    mid_timestamp <- x$Timestamp[nrow(x) / 2]
    z <- suppressMessages(
        read_sapflow_file(filename, mid_timestamp))
    expect_equal(z, subset(x, x$Timestamp >= mid_timestamp))
})

test_that("process_sapflow_dir works locally", {
    # Currently this only tests non-Dropbox reading
    x <- process_sapflow_dir("test_data/", tz = "Europe/London")
    expect_s3_class(x, "data.frame")
    nfiles <- length(list.files("test_data/", pattern = "sapflow\\.dat$"))
    expect_identical(length(unique(x$Logger)), nfiles)
    expect_identical(lubridate::tz(x$Timestamp[1]), "Europe/London") # timezone set correctly

    # Handles an empty directory
    x <- process_sapflow_dir(tempdir())
    expect_s3_class(x, "data.frame")
    expect_identical(nrow(x), 0L)
})
