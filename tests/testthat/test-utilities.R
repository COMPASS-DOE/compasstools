test_that("calculate_skip works", {
    # Create a test dataset
    dat <- tibble(ts = seq.POSIXt(ymd_hms("2022-06-01 01:23:45"),
                                  ymd_hms("2022-06-30 12:34:56"),
                                  by = "hour"))
    for(col in letters) dat[[col]] <- rnorm(nrow(dat))
    tf <- file.path(tempdir(), "test.csv")
    readr::write_csv(dat, file = tf)

    # If no min_timestamp, should return zero (no skip)
    sk <- calculate_skip(tf, header_rows = 1, min_timestamp = NULL)
    expect_equal(sk, 0)

    # If min_timestamp before any date in file, no skip
    before_ts <- min(dat$ts) - 1
    sk <- calculate_skip(tf, header_rows = 1,
                         min_timestamp = before_ts, quiet = TRUE)
    expect_equal(sk, 0)

    # If min_timestamp at 50% point of file, should get the right number
    sk <- suppressMessages(
        calculate_skip(tf, header_rows = 1,
                       min_timestamp = dat$ts[nrow(dat) / 2], quiet = TRUE))
    expect_equal(sk, nrow(dat) / 2 - 1)

    # If min_timestamp beyond all dates in file, -1 (signal to skip file)
    sk <- suppressMessages(
        calculate_skip(tf, header_rows = 1,
                       min_timestamp = max(dat$ts) + 1, quiet = TRUE))
    expect_equal(sk, -1)

    # Honors 'quiet' parameter
    expect_silent(calculate_skip(tf, 1, before_ts, quiet = TRUE))
    expect_message(calculate_skip(tf, 1, before_ts, quiet = FALSE), "Skipping")

    # Errors on bad min_timestamp
    expect_error(
        suppressWarnings(calculate_skip(tf, 1, "2022-05-01", quiet = TRUE)),
        "Could not parse")
})


test_that("scan_folders works", {
    # Nonexistent parent folder
    expect_error(scan_folders("folder_doesnt_exist"), "doesn't exist")

    # Empty folder
    td <- file.path(tempdir(), "test")
    dir.create(td)
    expect_identical(scan_folders(td), list())
    # Folder with files, no subfolders
    file.create(file.path(td, "test.csv"))
    expect_identical(scan_folders(td), list())
    # Empty subfolder
    a <- file.path(td, "a")
    dir.create(a)
    expect_identical(scan_folders(td), list())
    # File in a folder!
    fn <- file.path(td, "a", "test.csv")
    file.create(fn)
    x <- scan_folders(td)
    expect_identical(length(x), 1L)
    expect_identical(basename(names(x)), "a")
    expect_identical(normalizePath(dirname(names(x))), normalizePath(td))
    expect_identical(basename(x[[1]]), "test.csv")
    expect_identical(normalizePath(dirname(x[[1]])), normalizePath(a))
    # Adding a non-csv file shouldn't change anything
    file.create(file.path(td, "a", "test.png"))
    expect_identical(scan_folders(td), x)
    # Second file
    file.create(file.path(td, "a", "test2.csv"))
    y <- scan_folders(td)
    expect_identical(length(y), 1L)
    expect_identical(length(y[[1]]), 2L)
    expect_identical(basename(y[[1]]), c("test.csv", "test2.csv"))
    # Second folder
    b <- file.path(td, "b")
    dir.create(b)
    expect_identical(scan_folders(td), y) # no new files so no change
    file.create(file.path(td, "b", "test3.csv"))
    z <- scan_folders(td)
    expect_identical(length(z), 2L)
    expect_identical(z[[1]], y[[1]]) # no change to first entry
    expect_identical(basename(names(z)[2]), "b")
    expect_identical(length(z[[2]]), 1L)
    expect_identical(basename(z[[2]]), "test3.csv")

    # Respects file pattern
    expect_identical(scan_folders(td, file_pattern = "kjb"), list())

    # Generates messages with asked
    expect_silent(scan_folders(td, quiet = TRUE))
    suppressMessages({
        expect_message(scan_folders(td, quiet = FALSE), "files")
    })
})
