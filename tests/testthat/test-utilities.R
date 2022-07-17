test_that("calculate_skip works", {
    # Create a test dataset
    dat <- tibble(ts = seq.POSIXt(ymd_hms("2022-06-01 01:23:45"),
                                  ymd_hms("2022-06-30 12:34:56"),
                                  by = "hour"))
    for(col in letters) dat[[col]] <- rnorm(nrow(dat))
    tf <- file.path(tempdir(), "test.csv")
    readr::write_csv(dat, file = tf)

    # If no min_timestamp, should return number of header rows only (no skip)
    sk <- calculate_skip(tf, header_rows = 1, min_timestamp = NULL)
    expect_equal(sk, 1)

    # If min_timestamp before any date in file, no skip
    before_ts <- min(dat$ts) - 1
    sk <- calculate_skip(tf, header_rows = 1,
                         min_timestamp = before_ts, quiet = TRUE)
    expect_equal(sk, 1)

    # If min_timestamp at 50% point of file, should get the right number
    sk <- suppressMessages(
        calculate_skip(tf, header_rows = 1,
                       min_timestamp = dat$ts[nrow(dat) / 2], quiet = TRUE))
    expect_equal(sk, nrow(dat) / 2)

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
