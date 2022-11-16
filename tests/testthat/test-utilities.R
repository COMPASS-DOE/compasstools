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


test_that("expand_string works", {
    # No expansion
    expect_identical("hello", expand_string("hello"))

    # Comma separator
    expect_identical(c("A", "B"), expand_string("{A,B}"))
    expect_identical(c("CA", "CB"), expand_string("C{A,B}"))
    expect_identical(c("AD", "BD"), expand_string("{A,B}D"))
    expect_identical("A,B", expand_string("A,B")) # no braces
    expect_identical("{A,B}", expand_string("{A,B}", expand_comma = FALSE))

    # Colon separator
    expect_identical(c("1", "2"), expand_string("{1:2}"))
    expect_identical(c("C1", "C2"), expand_string("C{1:2}"))
    expect_identical(c("1D", "2D"), expand_string("{1:2}D"))
    expect_identical("1:2", expand_string("1:2")) # no braces
    expect_identical("{A:B}", expand_string("{A:B}")) # no numbers
    expect_identical("{1:2}", expand_string("{1:2}", expand_colon = FALSE))

    # Nesting
    expect_identical(c("A", "B1", "B2", "C"), expand_string("{A,B{1:2},C}"))

    # Misc
    expect_message(expand_string("AAA", quiet = FALSE), regexp = "AAA")
})

test_that("expand_df works", {
    # No expansion
    x <- data.frame(x = 1)
    expect_identical(expand_df(x), x)
    x <- dplyr::tibble(x = 1)
    expect_identical(expand_df(x), x)

    # Comma separator
    x <- data.frame(C1 = "{A,B}", C2 = "1")
    y <- expand_df(x)
    expect_identical(nrow(y), 2L)
    expect_identical(y$C1, c("A", "B"))
    expect_identical(y$C2, c("1", "1")) # replicates

    x <- data.frame(C1 = "{A,B}", C2 = "{C,D}")
    y <- expand_df(x)
    expect_identical(y$C2, c("C", "D")) # sequences

    # Colon separator
    x <- data.frame(C1 = "{1:2}", C2 = "1")
    y <- expand_df(x)
    expect_identical(nrow(y), 2L)
    expect_identical(y$C1, c("1", "2"))
    expect_identical(y$C2, c("1", "1")) # replicates

    x <- data.frame(C1 = "{2:1}", C2 = "{C,D}")
    y <- expand_df(x)
    expect_identical(y$C1, c("2", "1")) # descending
    expect_identical(y$C2, c("C", "D")) # sequences

    # Nesting

})
