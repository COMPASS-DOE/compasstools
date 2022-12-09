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
    expect_identical(dirname(names(x)), td)
    expect_identical(basename(x[[1]]), "test.csv")
    expect_identical(dirname(x[[1]]), a)
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

    # Generates messages with asked
    expect_silent(scan_folders(td, quiet = TRUE))
    suppressMessages({
        expect_message(scan_folders(td, quiet = FALSE), "files")
    })
})
