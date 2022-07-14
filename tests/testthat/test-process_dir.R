test_that("process_dir works", {
    # Custom progress bar and read file functions
    pb_count <- 0
    pb <- function(x) pb_count <<- pb_count + 1
    rf_count <- 0
    rf <- function(fn) {
        rf_count <<- rf_count + 1
        read.csv(fn)
    }
    # Empty dir; neither progress bar nor read function should get called
    td <- tempdir()
    out <- process_dir(td, "",
                       read_function = rf,
                       dropbox_token = NULL,
                       progress_bar = pb)
    expect_identical(pb_count, 0)
    expect_identical(rf_count, 0)
    expect_s3_class(out, "data.frame")

    # Write some files, both matching and non-matching,
    # into the tempdir...
    n <- 3
    lapply(seq_len(n), function(x) {
        readr::write_csv(cars, file.path(td, paste0(x, ".csv")))
        readr::write_csv(cars, file.path(td, paste0(x, ".txt")))
    })

    # ...and read
    out <- process_dir(td, "csv",
                       read_function = rf,
                       dropbox_token = NULL,
                       progress_bar = pb)
    expect_identical(pb_count, n)
    expect_identical(rf_count, n)
    expect_s3_class(out, "data.frame")
    expect_equal(nrow(out), n * nrow(cars))
    expect_identical(colnames(out), colnames(cars))
})
