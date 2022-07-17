test_that("read_sapflow_file works", {
    x <- read_sapflow_file("test_data/PNNL_11_sapflow.dat")
    expect_s3_class(x, "data.frame")
    y <- readLines("test_data/PNNL_11_sapflow.dat")
    expect_equal(nrow(x), length(y) - 4) # 4 header lines in sapflux files
})

test_that("process_sapflow_dir works locally", {
    # Currently this only tests non-Dropbox reading
    x <- process_sapflow_dir("test_data/", tz = "Europe/London")
    expect_s3_class(x, "data.frame")
    expect_identical(length(unique(x$Logger)), 2L)  # there are 2 files
    expect_identical(lubridate::tz(x$Timestamp[1]), "Europe/London") # timezone set correctly

    # Handles an empty directory
    x <- process_sapflow_dir(tempdir())
    expect_s3_class(x, "data.frame")
    expect_identical(nrow(x), 0L)
})
