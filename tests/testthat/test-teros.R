test_that("read_teros_file works", {
    x <- read_teros_file("test_data/PNNL_11_Terosdata.dat")
    y <- readLines("test_data/PNNL_11_Terosdata.dat")
    expect_s3_class(x, "data.frame")
    expect_equal(nrow(x), length(y) - 4) # 4 header lines in TEROS files
})

test_that("process_teros_dir works locally", {
    # Currently this only tests non-Dropbox reading
    x <- process_teros_dir("test_data/", tz = "Europe/London")
    expect_s3_class(x, "data.frame")
    nfiles <- length(list.files("test_data/", pattern = "Terosdata\\.dat$"))
    expect_identical(length(unique(x$Logger)), nfiles)
    expect_identical(lubridate::tz(x$Timestamp[1]), "Europe/London") # timezone set correctly

    # Handles an empty directory
    x <- process_sapflow_dir(tempdir())
    expect_s3_class(x, "data.frame")
    expect_identical(nrow(x), 0L)
})
