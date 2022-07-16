test_that("read_aquatroll600_file works", {
    x <- read_aquatroll600_file("test_data/PNNL_23_WaterLevel600.dat")
    y <- readLines("test_data/PNNL_23_WaterLevel600.dat")
    expect_s3_class(x, "data.frame")
    expect_equal(nrow(x), length(y) - 4) # 4 header lines in Aqua TROLL 600 files
})

test_that("read_aquatroll200_file works", {
    x <- read_aquatroll200_file("test_data/PNNL_21_WaterLevel200.dat")
    y <- readLines("test_data/PNNL_21_WaterLevel200.dat")
    expect_s3_class(x, "data.frame")
    expect_equal(nrow(x), length(y) - 4) # 4 header lines in Aqua TROLL 200 files
})

test_that("process_aquatroll_dir works locally", {
    # Currently this only tests non-Dropbox reading
    x <- process_aquatroll_dir("test_data/", tz = "Europe/London")
    expect_s3_class(x, "data.frame")
    nfiles <- length(list.files("test_data/", pattern = "[2|6]00"))
    expect_identical(length(unique(x$Logger_ID)), nfiles)
    expect_identical(lubridate::tz(x$Timestamp[1]), "Europe/London") # timezone set correctly

    # Handles an empty directory
    x <- process_sapflow_dir(tempdir())
    expect_s3_class(x, "data.frame")
    expect_identical(nrow(x), 0L)
})
