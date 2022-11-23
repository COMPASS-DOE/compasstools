test_that("read_datalogger works", {
    filename <- "test_data/PNNL_11_sapflow.dat"
    x <- read_datalogger_file(filename)
    expect_s3_class(x, "data.frame")
    y <- readLines(filename)
    expect_equal(nrow(x), length(y) - 4) # 4 header lines in sapflux files

    # Logger and table info parsed correctly
    expect_identical(colnames(x)[1], "Logger")
    expect_identical(colnames(x)[2], "Table")
    expect_true(all(x$Logger == "PNNL_11"))
    expect_true(all(x$Table == "sapflow"))
})
