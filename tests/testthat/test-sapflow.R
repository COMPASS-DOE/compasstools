test_that("read_sapflow_file works", {
    x <- read_sapflow_file("test_data/PNNL_11_sapflow_1min.dat")
    expect_s3_class(x, "data.frame")
    expect_identical(nrow(x), 5L)
})
