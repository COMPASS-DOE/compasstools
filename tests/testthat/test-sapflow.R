test_that("read_sapflow_file works", {
    x <- read_sapflow_file("test_data/PNNL_11_sapflow.dat")
    expect_s3_class(x, "data.frame")
    expect_identical(nrow(x), 5L)
})

test_that("read_sapflow_dir works locally", {
    x <- read_sapflow_dir("test_data/")
    expect_s3_class(x, "data.frame")
    expect_identical(length(unique(x$Logger)), 2L)  # there are 2 files
})
