# read_Lx functions

test_that("read_L1 works", {
    # Handles bad input
    expect_error(read_L1(letters[1:2]), regexp = "Only one variable")

    # Works
    x <- read_L1(variable = "gw-tds", path = "./test_data", quiet = TRUE)
    expect_s3_class(x, "data.frame")

    # Respects quiet flag
    expect_no_message(read_L1(variable = "gw-tds", path = "./test_data", quiet = TRUE))

    # Warns if no files found
    expect_warning(read_L1("A", quiet = TRUE), regexp = "No files found")
})


test_that("read_L2 works", {
    # Handles bad input
    expect_error(read_L2(letters[1:2]), regexp = "Only one variable")

    # Works
    x <- read_L2(variable = "sonde-fdom-rfu", path = "./test_data", quiet = TRUE)
    expect_s3_class(x, "data.frame")

    # Respects quiet flag
    expect_no_message(read_L2(variable = "sonde-fdom-rfu", path = "./test_data", quiet = TRUE))

    # Warns if no files found
    expect_warning(read_L2("A", quiet = TRUE), regexp = "No files found")
})
