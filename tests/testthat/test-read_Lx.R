# read_Lx_variable functions

test_that("read_L1_variable works", {
    # Handles bad input
    expect_error(read_L1_variable(letters[1:2]), regexp = "Only one variable")

    # Single site
    x <- read_L1_variable(variable = "gw-tds", path = "./test_data", site = "TMP", quiet = TRUE)
    expect_s3_class(x, "data.frame")
    expect_true(all(x$Site == "TMP"))

    # Multiple sites
    x <- read_L1_variable(variable = "gw-tds", path = "./test_data", quiet = TRUE)
    expect_s3_class(x, "data.frame")
    expect_true(length(unique(x$Site)) > 1)

    # Respects quiet flag
    expect_no_message(read_L1_variable(variable = "gw-tds", path = "./test_data", quiet = TRUE))

    # Warns if no files found
    expect_warning(read_L1_variable("A", path = "./test_data", quiet = TRUE),
                   regexp = "No files found")
})


test_that("read_L2_variable works", {
    # Handles bad input
    expect_error(read_L2_variable(letters[1:2]), regexp = "Only one variable")

    # Single site
    x <- read_L2_variable(variable = "sonde-fdom-rfu", path = "./test_data", site = "OWC", quiet = TRUE)
    expect_s3_class(x, "data.frame")
    expect_true(all(x$Site == "OWC"))

    # Multiple sites
    x <- read_L2_variable(variable = "sonde-fdom-rfu", path = "./test_data", quiet = TRUE)
    expect_s3_class(x, "data.frame")
    expect_true(length(unique(x$Site)) > 1)

    # Respects quiet flag
    expect_no_message(read_L2_variable(variable = "sonde-fdom-rfu", path = "./test_data", quiet = TRUE))

    # Warns if no files found
    expect_warning(read_L2_variable("A", path = "./test_data", quiet = TRUE),
                   regexp = "No files found")
})
