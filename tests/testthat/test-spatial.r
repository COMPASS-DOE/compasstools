test_that("spatial works", {
    # handles bad inputs
    expect_error(nearest_neighbor_TMP(1:2), regexp = "length\\(location")
    expect_error(nearest_neighbor_TMP("xx"), regexp = "does not appear to be a TEMPEST grid cell")
    expect_error(nearest_neighbor_TMP("A1", radius = -1), regexp = "radius >= 0")

    # 0-radius
    expect_identical(nearest_neighbor_TMP("A1", radius = 0), "A1")
    # 1-radius
    expect_identical(sort(nearest_neighbor_TMP("C2", radius = 1)),
                     sort(c("B1", "C1", "D1", "B2", "C2", "D2", "B3", "C3", "D3")))
    # edge case
    expect_identical(sort(nearest_neighbor_TMP("A2", radius = 1)),
                     sort(c("A1", "B1", "A2", "B2", "A3", "B3")))
    # corner case
    expect_identical(sort(nearest_neighbor_TMP("A1", radius = 1)),
                     sort(c("A1", "B1", "A2", "B2")))
    # 2-radius
    expect_length(nearest_neighbor_TMP("C3", radius = 2), 25)
})
