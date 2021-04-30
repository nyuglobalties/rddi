context("DDI Variable specification")

test_that("Attribute is in set", {
    expect_equal(check_attribs_in_set("one", c("a", "one", "orange"), "test"), "one")
})

test_that("Element cardinality", {
    test_node <- ddi_var("test", ddi_notes("one"), ddi_notes("two"), ddi_notes("three"))
    expect_equal(check_cardinality(test_node$content, "notes"), 3)
})