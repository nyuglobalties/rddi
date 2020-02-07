context("XML interoperability checks")

test_that("DDI to XML conversion loses no information", {
  test_node <- build_branch_node("branchtest", attribs = list(attr = "yes"))

  expect_equal(as_ddi(as_xml(test_node)), test_node)
})
