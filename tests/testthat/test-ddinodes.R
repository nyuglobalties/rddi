context("DDI node creation")

test_that("Creation of branch nodes makes sense", {

  test_branch <- function(...) {
    components <- dots_to_xml_components(...)

    children <- c("x", "y")

    build_branch_node(
      "branch",
      allowed_children = children,
      attribs = components$attribs,
      content = components$content
    )
  }

  x <- build_leaf_node("x", content = "Hey")
  y <- build_leaf_node("y", content = "Ma")
  z <- build_branch_node("z")

  expect_error(test_branch("Invalid text content"), class = "rddi_error")
  expect_error(test_branch(x, y, z), class = "rddi_error")
  expect_silent(test_branch(x, y))

  test_branch2 <- function(...) {
    components <- dots_to_xml_components(...)

    allowed_children <- c("x", "y")
    req_children <- "x"

    build_branch_node(
      "branch",
      allowed_children = children,
      required_children = req_children,
      components = components
    )
  }

  expect_error(test_branch2(), class = "rddi_error")
})
