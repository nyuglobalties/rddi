context("Unwrapped nodes meet DDI non-strict compliance")

test_that("Unwrapping is consistent", {
  expect_error(unwrap())

  # Attributes are not forwarded
  expect_error(unwrap(test = "thing"))

  test <- unwrap("text", ddi_labl("label"))

  # Mixed content is allowed
  expect_equivalent(length(test$content), 2)
  expect_true(is_ddi_node(test$content[[2]]))
})

test_that("XML conversion of unwrapped nodes makes sense", {
  # XML unwrapping can only occur in the context of a parent node
  test <- unwrap("text", ddi_labl("label"))

  err <- expect_error(as_xml(test))
  expect_s3_class(err, "ddi_err_unwrap_noparent")

  uni <- function(...) {
    components <- dots_to_xml_components(...)

    build_branch_node(
      "universe",
      components = components
    )
  }

  test_uni <- uni(test)
  xml_uni <- as_xml(test_uni)

  # Docroot not included
  expect_equivalent(
    xml2::xml_text(xml_uni),
    "text<labl>label</labl>"
  )
})

test_that("Unwrapped argument forwarding for node creation works", {
  uni <- function(..., world = "this") {
    components <- dots_to_xml_components(...)
    content <- unwrap_content(components$content)
    attribs <- components$attribs

    attribs$world <- world

    build_branch_node(
      "universe",
      content = content,
      attribs = attribs
    )
  }

  test_uni <- uni("text", ddi_labl("label"), thing = "stuff", world = "another")

  expect_s3_class(test_uni$content[[1]], "ddi_unwrapped")
  expect_true(all(c("world", "thing") %in% names(test_uni$attribs)))

  xml_uni <- as_xml(test_uni)
  expect_equivalent(
    xml2::xml_text(xml_uni),
    "text<labl>label</labl>"
  )
})

test_that("Child inspection forwarding occurs correctly", {
  concept <- simple_leaf_node("concept")
  txt <- simple_leaf_node("txt")
  bad <- simple_leaf_node("bad")

  test_uni1 <- function(...) {
    components <- dots_to_xml_components(...)
    content <- unwrap_content(components$content)
    attribs <- components$attribs

    build_branch_node(
      "uni",
      content = content,
      attribs = attribs,
      allowed_children = c("txt", "concept")
    )
  }

  test_uni2 <- function(...) {
    components <- dots_to_xml_components(...)
    content <- unwrap_content(components$content)
    attribs <- components$attribs

    build_branch_node(
      "uni",
      content = content,
      attribs = attribs,
      allowed_children = c("txt", "concept"),
      required_children = "concept"
    )
  }

  expect_error(test_uni1(
    "Text",
    txt("text"),
    concept("concept"),
    bad("this is bad")
  ), class = "rddi_unallowed_child_error")

  expect_error(test_uni2(
    "more text",
    txt("text")
  ), class = "rddi_missing_required_child_error")
})

test_that("mixed content variables produce valid DDI", {
  mixed_content_cb_othrStdyMat <- ddi_codeBook(
    ddi_stdyDscr(
      ddi_citation(
        ddi_titlStmt(
          ddi_titl("Study title")
        )
      ),
      ddi_othrStdyMat(
        ddi_relMat(
          "description of related material",
          ddi_citation(
            ddi_titlStmt(
              ddi_titl("Title of Related Material")
            )
          )
        )
      )
    )
  )
  
  mixed_content_cb_sumDscr <- ddi_codeBook(
    ddi_stdyDscr(
      ddi_citation(
        ddi_titlStmt(
          ddi_titl("Study title")
        )
      ),
      ddi_stdyInfo(
        ddi_sumDscr(
          ddi_dataKind(
            "survey data",
            ddi_txt("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed 
                  do eiusmod tempor incididunt...")
          )
        )
      )
    )
  )
  
  mixed_content_cb_data <- ddi_codeBook(
    ddi_stdyDscr(
      ddi_citation(
        ddi_titlStmt(
          ddi_titl("Study title")
        )
      )
    ),
    ddi_dataDscr(
      ddi_var(varname = "var1", 
              ddi_labl("Label of variable 1"),
              ddi_qstn(ID = "Q1", "Please answer question 1",
                       ddi_postQTxt("Question 2 will follow"))
              )
    )
  )
    
  expect_equivalent(validate_codebook(mixed_content_cb_othrStdyMat), TRUE)
  expect_equivalent(validate_codebook(mixed_content_cb_sumDscr), TRUE)
  expect_equivalent(validate_codebook(mixed_content_cb_data), TRUE)
})