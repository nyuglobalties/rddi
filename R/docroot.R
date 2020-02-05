ddi_codeBook <- function(...) {
  schema <- list(
    version = "2.5",
    xmlns = "ddi:codebook:2_5",
    `xmlns:xsi` = "http://www.w3.org/2001/XMLSchema-instance",
    `xsi:schemaLocation` = "http://www.ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/codebook.xsd"
  )

  components <- dots_to_xml_components(...)

  allowed_children <- c(
    "dataDscr",
    "docDscr",
    "fileDscr",
    "otherMat",
    "studyDscr"
  )

  build_branch_node(
    "codeBook",
    allowed_children = allowed_children,
    root = TRUE,
    content = components$content,
    attribs = schema
  )
}

ddi_dataDscr <- function(...) {
  allowed_children <- c(
    "nCube",
    "nCubeGrp",
    "notes",
    "var",
    "varGrp"
  )

  components <- dots_to_xml_components(...)

  build_branch_node(
    "dataDscr",
    allowed_children = allowed_children,
    content = components$content
  )
}
