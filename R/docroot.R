ddi_codeBook <- function(...) {
  schema <- list(
    version = "2.5",
    xmlns = "ddi:codebook:2_5",
    `xmlns:xsi` = "http://www.w3.org/2001/XMLSchema-instance",
    `xsi:schemaLocation` = "http://www.ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/codebook.xsd"
  )

  components <- dots_to_xml_components(...)
  content <- components$content

  allowed_children <- c(
    "dataDscr",
    "docDscr",
    "fileDscr",
    "otherMat",
    "stdyDscr"
  )

  build_branch_node(
    "codeBook",
    allowed_children = allowed_children,
    required_children = "stdyDscr",
    root = TRUE,
    content = content,
    attribs = schema
  )
}

#' Study description
#' 
#' All DDI codebooks must have a study description which
#' contains information about the study overall, including
#' a citation to the work(s) referenced in the codebook. At least
#' one citation must be present, capturing the whole study.
#'
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @export
ddi_stdyDscr <- function(...) {
  allowed_children <- c(
    "citation",
    "studyAuthorization"
  )

  components <- dots_to_xml_components(...)
  
  build_branch_node(
    "stdyDscr",
    allowed_children = allowed_children,
    required_children = "citation",
    attribs = components$attribs,
    content = components$content
  )
}

#' Create a DDI citation
#'
#' Citation entities belong in study and DDI document description blocks.
#'
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#' 
#' @export
ddi_citation <- function(...) {
  allowed_children <- c(
    "titlStmt",
    "biblCit"
  )

  components <- dots_to_xml_components(...)

  build_branch_node(
    "citation",
    allowed_children = allowed_children,
    required_children = "titlStmt",
    components = components
  )
}

#' @rdname ddi_citation
#' @export
ddi_titlStmt <- function(...) {
  allowed_children <- c(
    "titl",
    "subTitl",
    "altTitl",
    "parTitl"
  )

  components <- dots_to_xml_components(...)

  build_branch_node(
    "titlStmt",
    allowed_children = allowed_children,
    components = components
  )
}

ddi_titl <- simple_leaf_node("titl")
ddi_subTitl <- simple_leaf_node("subTitl")
ddi_altTitl <- simple_leaf_node("altTitl")

ddi_parTitl <- function(language, ...) {
  stopifnot(is.character(language))

  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if (is.null(attribs)) {
    attribs <- list()
  }

  attribs[["xml:lang"]] <- language

  build_leaf_node(
    "parTitl",
    attribs = attribs,
    content = components$content
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
    components = components
  )
}
