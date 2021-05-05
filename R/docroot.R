#' Codebook
#' 
#' The root node of a DDI 2.5 Codebook file. This file must contain stdyDscr
#' 
#' @param ... Child nodes or attributes
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/codeBook.html}{codeBook documentation}
#' 
#' @export
ddi_codeBook <- function(...) {
  components <- dots_to_xml_components(...)
  content <- components$content
  attribs <- components$attribs
  attribs$version <- "2.5"
  attribs$xmlns <- "ddi:codebook:2_5"
  attribs$`xmlns:xsi` <- "http://www.w3.org/2001/XMLSchema-instance"
  attribs$`xsi:schemaLocation` <- "http://www.ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/codebook.xsd"


  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "version", "codeBookAgency", "xmlns", "xmlns:xsi", "xsi:schemaLocation")
    if(!is.null(attribs$version) & attribs$version != "2.5") attribs$version = "2.5"
    attribs <- validate_attributes(attribs, allowed_attribs, "codeBook")
  }
  
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
    attribs = attribs
  )
}

#' stdyDscr and its children
#' 
#' All DDI codebooks must have a study description which
#' contains information about the study overall, including
#' a citation to the work(s) referenced in the codebook. At least
#' one citation must be present, capturing the whole study.
#'
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/stdyDscr.html}{stdyDscr documentation}
#' 
#' @export
ddi_stdyDscr <- function(...) {
  allowed_children <- c(
    "citation",
    "studyAuthorization",
    "stdyInfo",
    "studyDevelopment",
    "method",
    "dataAccs",
    "othrStdyMat",
    "notes"
  )

  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "access")
    attribs <- validate_attributes(attribs, allowed_attribs, "stdyDscr")
  }

  build_branch_node(
    "stdyDscr",
    allowed_children = allowed_children,
    required_children = "citation",
    attribs = attribs,
    content = components$content
  )
}

#' dataDscr and its children
#' 
#' Description of variables within the Codebook. 
#' 
#' @param ... Child nodes or attributes. 
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataDscr.html}{dataDscr documentation}
#' 
#' @export
ddi_dataDscr <- function(...) {
  allowed_children <- c(
    "nCube",
    "nCubeGrp",
    "notes",
    "var",
    "varGrp"
  )

  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "dataDscr")
  }

  build_branch_node(
    "dataDscr",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' fileDscr and its children
#' 
#' Information about the data file(s) that comprises a collection. 
#' 
#' @param ... Child nodes or attributes. 
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataDscr.html}{dataDscr documentation}
#' 
#' @export
ddi_fileDscr <- function(...) {
  allowed_children <- c(
    "fileTxt",
    "locMap",
    "notes"
  )

  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "URI", "sdatrefs", "methrefs", "pubrefs")
    attribs <- validate_attributes(attribs, allowed_attribs, "fileDscr")
  }

  if(check_cardinality(components$content, "locMap") > 1) rddi_err("Only 0 or 1 locMap children are allowed in fileDscr")

  build_branch_node(
    "fileDscr",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}
