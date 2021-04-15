#' Codebook
#' 
#' The root node of a DDI 2.5 Codebook file. This file must contain stdyDscr
#' 
#' @param ... Child nodes or attributes
#'
#' @section DDI Codebook 2.5 Documentation:
#'
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/codeBook.html}
#' 
#' @export
ddi_codeBook <- function(...) {
#  schema <- list(
#    version = "2.5",
#    xmlns = "ddi:codebook:2_5",
#    `xmlns:xsi` = "http://www.w3.org/2001/XMLSchema-instance",
#    `xsi:schemaLocation` = "http://www.ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/codebook.xsd"
#  )
# NEED TO ASK ABOUT ABOVE

  components <- dots_to_xml_components(...)
  content <- components$content
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "version", "codeBookAgency")
    if(!is.null(attribs$version) & attribs$version != "2.5") attribs$version = "2.5"
    check_attribs_in_set(names(attribs), allowed_attribs, field = "codebook")
    check_attribs(attribs)
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

#' Study description
#' 
#' All DDI codebooks must have a study description which
#' contains information about the study overall, including
#' a citation to the work(s) referenced in the codebook. At least
#' one citation must be present, capturing the whole study.
#'
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation:
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/stdyDscr.html}
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
    check_attribs_in_set(names(attribs), allowed_attribs, field = "stdyDscr")
    check_attribs(attribs)
  }

  build_branch_node(
    "stdyDscr",
    allowed_children = allowed_children,
    required_children = "citation",
    attribs = attribs,
    content = components$content
  )
}

#' Variable description
#' 
#' Description of variables within the Codebook. 
#' @param ... Child nodes or attributes. 
#'
#' @section DDI Codebook 2.5 Documentation:
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataDscr.html}
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
    check_attribs_in_set(names(attribs), allowed_attribs, field = "dataDscr")
    check_attribs(attribs)
  }

  components <- dots_to_xml_components(...)

  build_branch_node(
    "dataDscr",
    allowed_children = allowed_children,
    attribs = attribs,
    components = components
  )
}
