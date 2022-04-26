#' Codebook
#' 
#' The root node of a DDI 2.5 Codebook file. This file must contain stdyDscr. 
#' More information on this element, especially the allowed attributes, can 
#' be found in the references.
#' 
#' @param ... Child nodes or attributes.
#' 
#' @section Shared and complex child nodes:
#' * [ddi_dataDscr()]
#' * [ddi_docDscr()]
#' * [ddi_fileDscr()]
#' * [ddi_otherMat()] 
#' * [ddi_stdyDscr()]
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
#' All DDI codebooks must have a study description which contains information 
#' about the study overall. The Study Description consists of information about the data 
#' collection, study, or compilation that the DDI-compliant documentation file 
#' describes. This section includes information about how the study should be 
#' cited, who collected or compiled the data, who distributes the data, 
#' keywords about the content of the data, summary (abstract) of the content 
#' of the data, data collection methods and processing, etc. At least
#' one citation must be present, capturing the whole study. More information 
#' on this element, especially the allowed attributes, can be found in the references.
#' 
#' \emph{Parent node}
#'
#' `stdyDscr` is contained in `codeBook`.
#'
#' @param ... Child nodes or attributes. 
#' 
#' @section Shared and complex child nodes:
#' 
#' * [ddi_citation()]
#' * [ddi_dataAccs()] 
#' * [ddi_method()]
#' * [ddi_notes()] 
#' * [ddi_othrStdyMat()]
#' * [ddi_stdyInfo()] 
#' * [ddi_studyAuthorization()] 
#' * [ddi_studyDevelopment()]
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
#' Description of variables within the Codebook. More information on this 
#' element, especially the allowed attributes, can be found in the references.
#' 
#' \emph{Parent node}
#'
#' `dataDscr` is contained in `codeBook`.
#' 
#' @param ... Child nodes or attributes. 
#' 
#' @section Shared and complex child nodes:
#' 
#' * [ddi_nCube()]
#' * [ddi_nCubeGrp()] 
#' * [ddi_notes()] 
#' * [ddi_var()]
#' * [ddi_varGrp()] 
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
#' Information about the data file(s) that comprises a collection. This section 
#' can be repeated for collections with multiple files. More information on 
#' this element, especially the allowed attributes, can be found in the references.
#' 
#' \emph{Parent node}
#'
#' `fileDscr` is contained in `codeBook`.
#' 
#' @param ... Child nodes or attributes. 
#'
#' @section Shared and complex child nodes:
#' 
#' * [ddi_fileTxt()]
#' * [ddi_locMap()] 
#' * [ddi_notes()] 
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

#' otherMat and its children
#' 
#' This section allows for the inclusion of other materials that are related to 
#' the study as identified and labeled by the DTD/Schema users (encoders). The 
#' materials may be entered as PCDATA (ASCII text) directly into the document 
#' (through use of the "txt" element). This section may also serve as a 
#' "container" for other electronic materials such as setup files by providing 
#' a brief description of the study-related materials accompanied by the attributes 
#' "type" and "level" defining the material further. Other 
#' Study-Related Materials may include: questionnaires, coding notes, 
#' SPSS/SAS/Stata setup files (and others), user manuals, continuity guides, 
#' sample computer software programs, glossaries of terms, interviewer/project 
#' instructions, maps, database schema, data dictionaries, show cards, coding 
#' information, interview schedules, missing values information, frequency 
#' files, variable maps, etc. More information on this element, especially 
#' the allowed attributes, can be found in the references.
#' 
#' \emph{Parent nodes}
#'
#' `otherMat` is contained in the following elements: `codeBook` and `otherMat`.
#' 
#' @param ... Child nodes or attributes. 
#'
#' @section Shared and complex child nodes:
#' 
#' * [ddi_citation()]
#' * [ddi_labl()] 
#' * [ddi_notes()] 
#' * [ddi_otherMat()]
#' * [ddi_table()]
#' * [ddi_txt()]
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataDscr.html}{dataDscr documentation}
#' 
#' @export
ddi_otherMat <- function(...) {
  allowed_children <- c(
    "citation",
    "labl",
    "notes",
    "otherMat",
    "table",
    "txt"
  )
  
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                         "type", "level", "URI")
    attribs <- validate_attributes(attribs, allowed_attribs, "otherMat")
  }
  
  build_branch_node(
    "otherMat",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}
