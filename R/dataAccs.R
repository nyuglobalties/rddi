#' dataAccs and its children
#'
#' This section describes data access conditions and terms of use for the data collection. In cases where access conditions differ across individual files 
#' or variables, multiple access conditions can be specified.  For more information see the references section.
#'
#' @param ... Child nodes or attributes. 
#'
#' @section Branch node children allowed:
#' * [ddi_setAvail()]
#' * [ddi_useStmt()]
#' 
#' @section General children allowed:
#' * [ddi_notes()]
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataAccs.html}{dataAccs documentation}
#' 
#' @export
ddi_dataAccs <- function(...) {
  allowed_children <- c(
    "notes",
    "setAvail", 
    "useStmt"
  )

  components <- dots_to_xml_components(...)
  attribs <- components$attribs    

  if (!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "dataAccs")
  }

  build_branch_node(
    "dataAccs",
    allowed_children = allowed_children,
    components = components,
    attribs = attribs
  )
}

#' setAvail and its children
#' 
#' Information on availability and storage of the data set collection. For more information on setAvail and its children and attributes see the references.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section General Children allowed:
#' * [ddi_notes()]
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/setAvail.html}{setAvail documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/accsPlac.html}{accsPlac documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/avlStatus.html}{avlStatus documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/collSize.html}{collSize documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/complete.html}{complete documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/fileQnty.html}{fileQnty documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/origArch.html}{origArch documentation}
#' 
#' @export
ddi_setAvail <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "media", "callno", "label", "type")
    attribs <- validate_attributes(attribs, allowed_attribs, "setAvail")
  }

  allowed_children <- c(
    "accsPlac",
    "origArch", 
    "avlStatus",
    "collSize",
    "complete",
    "fileQnty",
    "notes"
  )  

  build_branch_node(
    "setAvail",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_setAvail
#' @export
ddi_accsPlac <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "URI")
    attribs <- validate_attributes(attribs, allowed_attribs, "accsPlac")
  }

  build_leaf_node(
    "accsPlac",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_setAvail
#' @export
ddi_avlStatus <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "avlStatus")
  }

  build_leaf_node(
    "avlStatus",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_setAvail
#' @export
ddi_collSize <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "collSize")
  }

  build_leaf_node(
    "collSize",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_setAvail
#' @export
ddi_complete <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "complete")
  }

  build_leaf_node(
    "complete",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_setAvail
#' @export
ddi_fileQnty <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "fileQnty")
  }

  build_leaf_node(
    "fileQnty",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_setAvail
#' @export
ddi_origArch <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "origArch")
  }

  build_leaf_node(
    "origArch",
    attribs = attribs,
    content = components$content
  )
}