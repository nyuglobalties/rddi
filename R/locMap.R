#' locMap and its child nodes
#'
#' This element maps individual data entries to one or more physical storage 
#' locations. It is used to describe the physical location of aggregate/tabular 
#' data in cases where the nCube model is employed. More information 
#' on the allowed attributes for fileTxt and its child nodes can be found 
#' below and in the references. 
#'  
#' @section Branch node children allowed:
#' * [ddi_dataItem()]
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/locMap.html}{locMap documentation}
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema.
#'
#' @export
ddi_locMap <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn") 
    attribs <- validate_attributes(attribs, allowed_attribs, "locMap")
  }

  allowed_children <- c(
    "dataItem"
  )

  build_branch_node(
    "locMap",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' dataItem and its child nodes
#'
#' Identifies a physical storage location for an individual data entry, serving 
#' as a link between the physical location and the logical content description 
#' of each data item. . It is used to describe the physical location of aggregate/tabular 
#' data in cases where the nCube model is employed. More information 
#' on the allowed attributes for dataItem and its child nodes can be found 
#' below and in the references. 
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataItem.html}{dataItem documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/CubeCoord.html}{CubeCoord documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/physLoc.html}{physLoc documentation}
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema.
#'
#' @export
ddi_dataItem <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "varRef", "nCubeRef") 
    attribs <- validate_attributes(attribs, allowed_attribs, "dataItem")
  }

  allowed_children <- c(
    "CubeCoord",
    "physLoc"
  )

  build_branch_node(
    "dataItem",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_dataItem
#' @export
ddi_CubeCoord <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleURN", "ddiCodebookURN",
                        "coordNo", "coordVal", "coordValRef")
    attribs <- validate_attributes(attribs, allowed_attribs, "CubeCoord")
  }

  build_leaf_node(
    "CubeCoord",
    attribs = attribs
  )
}

#' @rdname ddi_dataItem
#' @export
ddi_physLoc <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleURN", "ddiCodebookURN",
                        "type", "recRef", "startPos", "width", "endPos")
    attribs <- validate_attributes(attribs, allowed_attribs, "physLoc")
  }

  build_leaf_node(
    "physLoc",
    attribs = attribs
  )
}