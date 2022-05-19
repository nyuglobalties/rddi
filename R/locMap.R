#' locMap and its child nodes
#'
#' This element maps individual data entries to one or more physical storage 
#' locations. It is used to describe the physical location of aggregate/tabular 
#' data in cases where the nCube model is employed. More information on these 
#' elements, especially their allowed attributes, can be found in the references. 
#'  
#' \emph{Parent nodes}
#' 
#' `locMap` is contained in `fileDscr`.
#'
#' @section Shared and complex child nodes:
#' * [ddi_dataItem()]
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/locMap.html}{locMap documentation}
#' 
#' @param ... Child nodes or attributes. 
#' 
#' @return A ddi_node object.
#' 
#' @examples 
#' ddi_locMap()
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
#' of each data item. . It is used to describe the physical location of 
#' aggregate/tabular data in cases where the nCube model is employed. More 
#' information on these elements, especially their allowed attributes, can be 
#' found in the references. 
#' 
#' \emph{Parent nodes}
#' 
#' `dataItem` is contained in `locMap`.
#' 
#' \emph{dataItem specific child nodes} 
#' 
#' * `ddi_CubeCoord()` is an empty element containing only the attributes 
#' listed below. It is used to identify the coordinates of the data item within 
#' a logical nCube describing aggregate data. CubeCoord is repeated for each 
#' dimension of the nCube giving the coordinate number ("coordNo") and coordinate 
#' value ("coordVal"). Coordinate value reference ("cordValRef") is an ID 
#' reference to the variable that carries the coordinate value. The attributes 
#' provide a complete coordinate location of a cell within the nCube.
#' 
#' * `ddi_physLoc()` is an empty element containing only the attributes listed 
#' below. Attributes include "type" (type of file structure: rectangular, 
#' hierarchical, two-dimensional, relational), "recRef" (IDREF link to the 
#' appropriate file or recGrp element within a file), "startPos" (starting 
#' position of variable or data item), "endPos" (ending position of variable or 
#' data item), "width" (number of columns the variable/data item occupies), 
#' "RecSegNo" (the record segment number, deck or card number the variable or 
#' data item is located on), and "fileid" (an IDREF link to the fileDscr 
#' element for the file that includes this physical location).
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataItem.html}{dataItem documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/CubeCoord.html}{CubeCoord documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/physLoc.html}{physLoc documentation}
#' 
#' @param ... Child nodes or attributes. 
#' 
#' @return A ddi_node object.
#' 
#' @examples 
#' ddi_dataItem()
#' 
#' # Functions that need to be wrapped in ddi_dataItem()
#' 
#' ddi_CubeCoord(coordNo = "1", coordVal = "3")
#' 
#' ddi_physLoc(type = "rectangular", 
#'             recRef = "R1",
#'             startPos = "55",
#'             endPos = "57",
#'             width = "3")
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