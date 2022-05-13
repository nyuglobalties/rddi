#' table and its child nodes
#' 
#' Used to create a table in DDI 2.5. More information on this element, 
#' especially the allowed attributes, can be found in the references.
#' 
#' \emph{Parent nodes}
#'
#' `table` is contained in the following elements: `key`; `notes`; `otherMat`; 
#' and `txt`.
#' 
#' @param ... Child nodes or attributes. 
#'
#' @section Shared and complex child nodes:
#' * [ddi_tgroup()]
#' * [ddi_titl()] 
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/table.html}{table documentation}
#'  
#' @export
ddi_table <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                         "frame", "colsep", "rowsep", "pgwide") 
    attribs <- validate_attributes(attribs, allowed_attribs, "table")
  }
  
  allowed_children <- c(
    "titl",
    "tgroup"
  )
  
  build_branch_node(
    "table",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' tgroup and its child nodes
#' 
#' This is the table group. More information on this element, especially the 
#' allowed attributes, can be found in the references.
#' 
#' \emph{Parent nodes}
#'
#' `tgroup` is contained in `table`.
#' 
#' \emph{tgroup specific child node}
#' 
#' * `ddi_colspec()` is the column specification for each column. It is an 
#' empty element.
#' 
#' @param ... Child nodes or attributes. 
#'
#' @section Shared and complex child nodes:
#' * [ddi_tbody()] 
#' * [ddi_thead()]
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/tgroup.html}{tgroup documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/colspec.html}{colspec documentation}
#'  
#' @export
ddi_tgroup <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                         "cols", "colsep", "rowsep", "align") 
    attribs <- validate_attributes(attribs, allowed_attribs, "tgroup")
  }
  
  allowed_children <- c(
    "colspec",
    "tbody",
    "thead"
  )
  
  build_branch_node(
    "tgroup",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_tgroup
#' @export
ddi_colspec <-  function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                         "colnum", "colname", "colwidth", "colsep", "rowsep", "align", "char", "charoff")
    attribs <- validate_attributes(attribs, allowed_attribs, "colspec")
  }
  
  build_leaf_node(
    "colspec",
    attribs = attribs
  )
}

#' tbody and its child nodes
#' 
#' This is the body of the table. More information on this element, especially 
#' the allowed attributes, can be found in the references.
#' 
#' \emph{Parent nodes}
#'
#' `tbody` is contained in `tgroup`.
#' 
#' @param ... Child nodes or attributes. 
#'
#' @section Shared and complex child nodes:
#' * [ddi_row()] 
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/tbody.html}{tbody documentation}
#'  
#' @export
ddi_tbody <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                         "valign") 
    attribs <- validate_attributes(attribs, allowed_attribs, "tbody")
  }
  
  allowed_children <- c(
    "row"
  )
  
  build_branch_node(
    "tbody",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' thead and its child nodes
#' 
#' This is the table header. More information on this element, especially the 
#' allowed attributes, can be found in the references.
#' 
#' \emph{Parent nodes}
#'
#' `thead` is contained in `tgroup`.
#' 
#' @param ... Child nodes or attributes. 
#'
#' @section Shared and complex child nodes:
#' * [ddi_row()] 
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/thead.html}{thead documentation}
#'  
#' @export
ddi_thead <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                         "valign") 
    attribs <- validate_attributes(attribs, allowed_attribs, "thead")
  }
  
  allowed_children <- c(
    "row"
  )
  
  build_branch_node(
    "thead",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' row and its child nodes
#' 
#' Each row represents a table row. More information on this element, 
#' especially the allowed attributes, can be found in the references.
#' 
#' \emph{Parent nodes}
#'
#' `row` can be found in `tbody` and `thead`.
#' 
#' \emph{Child node}
#' 
#' `entry` is each table entry in the row.
#' 
#' @param ... Child nodes or attributes. 
#'
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/row.html}{row documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/entry.html}{entry documentation}
#'  
#' @export
ddi_row <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                         "rowsep", "valign") 
    attribs <- validate_attributes(attribs, allowed_attribs, "row")
  }
  
  allowed_children <- c(
    "entry"
  )
  
  build_branch_node(
    "row",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_row
#' @export
ddi_entry <-  function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                         "colname", "namest", "nameend", "morerows", "colsep", "rowsep",
                         "align", "char", "charoff", "valign")
    attribs <- validate_attributes(attribs, allowed_attribs, "colspec")
  }
  
  build_leaf_node(
    "entry",
    attribs = attribs,
    content = components$content
  )
}


