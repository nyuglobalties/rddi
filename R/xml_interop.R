#' Get XML representation of ddi_node objects
#'
#' @param x A `ddi_node` object (or subclass)
#' @param ... Arguments to pass to methods
#' @return An `xml_document` or `xml_node` object whether the object is a root node or not, respectively
#'
#' @export
as_xml <- function(x, ...)  {
  UseMethod("as_xml")
}

#' @export
as_xml.ddi_node <- function(x, parent = NULL, ...) {
  stopifnot(is.null(parent) || inherits(parent, "xml_node"))

  if (!is.null(parent)) {
    xml_node <- xml_add_child(parent, x$tag)
  } else {
    xml_node <- xml_new_root(x$tag)
  }

  xml_attrs(xml_node) <- x$attribs

  if (!is.null(x$content)) {
    if (is.character(x$content)) {
      xml_text(xml_node) <- x$content 
    } else {
      for (child in x$content) {
        as_xml(child, xml_node, ...)
      }
    }
  }

  xml_node
}

#' Shortcut to text representation of DDI XML
#'
#' Functionally equivalent to `as.character(as_xml(ddi_node_obj))`
#'
#' @param x A ddi_node object
#' @return A string containing the text representation of XML
#' @export
as_xml_string <- function(x) {
  stopifnot(is_ddi_node(x))

  as.character(as_xml(x))
}

#' @export
as_xml.ddi_root <- function(x, ...) {
  xml_node <- xml_new_root(x$tag)
  xml_attrs(xml_node) <- x$attribs

  if (!is.null(x$content)) {
    for (child in x$content) {
      as_xml(child, xml_node, ...)
    }
  }
  
  xml_node
}

#' Convert XML trees to DDI objects
#'
#' @param x An `xml_node` object
#' @param ... Arguments to pass to methods
#' @return The DDI equivalent of the XML tree
#' @export
as_ddi <- function(x, ...) {
  UseMethod("as_ddi")
}

#' @export
as_ddi.xml_node <- function(x, root = FALSE, ...) {
  tagname <- xml_name(x)

  if (length(xml_attrs(x)) != 0) {
    attribs <- as.list(xml_attrs(x))
  } else {
    attribs <- list()
  }

  if (length(xml_children(x)) == 0) {
    content <- xml_text(x) %if_empty_string% list()
  } else {
    content <- list()
  }

  node <- do.call(
    ddi_node, 
    c(
      list(tagname = tagname, .root = root), 
      attribs, 
      content
    )
  )

  if (length(xml_children(x)) > 0) {
    children <- map(xml_children(x), as_ddi)

    for (child in children) {
      node <- ddi_add_child(node, child)
    }
  }

  node
}
