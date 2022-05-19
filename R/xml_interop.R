#' Get XML representation of ddi_node objects
#'
#' @param x A `ddi_node` object (or subclass).
#' @param ... Arguments to pass to methods.
#' @return An `xml_document` or `xml_node` object whether the object is a root node or not, respectively.
#' 
#' @examples 
#' cb <- ddi_codeBook(ddi_stdyDscr(ddi_citation(ddi_titlStmt(ddi_titl("Sample")))))
#' as_xml(cb)
#'
#' @export
as_xml <- function(x, ...) {
  UseMethod("as_xml")
}

#' @export
as_xml.ddi_node <- function(x, parent = NULL, rooted = is.null(parent), ...) {
  stopifnot(is.null(parent) || inherits(parent, "xml_node"))

  if (!is.null(parent)) {
    xml_node <- xml_add_child(parent, x$tag)
  } else {
    xml_node <- xml_new_root(x$tag)

    if (!isTRUE(rooted)) {
      xml_node <- xml_as_node(xml_node)
    }
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

#' @export
as_xml.ddi_unwrapped <- function(x, parent = NULL, rooted = FALSE, ...) {
  if (is.null(parent)) {
    rddi_err(c(
      "An unwrapped ddi node can only be evaluated ",
      "in the context of a parent XML node"
    ), .type = "ddi_err_unwrap_noparent")
  }

  nodes <- map_lgl(x$content, is_ddi_node)

  chr_content <- map2_chr(
    x$content, nodes,
    function(cont, is_node) {
      if (is_node) {
        as_xml_string(cont, rooted = rooted)
      } else {
        as.character(cont)
      }
    }
  )
  chr_content <- paste0(chr_content, collapse = "")

  xml_text(parent) <- chr_content
  parent
}

#' Shortcut to text representation of DDI XML
#'
#' Functionally equivalent to `as.character(as_xml(ddi_node_obj))`.
#'
#' @param x A ddi_node object.
#' @param ... Arguments forwarded to `as_xml()`.
#' @return A string containing the text representation of XML.
#' 
#' @examples 
#' cb <- ddi_codeBook(ddi_stdyDscr(ddi_citation(ddi_titlStmt(ddi_titl("Sample")))))
#' as_xml_string(cb)
#' 
#' @export
as_xml_string <- function(x, ...) {
  stopifnot(is_ddi_node(x))

  as.character(as_xml(x, ...))
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
#' @param x An `xml_node` object.
#' @param ... Arguments to pass to methods.
#' @return The DDI equivalent of the XML tree.
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

xml_as_node <- function(xml_node) {
  class(xml_node) <- setdiff(class(xml_node), "xml_document")
  xml_node
}