ddi_node <- function(
  tagname, 
  ...,
  .root = FALSE
) {
  stopifnot(is.character(tagname) && length(tagname) == 1)

  node <- list(
    tag = tagname,
    attribs = NULL,
    content = NULL
  )

  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  content <- components$content

  if (length(content) > 1) {
    rddi_err(c(
      "Unnamed arguments to DDI node creation are treated as text content. ",
      "Multiple arguments are not allowed."
    ))
  }

  if (inherits(content, "ddi_node")) {
    rddi_err(c(
      "Use ddi_add_child instead of assigning child nodes at node creation."
    ))
  }

  if (!is.null(attribs)) {
    stopifnot(is.list(attribs) && !is.null(names(attribs)))

    node$attribs <- attribs
  }

  if (!is.null(content)) {
    node$content <- as.character(content)
  }

  add_ddi_class(node, root = .root)
}

ddi_root <- function(tagname, ...) {
  ddi_node(tagname, ..., .root = TRUE)
}

is_ddi_node <- function(x) {
  inherits(x, "ddi_node")
}

print.ddi_node <- function(x, ...) {
  xml_node <- as_xml(x)
  class(xml_node) <- setdiff(class(xml_node), "xml_document")
  print(xml_node, ...)

  invisible()
}

print.ddi_root <- function(x, ...) {
  xml_node <- as_xml(x)
  print(xml_node, ...)

  invisible()
}

add_ddi_class <- function(node, root = FALSE) {
  if (isTRUE(root)) {
    new_classes <- c("ddi_root", "ddi_node")
  } else {
    new_classes <- "ddi_node"
  }

  structure(
    node,
    class = new_classes
  )
}

as_xml <- function(x, ...)  {
  UseMethod("as_xml")
}

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

build_branch_node <- function(
  tagname,
  parent = NULL,
  allowed_children = NULL,
  required_children = NULL,
  root = FALSE,
  content = NULL,
  attribs = NULL, 
  components = NULL
) {
  if ((is.null(attribs) && is.null(content)) && !is.null(components)) {
    attribs <- components$attribs
    content <- components$content
  }

  if (isTRUE(root)) {
    stem_node <- ddi_root(tagname, !!!attribs)
  } else {
    stem_node <- ddi_node(tagname, !!!attribs)
  }

  if (!is.null(content)) {
    if (!all(map_lgl(content, is_ddi_node))) {
      rddi_err("Unnamed arguments to `ddi_{tagname}()` must all be DDI nodes.")
    }

    if (!is.null(required_children)) {
      if (!is.null(allowed_children)) {
        stopifnot(all(required_children %in% allowed_children))
      }

      for (rc in required_children) {
        if (!any(map_lgl(content, function(x, req) x$tag == req, rc))) {
          rddi_err("Required child '{rc}' not found for '{tagname}'.")
        }
      }
    }

    for (child in content) {
      stem_node <- ddi_add_child(
        stem_node,
        child,
        .allowed_children = allowed_children
      )
    }
  } else {
    if (!is.null(required_children)) {
      rddi_err(c(
        "No children specified when some are required: ",
        "[{glue_collapse(required_children, ', ')}]"
      ))
    }
  }

  stem_node
}

build_leaf_node <- function(
  tagname,
  parent = NULL,
  content = NULL,
  attribs = NULL,
  components = NULL
) {
  if ((is.null(attribs) && is.null(content)) && !is.null(components)) {
    attribs <- components$attribs
    content <- components$content
  }

  leaf_node <- ddi_node(tagname, !!!attribs)

  if (!is.null(content)) {
    if (any(map_lgl(content, is_ddi_node))) {
      rddi_err("Leaf nodes cannot have ddi_node content.")
    }

    leaf_node$content <- as.character(content)
  }

  leaf_node
}

simple_leaf_node <- function(tagname) {
  function(...) {
    components <- dots_to_xml_components(...)
    build_leaf_node(tagname, components = components)
  }
}
