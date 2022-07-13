# Internal function that creates the ddi_node objects. Deals directly
# with the underlying representation of the nodes. To model DDI elements,
# use build_branch_node() or build_leaf_node().
ddi_node <- function(tagname,
                     ...,
                     .root = FALSE) {
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
    node$content <- as.character(content) %if_empty_string% NULL
  }

  add_ddi_class(node, root = .root)
}

ddi_root <- function(tagname, ...) {
  ddi_node(tagname, ..., .root = TRUE)
}

is_ddi_node <- function(x) {
  inherits(x, "ddi_node")
}

#' @export
print.ddi_node <- function(x, ...) {
  xml_node <- as_xml(x)
  class(xml_node) <- setdiff(class(xml_node), "xml_document")
  print(xml_node, ...)

  invisible()
}

#' @export
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

#' Create DDI nodes
#'
#' All `ddi_<tag>()` functions either create branch nodes or leaf nodes.
#' Branches must only contain other nodes, whereas leaves may only have
#' a single string for its content. All nodes may be empty, however.
#' If no checks on function paramenters are necessary for leaf nodes,
#' use `simple_lead_node()`.
#'
#' @param tagname The name for the XML representation of this node
#' @param allowed_children A character vector of allowed children's tag names. Defaults to `NULL`, indicating any child is allowed.
#' @param required_children A character vector of required children's tag names. Default to `NULL`, indicating no children are strictly required.
#' @param root An indicator whether this node is the document root. Should really be reserved for `ddi_codeBook()`
#' @param content For a branch node, a list of child nodes. For a leaf node, a single string. Defaults to `NULL`, indicating no content.
#' @param attribs A named list representing the XML attributes for this node
#' @param components A shorthand object to `attribs` and `content`. If both `attribs` and `content` are `NULL`, `components` -- a list with "attribs" and "content" as fields -- will be used instead.
#' @return A `ddi_node` object
#'
#' @rdname build_node
#'
#' @noRd
build_branch_node <- function(tagname,
                              allowed_children = NULL,
                              required_children = NULL,
                              root = FALSE,
                              content = NULL,
                              attribs = NULL,
                              components = NULL) {
  if ((is.null(attribs) && is.null(content)) && !is.null(components)) {
    attribs <- components$attribs
    content <- components$content
  }

  if (isTRUE(root)) {
    branch_node <- ddi_root(tagname, !!!attribs)
  } else {
    branch_node <- ddi_node(tagname, !!!attribs)
  }


  if (!is.null(required_children) && !is.null(allowed_children)) {
    stopifnot(all(required_children %in% allowed_children))
  }

  check_allowed_content(content, branch_node, allowed_children)
  check_required_content(content, branch_node, required_children)
  
  content <- content[order(match(lapply(content, "[[", "tag"), allowed_children))]

  if (!is.null(content)) {
    for (child in content) {
      branch_node <- ddi_add_child(
        branch_node,
        child
      )
    }
  }

  branch_node
}

check_allowed_content <- function(content, parent, allowed, ...) {
  UseMethod("check_allowed_content", content)
}

#' @export
check_allowed_content.default <- function(content, parent, allowed, ...) {
  if (!is.null(allowed)) {
    if (length(content) < 1) {
      return(TRUE)
    }

    for (child in content) {
      if (!is_ddi_node(child)) {
        rddi_err("Non-DDI node content passed to XML wrapped context")
      }

      check_allowed_content(child, parent, allowed, ...)
    }
  }

  TRUE
}

#' @export
check_allowed_content.ddi_unwrapped <- function(content, parent, allowed, ...) { # nolint
  ddi_content <- map_lgl(content$content, is_ddi_node)

  check_allowed_content(content$content[ddi_content], parent, allowed, ...)
}

#' @export
check_allowed_content.ddi_node <- function(content, parent, allowed, ...) {
  if (!content$tag %in% allowed) {
    rddi_err(c(
      "'{content$tag}' is not an acceptable child element for {parent$tag}.\n",
      "These are the allowed children: [{glue_collapse(allowed, ', ')}]"
    ), .type = "rddi_unallowed_child_error")
  }

  TRUE
}

check_required_content <- function(content, parent, required, ...) {
  UseMethod("check_required_content", content)
}

#' @export
check_required_content.default <- function(content, parent, required, ...) {
  if (is.null(content)) {
    if (!is.null(required)) {
      rddi_err(c(
        "No children specified when some are required: ",
        "[{glue_collapse(required, ', ')}]"
      ))
    }
  } else {
    if (is_unwrapped(content[[1]])) {
      return(check_required_content(content[[1]], parent, required, ...))
    }

    if (!is.null(required)) {
      for (rc in required) {
        if (!any(map_lgl(content, function(x, req) x$tag == req, rc))) {
          rddi_err(
            "Required child '{rc}' not found for '{parent$tag}'.",
            .type = "rddi_missing_required_child_error"
          )
        }
      }
    }
  }

  TRUE
}

#' @export
check_required_content.ddi_unwrapped <- function(content, parent, required, ...) { # nolint
  ddi_content <- map_lgl(content$content, is_ddi_node)

  check_required_content(content$content[ddi_content], parent, required, ...)
}

#' @rdname build_node
#' @noRd
build_leaf_node <- function(tagname,
                            content = NULL,
                            attribs = NULL,
                            components = NULL) {
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

#' @rdname build_node
#' @noRd
simple_leaf_node <- function(tagname) {
  function(...) {
    components <- dots_to_xml_components(...)
    build_leaf_node(tagname, components = components)
  }
}