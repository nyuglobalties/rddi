ddi_node <- function(
  tagname, 
  ..., 
  .root = FALSE,
  .with_document = FALSE
) {
  stopifnot(is.character(tagname))

  node <- xml_new_root(tagname)

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
    xml_attrs(node) <- attribs
  }

  if (!is.null(content)) {
    xml_text(node) <- content
  }

  add_ddi_class(node, root = .root)
}

ddi_root <- function(tagname, ...) {
  ddi_node(tagname, ..., .root = TRUE)
}

is_ddi_node <- function(x) {
  inherits(x, "ddi_node")
}

add_ddi_class <- function(node, root = FALSE) {
  stopifnot(inherits(node, "xml_node"))

  old_classes <- setdiff(class(node), "xml_document")

  if (isTRUE(root)) {
    new_classes <- c("ddi_root", "ddi_node", old_classes)
  } else {
    new_classes <- c("ddi_node", old_classes)
  }

  structure(
    node,
    class = new_classes
  )
}

build_branch_node <- function(
  tagname,
  allowed_children = NULL,
  root = FALSE,
  content = NULL,
  attribs = NULL
) {
  if (isTRUE(root)) {
    stem_node <- ddi_root(tagname, !!!attribs)
  } else {
    stem_node <- ddi_node(tagname, !!!attribs)
  }

  if (!is.null(content)) {
    if (!all(map_lgl(content, is_ddi_node))) {
      rddi_err("Unnamed arguments to `ddi_{tagname}()` must all be DDI nodes.")
    }

    for (child in content) {
      stem_node <- ddi_add_child(
        stem_node,
        child,
        .allowed_children = allowed_children
      )
    }
  }

  add_ddi_class(stem_node, root = root)
}

build_leaf_node <- function(
  tagname,
  content = NULL,
  attribs = NULL
) {
  leaf_node <- ddi_node(tagname, !!!attribs)

  if (!is.null(content)) {
    if (any(map_lgl(content, is_ddi_node))) {
      rddi_err("Leaf nodes cannot have ddi_node content.")
    }

    xml_text(leaf_node) <- as.character(content)
  }

  add_ddi_class(leaf_node)
}
