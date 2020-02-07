ddi_add_child <- function(.node, .child, ...) {
  UseMethod("ddi_add_child")
}

ddi_add_child.ddi_node <- function(
  .node, 
  .child,
  .allowed_children = NULL,
  ...
) {
  add_child_node(.node, .child, .allowed_children, ...)
}

ddi_add_child.ddi_root <- function(
  .node, 
  .child,
  .allowed_children = NULL,
  ...
) {
  add_child_node(.node, .child, .allowed_children, ...)
}

add_child_node <- function(.node, .child, .allowed_children, ...) {
  stopifnot(is_ddi_node(.node) && is_ddi_node(.child))

  if (!is.null(.allowed_children)) {
    stopifnot(is.character(.allowed_children))

    if (!.child$tag %in% .allowed_children) {
      rddi_err(c(
        "'{.child$tag}' is not an acceptable child element for {.node$tag}.\n",
        "These are the allowed children: [{glue_collapse(.allowed_children, ', ')}]"
      ))
    }
  }

  if (is.null(.node$content)) {
    .node$content <- list(.child)
  } else {
    .node$content <- c(.node$content, .child)
  }

  .node
} 
