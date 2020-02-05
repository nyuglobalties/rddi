ddi_add_child <- function(.node, .child, ...) {
  UseMethod("ddi_add_child")
}

ddi_add_child.ddi_node <- function(
  .node, 
  .child,
  .allowed_children = NULL,
  ...
) {
  new_node <- add_child_node(.node, .child, .allowed_children, ...)

  add_ddi_class(new_node)
}

ddi_add_child.ddi_root <- function(
  .node, 
  .child,
  .allowed_children = NULL,
  ...
) {
  new_node <- add_child_node(.node, .child, .allowed_children, ...)

  add_ddi_class(new_node, root = TRUE)
}

add_child_node <- function(.node, .child, .allowed_children, ...) {
  stopifnot(is_ddi_node(.node) && is_ddi_node(.child))

  if (!is.null(.allowed_children)) {
    stopifnot(is.character(.allowed_children))

    if (!xml_name(.child) %in% .allowed_children) {
      rddi_err(c(
        "'{xml_name(.child)}' not an acceptable child element for {xml_name(.node)}.\n",
        "These are the allowed children: [{glue_collapse(.allowed_children, ', ')}]"
      ))
    }
  }

  # xml2 returns the child, not the parent
  xml_parent(xml_add_child(.node, .child, ...))
} 
