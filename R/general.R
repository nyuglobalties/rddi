ddi_labl <- function(...) {
  components <- dots_to_xml_components(...)

  build_leaf_node(
    "labl",
    attribs = components$attribs,
    content = components$content
  )
}