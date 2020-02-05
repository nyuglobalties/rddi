ddi_var <- function(varname, database_id, ...) {
  stopifnot(is.character(varname) && is.character(database_id))
  stopifnot(length(varname) == 1 && length(database_id) == 1)

  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if (is.element("nature", names(attribs))) {
    known_nature <- c(
      "nominal", "ordinal", "interval",
      "ratio", "percent", "other"
    )

    if (!is.element(attribs$nature, known_nature)) {
      rddi_err(c(
        "'{attribs$nature}' is not an acceptable variable nature.\n",
        "These are the accepted natures: [{glue_collapse(known_nature, ', ')}]"
      ))
    }
  }

  attribs$name <- varname
  attribs$files <- database_id

  allowed_children <- c(
    "catgry",
    "catLevel",
    "labl",
    "qstn",
    "sumStat"
  )

  build_branch_node(
    "var",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

ddi_catLevel <- function(level_id, level_name) {
  stopifnot(is.character(level_id) && is.character(level_name))

  build_leaf_node(
    "catLevel",
    attribs = list(
      ID = level_id,
      levelnm = level_name
    )
  )
}

ddi_catgry <- function(...) {
  components <- dots_to_xml_components(...)

  allowed_children <- c(
    "catStat",
    "catValu",
    "labl"
  )

  build_branch_node(
    "catgry",
    allowed_children = allowed_children,
    attribs = components$attribs,
    content = components$content
  )
}

ddi_catgry <- function(..., missing = FALSE) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  attribs$missing <- if (missing) "Y" else "N"

  allowed_children <- c(
    "catStat",
    "catValu",
    "labl"
  )

  build_branch_node(
    "catgry",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

ddi_catValu <- function(value) {
  build_leaf_node(
    "catValu",
    content = value
  )
}
