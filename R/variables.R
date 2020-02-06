#' DDI variable specification
#'
#' Variables are specified with the <var> tag, which has a litany of 
#' attributes. The basic necessities of a variable, however, are given by its name, and type. 
#' If the variable is categorical, then the variable will have as many 
#' <catgry> (`ddi_catgry()`) entries as category levels. Missingness codes are specified in `ddi_catgry()`.
#'
#' @param varname The variable name
#' @param database_id An ID to the database origin; useful if multiple datasets are described in a codebook
#' @param discrete If a variable has discrete or continuous values. Default to `TRUE`
#' @param ... Any other parameters from the DDI Codebook 2.5 schema
#' @param level_name A category level name for hierarchical categorical variables (e.g. ordered categories)
#' @param missing If a category represents a missing response
#' @param id_object All `ddi_` functions that have dots can specify an object's ID with id_object
#'
#' @export
ddi_var <- function(varname, database_id = NULL, discrete = TRUE, ...) {
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
  attribs$intrvl <- if (isTRUE(discrete)) "discrete" else "contin"

  if (!is.null(database_id)) {
    attribs$files <- database_id
  }

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

#' @rdname ddi_var
#' @export
ddi_catLevel <- function(level_name, ...) {
  stopifnot(is.character(level_name))

  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if (!is.element("ID", names(attribs))) {
    rddi_err("Category levels must have an ID. Use the `id_object` parameter to set the ID.")
  }

  build_leaf_node(
    "catLevel",
    attribs = list(
      ID = attribs$ID,
      levelnm = level_name
    )
  )
}

#' @rdname ddi_var
#' @export
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

#' @rdname ddi_var
#' @export
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

#' @rdname ddi_var
#' @export
ddi_catValu <- function(value) {
  build_leaf_node(
    "catValu",
    content = value
  )
}
