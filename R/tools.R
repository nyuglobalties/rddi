#' Create object IDs
#'
#' In the DDI Codebook 2.5 specification, object IDs are not necessary, 
#' but they are recommended to create linkages. For example, variables being
#' specific to a certain database file should include an ID reference to
#' that file.
#'
#' To create these IDs, this function employs a generator that accepts the
#' `object` and dotted parameters to create a character vector of IDs.
#' By default, generated IDs adhere to the DDI 3 URN standard.
#'
#' @param object A character vector containing (presumably unique) object names
#' @param generator A function that takes `object` and `...` and returns a character vector of IDs. Setting the "rddi.id_generator" option will override this functionality.
#' @param sep Any spaces in each element of `object` will be replaced with `sep`. Note: strings of whitespace will be replaced by a single `sep`, not multiple.
#' @param ... Arguments passed onto `generator`
create_object_id <- function(object, ..., generator = getOption("rddi.id_generator"), sep = ".") {
  stopifnot(is.character(object))

  object <- gsub("\\s+", sep, object)
  generator(object, ...)
}

dots_to_xml_components <- function(...) {
  dots <- dots_partition(...)
  attribs <- dots$named %if_empty% NULL
  content <- dots$unnamed %if_empty% NULL

  # ID parameters, for specifying how object IDs are constructed,
  # should be built into the universal "ID" attribute
  id_attrib_pattern <- "^\\id_"
  is_id_attrib <- grepl(id_attrib_pattern, names(attribs))

  if (any(is_id_attrib)) {
    id_params <- attribs[is_id_attrib]
    attribs <- attribs[!is_id_attrib]
    names(id_params) <- gsub(id_attrib_pattern, "", names(id_params))

    if (!is.element("object", names(id_params))) {
      rddi_err("The `id_object` parameter must be defined if you want to set a DDI ID.")
    }

    attribs[["ID"]] <- do.call(create_object_id, id_params)
  }

  list(
    attribs = attribs,
    content = content
  )
}

validate_codebook <- function(codebook) {
  stopifnot(is_ddi_node(codebook))

  if (xml_name(codebook) != "codeBook") {
    rddi_err("Codebook validation is only applicable from the codebook XML node")
  }

  codebook <- xml_new_root(codebook)
  schema <- read_xml(system.file("codebook.xsd", package = "rddi"))

  xml_validate(codebook, schema)
}
