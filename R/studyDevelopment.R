#' studyDevelopment and developmentActivity
#' 
#' studyDevelopment is the parent node of developmentActivity. It describes the process of study development as a series of development activities. 
#' These activities can be typed using a controlled vocabulary that describes the activity, #' listing participants with their role and affiliation, 
#' resources used (sources of information), and the outcome of the development activity. For more information on both elements and the child nodes of 
#' developmentActivity see the resources.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation:
#'
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/studyDevelopment.html}
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/developmentActivity.html}
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/description.html}
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/participant.html}
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/outcome.html}
#' 
#' @export
ddi_studyDevelopment <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "studyDevelopment")
  }

  allowed_children <- c(
    "developmentActivity"
  )

  build_branch_node(
    "studyAuthorization",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_studyDevelopment 
#' @export
ddi_developmentActivity <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "type")
    attribs <- validate_attributes(attribs, allowed_attribs, "developmentActivity")
  }

  allowed_children <- c(
    "description",
    "participant",
    "resource", 
    "outcome"
  )

  build_branch_node(
    "developmentActivity",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )    
}

#' @rdname ddi_studyDevelopment
#' @export
ddi_description <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "description")
  }

  build_leaf_node(
    "description",
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_studyDevelopment
#' @export
ddi_participant <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "affiliation", "abbr", "role")
    attribs <- validate_attributes(attribs, allowed_attribs, "participant")
  }

  build_leaf_node(
    "participant",
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_studyDevelopment
#' @export
ddi_outcome <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "outcome")
  }

  build_leaf_node(
    "outcome",
    attribs = attribs,
    content = components$content
  )   
}

#' resource
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section General children allowed:
#' * [ddi_dataSrc()]
#' * [ddi_srcOrig()]
#' * [ddi_srcDocu()]
#' * [ddi_srcOrig()]
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/resource.html}{resource documentation}
#' 
#' @export
ddi_resource <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "resource")
  }

  allowed_children <- c(
    "dataSrc",
    "srcChar",
    "srcDocu", 
    "srcDocu"
  )

  build_branch_node(
    "resource",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}