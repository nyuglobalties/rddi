#' Study Development
#' 
#' Describe the process of study development as a series of development activities. These activities can be typed using a controlled vocabulary. Describe the activity, 
#' listing participants with their role and affiliation, resources used (sources of information), and the outcome of the development activity.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/studyDevelopment.html}
#' 
#' @export
ddi_studyDevelopment <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "studyAuthorization")
    check_attribs(attribs)
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

#' Development Activity
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/developmentActivity.html}
#' 
#' @export
ddi_developmentActivity <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "type")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "developmentActivity")
    check_attribs(attribs)
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



#' Description
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/description.html}
#' 
#' @export
ddi_description <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "description")
    check_attribs(attribs)
  }

  build_leaf_node(
    "description",
    attribs = attribs,
    content = components$content
  )   
}

#' Participant
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/participant.html}
#' 
#' @export
ddi_participant <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "affiliation", "abbr", "role")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "participant")
    check_attribs(attribs)
  }

  build_leaf_node(
    "participant",
    attribs = attribs,
    content = components$content
  )   
}

#' Outcome
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/outcome.html}
#' 
#' @export
ddi_outcome <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "outcome")
    check_attribs(attribs)
  }

  build_leaf_node(
    "outcome",
    attribs = attribs,
    content = components$content
  )   
}

#' Resource
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/resource.html}
#' 
#' @export
ddi_resource <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "resource")
    check_attribs(attribs)
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

#' dataSrc
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataSrc_1.html}
#' 
#' @export
ddi_dataSrc <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "dataSrc")
    check_attribs(attribs)
  }

  build_leaf_node(
    "dataSrc",
    attribs = attribs,
    content = components$content
  )
}

#' Characteristics of Source Noted
#' 
#' Assessment of characteristics and quality of source material. May not be relevant to survey data. This element may be repeated to support multiple 
#' language expressions of the content.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataSrc_1.html}
#' 
#' @export
ddi_srcChar <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "srcChar")
    check_attribs(attribs)
  }

  build_leaf_node(
    "srcChar",
    attribs = attribs,
    content = components$content
  )
}

#' Documentation and Access to Sources
#' 
#' Level of documentation of the original sources. May not be relevant to survey data. This element may be repeated to support multiple language expressions of the content.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/srcDocu.html}
#' 
#' @export
ddi_srcDocu <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "srcDocu")
    check_attribs(attribs)
  }

  build_leaf_node(
    "srcDocu",
    attribs = attribs,
    content = components$content
  )
}
