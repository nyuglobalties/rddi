#' Study Authorization
#' 
#' Provides structured information on the agency that authorized the study, the date of authorization, and an authorization statement.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/studyAuthorization.html}
#' 
#' @export
ddi_studyAuthorization <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "date")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "studyAuthorization")
    check_attribs(attribs)
    check_elementVersionDate(attribs$date)
  }

  allowed_children <- c(
    "authorizingAgency",
    "authorizationStatement"
  )

  build_branch_node(
    "studyAuthorization",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )   
}

#' Authorizing Agency
#' 
#' Name of the agent or agency that authorized the study. The "affiliation" attribute indicates the institutional affiliation of the authorizing agent or agency. The 
#' "abbr" attribute holds the abbreviation of the authorizing agent's or agency's name.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/authorizingAgency.html}
#' 
#' @export
ddi_authorizingAgency <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "affiliation", "abbr")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "authorizingAgency")
    check_attribs(attribs)
  }

  build_leaf_node(
    "authorizingAgency",
    attribs = attribs,
    content = components$content
  )   
}

#' Authorization Statement
#' 
#' The text of the authorization. Use XHTML to capture significant structure in the document.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/authorizationStatement.html}
#' 
#' @export
ddi_authorizationStatement <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "authorizationStatement")
    check_attribs(attribs)
  }

  build_leaf_node(
    "authorizationStatement",
    attribs = attribs,
    content = components$content
  )   
}