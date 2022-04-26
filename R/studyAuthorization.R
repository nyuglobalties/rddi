#' studyAuthorization and its child nodes
#' 
#' Study Authorization provides structured information on the agency that 
#' authorized the study, the date of authorization, and an authorization 
#' statement. More information on these elements, especially their allowed 
#' attributes, can be found in the references. 
#' 
#' \emph{Parent nodes}
#' 
#' `studyAuthorization` is contained in `stdyDscr`.
#' 
#' \emph{studyAuthorization specific child nodes}
#' 
#' * `ddi_authorizationStatement()` is the text of the authorization.
#' 
#' * `ddi_authorizingAgency()` is the name of the agent or agency that 
#' authorized the study.
#' 
#' @param ... Child nodes or attributes. 
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/studyAuthorization.html}{studyAuthorization docuemntation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/authorizationStatement.html}{authorizationStatement documentation}
#' @references \href{{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/authorizingAgency.html}}{authorizingAgency documentation}
#' 
#' @export
ddi_studyAuthorization <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "date")
    attribs <- validate_attributes(attribs, allowed_attribs, "studyAuthorization")
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

#' @rdname ddi_studyAuthorization
#' @export
ddi_authorizationStatement <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "authorizationStatement")
  }
  
  build_leaf_node(
    "authorizationStatement",
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_studyAuthorization
#' @export
ddi_authorizingAgency <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "affiliation", "abbr")
    attribs <- validate_attributes(attribs, allowed_attribs, "authorizingAgency")
  }

  build_leaf_node(
    "authorizingAgency",
    attribs = attribs,
    content = components$content
  )   
}