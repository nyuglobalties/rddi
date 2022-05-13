#' studyDevelopment and its child nodes
#' 
#' Describe the process of study development as a series of development 
#' activities. These activities can be typed using a controlled vocabulary.  
#' More information on these elements, especially their allowed 
#' attributes, can be found in the references. 
#' 
#' \emph{Parent nodes}
#' 
#' `studyDevelopment` is contained in `stdyDscr`.
#' 
#' @section Shared and complex child nodes:
#' * [ddi_developmentActivity()] 
#' 
#' @param ... Child nodes or attributes. 
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/studyDevelopment.html}{studyDevelopment documentation}
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

#' developmentActivity and its child nodes
#' 
#' Describe the activity, listing participants with their role and affiliation, 
#' resources used (sources of information), and the outcome of the development 
#' activity.
#' 
#' \emph{Parent nodes}
#' 
#' `developmentActivity` is contained in `studyDevelopment`.
#' 
#' \emph{developmentActivity specific child nodes}
#' 
#' * `ddi_description()` describes the development activity.
#' 
#' * `ddi_outcome()` describes the outcome of the development activity.
#' 
#' * `ddi_participant()` lists teh participants conducting or designing the 
#' development activity.
#'
#' @section Shared and complex child nodes:
#' * [ddi_resource()] 
#' 
#' @param ... Child nodes or attributes. 
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/developmentActivity.html}{developmentActivity documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/description.html}{description documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/outcome.html}{outcome documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/participant.html}{participant documentation}
#' 
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

#' @rdname ddi_developmentActivity
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

#' @rdname ddi_developmentActivity
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

#' @rdname ddi_developmentActivity
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

#' resource and its child nodes
#' 
#' Resources used in the development of the activity. More information on these 
#' elements, especially their allowed attributes, can be found in the references. 
#' 
#' \emph{Parent nodes}
#' 
#' `resource` is contained in `developmentActivity`.
#' 
#' @param ... Child nodes or attributes.
#'
#' @section Shared and complex child nodes:
#' * [ddi_dataSrc()]
#' * [ddi_srcChar()]
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