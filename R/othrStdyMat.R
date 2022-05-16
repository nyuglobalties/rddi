#' othrStdyMat and its child nodes
#'
#' Other study description materials relating to the study description. This 
#' section describes other materials that are related to the study description 
#' that are primarily descriptions of the content and use of the study, such as 
#' appendices, sampling information, weighting details, methodological and 
#' technical details, publications based upon the study content, related 
#' studies or collections of studies, etc. This section may point to other 
#' materials related to the description of the study through use of the generic 
#' citation element, which is available for each element in this section. This 
#' maps to Dublin Core Relation element.More information on these elements, 
#' especially their allowed attributes, can be found in the references.  
#'
#' \emph{Parent nodes}
#' 
#' `othrStdyMat` is contained in `stdyDscr`.
#' 
#' \emph{othrStdyMat specific child nodes}
#' 
#' * `ddi_othRefs()` indicates other pertinent references. Can take the form of 
#' bibliographic citations. 
#' 
#' * `ddi_relMat()` describes materials related to the study description, such 
#' as appendices, additional information on sampling found in other documents, 
#' etc. Can take the form of bibliographic citations. This element can contain 
#' either PCDATA or a citation or both, and there can be multiple occurrences of 
#' both the citation and PCDATA within a single element. May consist of a single 
#' URI or a series of URIs comprising a series of citations/references to 
#' external materials which can be objects as a whole (journal articles) or 
#' parts of objects (chapters or appendices in articles or documents).
#' 
#' * `ddi_relPubl()` are bibliographic and access information about articles 
#' and reports based on the data in this collection. Can take the form of 
#' bibliographic citations. 
#' 
#' * `ddi_relStdy()` is information on the relationship of the current data 
#' collection to others (e.g., predecessors, successors, other waves or rounds) 
#' or to other editions of the same file. This would include the names of 
#' additional data collections generated from the same data collection vehicle 
#' plus other collections directed at the same general topic. Can take the form 
#' of bibliographic citations.
#'
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object. 
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/othrStdyMat.html}{othrStdyMat documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/othRefs.html}{othRefs documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/relMat.html}{relMat documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/relPubl.html}{relPubl documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/relStdy.html}{relStdy documentation}
#' 
#' @examples 
#' ddi_othrStdyMat()
#' 
#' # Functions that need to be wrapped in ddi_othrStdyMat()
#' 
#' ddi_othRefs("Part II of the documentation, the Field Representative's Manual, 
#'             is provided in hardcopy form only.")
#' 
#' ddi_relMat("Full details on the research design and procedures, sampling 
#'            methodology, content areas, and questionnaire design, as well as 
#'            percentage distributions by respondent's sex, race, region, college 
#'            plans, and drug use, appear in the annual ISR volumes MONITORING 
#'            THE FUTURE: QUESTIONNAIRE RESPONSES FROM THE NATION'S HIGH SCHOOL 
#'            SENIORS.")
#'            
#' ddi_relPubl("Economic Behavior Program Staff. SURVEYS OF CONSUMER FINANCES. 
#'             Annual volumes 1960 through 1970. Ann Arbor, MI: Institute for 
#'             Social Research.")
#'             
#' ddi_relStdy("ICPSR distributes a companion study to this collection titled 
#'              FEMALE LABOR FORCE PARTICIPATION AND MARITAL INSTABILITY, 1980: 
#'              [UNITED STATES] (ICPSR 9199).")
#' 
#' @export
ddi_othrStdyMat <- function(...) {
  allowed_children <- c(
    "relMat",
    "relStdy", 
    "relPubl",
    "othRefs"
  )

  components <- dots_to_xml_components(...)
  attribs <- components$attribs    

  if (!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "othrStdyMat")
  }

  build_branch_node(
    "othrStdyMat",
    allowed_children = allowed_children,
    components = components,
    attribs = attribs
  )
}

#' @rdname ddi_othrStdyMat
#' @export
ddi_othRefs <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "othRefs")
  }

  if(length(components$content) == 1 & is.character(components$content[[1]])) {
    build_leaf_node(
      "othRefs",
      attribs = attribs,
      content = components$content
    )
  } else {
    allowed_children = c("citation")
    build_branch_node(
      "othRefs",
      content = unwrap_content(components$content),
      attribs = components$attribs,
      allowed_children = allowed_children
    )
  }
}

#' @rdname ddi_othrStdyMat
#' @export
ddi_relMat <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "callno", "label", "media", "type")
    attribs <- validate_attributes(attribs, allowed_attribs, "relMat")
  }

  if(length(components$content) == 1 & is.character(components$content[[1]])) {
    build_leaf_node(
      "relMat",
      attribs = attribs,
      content = components$content
    )
  } else {
    allowed_children = c("citation")
    build_branch_node(
      "relMat",
      content = unwrap_content(components$content),
      attribs = components$attribs,
      allowed_children = allowed_children
    )
  }
}

#' @rdname ddi_othrStdyMat
#' @export
ddi_relPubl <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "relPubl")
  }

  if(length(components$content) == 1 & is.character(components$content[[1]])) {
    build_leaf_node(
      "relPubl",
      attribs = attribs,
      content = components$content
    )
  } else {
    allowed_children = c("citation")
    build_branch_node(
      "relPubl",
      content = unwrap_content(components$content),
      attribs = components$attribs,
      allowed_children = allowed_children
    )
  }
}

#' @rdname ddi_othrStdyMat
#' @export
ddi_relStdy <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "relStdy")
  }

  if(length(components$content) == 1 & is.character(components$content[[1]])) {
    build_leaf_node(
      "relStdy",
      attribs = attribs,
      content = components$content
    )
  } else {
    allowed_children = c("citation")
    build_branch_node(
      "relStdy",
      content = unwrap_content(components$content),
      attribs = components$attribs,
      allowed_children = allowed_children
    )
  }
}
