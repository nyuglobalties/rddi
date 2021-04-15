#' Other Study Description Materials
#'
#' Other materials relating to the study description. This section describes other materials that are related to the study description that are primarily 
#' descriptions of the content and use of the study, such as appendices, sampling information, weighting details, methodological and technical details, 
#' publications based upon the study content, related studies or collections of studies, etc. This section may point to other materials related to the 
#' description of the study through use of the generic citation element, which is available for each element in this section. This maps to Dublin Core 
#' Relation element. Note that codeBook/otherMat (Other Study-Related Materials), should be used for materials used in the production of the study or 
#' useful in the analysis of the study. The materials in codeBook/otherMat may be entered as PCDATA (ASCII text) directly into the document (through use 
#' of the txt element). That section may also serve as a "container" for other electronic materials by providing a brief description of the study-related 
#' materials accompanied by the "type" and "level" attributes further defining the materials. Other Study-Related Materials in codeBook/otherMat may include: 
#' questionnaires, coding notes, SPSS/SAS/Stata setup files (and others), user manuals, continuity guides, sample computer software programs, glossaries of 
#' terms, interviewer/project instructions, maps, database schema, data dictionaries, show cards, coding information, interview schedules, missing values 
#' information, frequency files, variable maps, etc.
#'
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/othrStdyMat.html}
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
        check_attribs_in_set(names(attribs), allowed_attribs, field = "othrStdyMat")
        check_attribs(attribs)
    }

    build_branch_node(
        "othrStdyMat",
        allowed_children = allowed_children,
        components = components,
        attribs = attribs
    )
}

#' Other References Notes
#' 
#' Indicates other pertinent references. Can take the form of bibliographic citations. 
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/othRefs.html}
#' 
#' @export
ddi_othRefs <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "othRefs")
    check_attribs(attribs)
  }

  build_leaf_node(
    "othRefs",
    attribs = attribs,
    content = components$content
  )
}

#' Related Materials
#' 
#' Describes materials related to the study description, such as appendices, additional information on sampling found in other documents, etc. 
#' Can take the form of bibliographic citations. This element can contain either PCDATA or a citation or both, and there can be multiple 
#' occurrences of both the citation and PCDATA within a single element. May consist of a single URI or a series of URIs comprising a series of 
#' citations/references to external materials which can be objects as a whole (journal articles) or parts of objects (chapters or appendices in 
#' articles or documents). 
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/relMat.html}
#' 
#' @export
ddi_relMat <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "callno", "label", "media", "type")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "relMat")
    check_attribs(attribs)
  }

  build_leaf_node(
    "relMat",
    attribs = attribs,
    content = components$content
  )
}

#' Related Publications
#' 
#' Bibliographic and access information aboutvarticles and reports based on the data in this collection. Can take the formbof bibliographic citations. 
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/relPubl.html}
#' 
#' @export
ddi_relPubl <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "relPubl")
    check_attribs(attribs)
  }

  build_leaf_node(
    "relPubl",
    attribs = attribs,
    content = components$content
  )
}

#' Related Studies
#' 
#' Information on the relationship of the current data collection to others (e.g., predecessors, successors, other waves or rounds) or to other editions of the same 
#' file. This would include the names of additional data collections generated from the same data collection vehicle plus other collections directed at the same general 
#' topic. Can take the form of bibliographic citations.  
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/relStdy.html}
#' 
#' @export
ddi_relStdy <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "relStdy")
    check_attribs(attribs)
  }

  build_leaf_node(
    "relStdy",
    attribs = attribs,
    content = components$content
  )
}
