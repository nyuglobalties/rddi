#' othrStdyMat and its leaf node children
#'
#' Other study description materials relating to the study description. This section describes other materials that are related to the study description that are primarily 
#' descriptions of the content and use of the study, such as appendices, sampling information, weighting details, methodological and technical details, 
#' publications based upon the study content, related studies or collections of studies, etc. This section may point to other materials related to the 
#' description of the study through use of the generic citation element, which is available for each element in this section. This maps to Dublin Core 
#' Relation element. More informaiton on othrStdyMat including its attributes and children can be found in the references.
#'
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/othrStdyMat.html}{othrStdyMat documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/othRefs.html}{othRefs documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/relMat.html}{relMat documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/relPubl.html}{relPubl documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/relStdy.html}{relStdy documentation}
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
