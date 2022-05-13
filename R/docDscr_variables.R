#' docDscr and its children
#' 
#' The Document Description consists of bibliographic information describing the 
#' DDI-compliant document itself as a whole. This Document Description can be 
#' considered the wrapper or header whose elements uniquely describe the full 
#' contents of the compliant DDI file. Since the Document Description section 
#' is used to identify the DDI-compliant file within an electronic resource 
#' discovery environment, this section should be as complete as possible. The 
#' author in the Document Description should be the individual(s) or 
#' organization(s) directly responsible for the intellectual content of the DDI 
#' version, as distinct from the person(s) or organization(s) responsible for 
#' the intellectual content of the earlier paper or electronic edition from 
#' which the DDI edition may have been derived. The producer in the Document 
#' Description should be the agency or person that prepared the marked-up 
#' document. Note that the Document Description section contains a 
#' Documentation Source subsection consisting of information about the source 
#' of the DDI-compliant file-- that is, the hardcopy or electronic codebook 
#' that served as the source for the marked-up codebook. These sections allow 
#' the creator of the DDI file to produce version, responsibility, and other 
#' descriptions relating to both the creation of that DDI file as a separate 
#' and reformatted version of source materials (either print or electronic) and 
#' the original source materials themselves. More information on this element, 
#' especially the allowed attributes, can be found in the references.
#' 
#' \emph{Parent node}
#'
#' `docDscr` is contained in `codeBook`.
#' 
#' \emph{docDscr specific child nodes}
#' 
#' * `ddi_docStatus()` indicates if the documentation is being 
#' presented/distributed before it has been finalized. Some data producers and 
#' social science data archives employ data processing strategies that provide 
#' for release of data and documentation at various stages of processing. The 
#' element may be repeated to support multiple language expressions of the 
#' content.
#' 
#' * `ddi_guide()` is the list of terms and definitions used in the 
#' documentation. Provided to assist users in using the document correctly. 
#' 
#' @param ... Child nodes or attributes. 
#'
#' @section Shared and complex child nodes:
#' 
#' * [ddi_citation()]
#' * [ddi_controlledVocabUsed()]
#' * [ddi_docSrc()] 
#' * [ddi_notes()] 
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/docDscr.html}{docDscr documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/docStatus.html}{docStatus documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/guide.html}{guide documentation}
#' 
#' @export
ddi_docDscr <- function(...) {
  allowed_children <- c(
    "citation",
    "controlledVocabUsed",
    "docSrc",
    "docStatus",
    "guide",
    "notes"
  )
  
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn"
    )
    attribs <- validate_attributes(attribs, allowed_attribs, "docDscr")
  }
  
  if(check_cardinality(components$content, "citation") > 1) rddi_err("Only 0 or 1 citation children are allowed in docDscr")
  
  build_branch_node(
    "docDscr",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}


#' @rdname ddi_docDscr
#' @export
ddi_docStatus <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleURN", "ddiCodebookURN")
    attribs <- validate_attributes(attribs, allowed_attribs, "docStatus")
  }
  
  build_leaf_node(
    "docStatus",
    attribs = attribs,
    content = components$content 
  )
}

#' @rdname ddi_docDscr
#' @export
ddi_guide <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleURN", "ddiCodebookURN")
    attribs <- validate_attributes(attribs, allowed_attribs, "guide")
  }
  
  build_leaf_node(
    "guide",
    attribs = attribs,
    content = components$content 
  )
}

#' docSrc and its child nodes
#'
#' Citation for the source document. This element encodes the bibliographic 
#' information describing the source codebook, including title information, 
#' statement of responsibility, production and distribution information, series 
#' and version information, text of a preferred bibliographic citation, and 
#' notes (if any). Information for this section should be taken directly from 
#' the source document whenever possible. If additional information is obtained 
#' and entered in the elements within this section, the source of this 
#' information should be noted in the source attribute of the particular 
#' element tag. More information on this element, especially the allowed 
#' attributes, can be found in the references.
#' 
#' \emph{Parent node}
#'
#' `docSrc` is contained in `docDscr`.
#'
#' @param ... Child nodes or attributes. 
#'
#' @section Shared and complex child nodes:
#' * [ddi_biblCit()]
#' * [ddi_distStmt()]
#' * [ddi_holdings()]
#' * [ddi_notes()]
#' * [ddi_prodStmt()]
#' * [ddi_rspStmt()]
#' * [ddi_serStmt()]
#' * [ddi_titlStmt()]
#' * [ddi_verStmt()]
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/docSrc.html}{docSrc documentation}
#' 
#' @export
ddi_docSrc <- function(...) {
  allowed_children <- c(
    "titlStmt",
    "rspStmt", 
    "prodStmt",
    "distStmt",
    "serStmt",
    "verStmt",
    "biblCit",
    "holdings",
    "notes"
  )
  
  components <- dots_to_xml_components(...)
  attribs <- components$attribs    
  
  if (!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                         "MARCURI")
    attribs <- validate_attributes(attribs, allowed_attribs, "docSrc")
  }
  
  if(check_cardinality(components$content, "rspStmt") > 1) rddi_err("Only 0 or 1 rspStmt children are allowed in docSrc")
  if(check_cardinality(components$content, "prodStmt") > 1) rddi_err("Only 0 or 1 prodStmt children are allowed in docSrc")
  if(check_cardinality(components$content, "distStmt") > 1) rddi_err("Only 0 or 1 distStmt children are allowed in docSrc")
  
  
  build_branch_node(
    "docSrc",
    allowed_children = allowed_children,
    components = components,
    attribs = attribs
  )
}

#' controlledVocabUsed and its child nodes
#'
#' Provides a code value, as well as a reference to the code list from which 
#' the value is taken. Note that the CodeValue can be restricted to reference 
#' an enumeration. More information on this element, especially the allowed 
#' attributes, can be found in the references.
#' 
#' \emph{Parent node}
#'
#' `controlledVocabUsed` is contained in `docDscr`.
#' 
#' \emph{controlledVocabUsed specific child nodes}
#' 
#' * `ddi_codeListAgencyName()` is the agency maintaining the code list.
#' 
#' * `ddi_codeListID()` identifies the code list that the value is taken from.
#' 
#' * `ddi_codeListName()` identifies the code list that the value is taken from
#' with a human-readable name.
#' 
#' * `ddi_codeListSchemeURN()` identifies the code list scheme using a URN.
#' 
#' * `ddi_codeListURN()` identifies the code list that the vlaue is taken from 
#' with a URN.
#' 
#' * `ddi_codeListVersionID()` is the version of the code list. (Default value 
#' is 1.0).
#'
#' @param ... Child nodes or attributes. 
#'
#' @section Shared and complex child nodes:
#' * [ddi_usage()]
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/controlledVocabUsed.html}{controlledVocabUsed documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/codeListAgencyName.html}{codeListAgencyName documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/codeListID.html}{codeListID documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/codeListName.html}{codeListName documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/codeListSchemeURN.html}{codeListSchemeURN documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/codeListURN.html}{codeListURN documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/codeListVersionID.html}{codeListVersionID documentation}
#' 
#' @export
ddi_controlledVocabUsed <- function(...) {
  allowed_children <- c(
    "codeListAgencyName",
    "codeListID",
    "codeListName",
    "codeListSchemeURN",
    "codeListURN",
    "codeListVersionID",
    "usage"
  )
  
  components <- dots_to_xml_components(...)
  attribs <- components$attribs    
  
  if (!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "controlledVocabUsed")
  }
  
  if(check_cardinality(components$content, "codeListID") > 1) rddi_err("Only 0 or 1 codeListID children are allowed in controlledVocabUsed")
  if(check_cardinality(components$content, "codeListName") > 1) rddi_err("Only 0 or 1 codeListName children are allowed in controlledVocabUsed")
  if(check_cardinality(components$content, "codeListAgencyName") > 1) rddi_err("Only 0 or 1 codeListAgencyName children are allowed in controlledVocabUsed")
  if(check_cardinality(components$content, "codeListVersionID") > 1) rddi_err("Only 0 or 1 codeListVersionID children are allowed in controlledVocabUsed")
  if(check_cardinality(components$content, "codeListURN") > 1) rddi_err("Only 0 or 1 codeListURN children are allowed in controlledVocabUsed")
  if(check_cardinality(components$content, "codeListSchemeURN") > 1) rddi_err("Only 0 or 1 codeListSchemeURN children are allowed in controlledVocabUsed")
  
  build_branch_node(
    "docSrc",
    allowed_children = allowed_children,
    required_children = "usage",
    components = components,
    attribs = attribs
  )
}

#' @rdname ddi_controlledVocabUsed
#' @export
ddi_codeListAgencyName <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleURN", "ddiCodebookURN")
    attribs <- validate_attributes(attribs, allowed_attribs, "codeListAgencyName")
  }
  
  build_leaf_node(
    "codeListAgencyName",
    attribs = attribs,
    content = components$content 
  )
}


#' @rdname ddi_controlledVocabUsed
#' @export
ddi_codeListID <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleURN", "ddiCodebookURN")
    attribs <- validate_attributes(attribs, allowed_attribs, "codeListID")
  }
  
  build_leaf_node(
    "codeListID",
    attribs = attribs,
    content = components$content 
  )
}

#' @rdname ddi_controlledVocabUsed
#' @export
ddi_codeListName <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleURN", "ddiCodebookURN")
    attribs <- validate_attributes(attribs, allowed_attribs, "codeListName")
  }
  
  build_leaf_node(
    "codeListName",
    attribs = attribs,
    content = components$content 
  )
}

#' @rdname ddi_controlledVocabUsed
#' @export
ddi_codeListSchemeURN <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleURN", "ddiCodebookURN")
    attribs <- validate_attributes(attribs, allowed_attribs, "codeListSchemeURN")
  }
  
  build_leaf_node(
    "codeListSchemeURN",
    attribs = attribs,
    content = components$content 
  )
}

#' @rdname ddi_controlledVocabUsed
#' @export
ddi_codeListURN <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleURN", "ddiCodebookURN")
    attribs <- validate_attributes(attribs, allowed_attribs, "codeListURN")
  }
  
  build_leaf_node(
    "codeListURN",
    attribs = attribs,
    content = components$content 
  )
}

#' @rdname ddi_controlledVocabUsed
#' @export
ddi_codeListVersionID <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleURN", "ddiCodebookURN")
    attribs <- validate_attributes(attribs, allowed_attribs, "codeListVersionID")
  }
  
  build_leaf_node(
    "codeListVersionID",
    attribs = attribs,
    content = components$content 
  )
}

#' usage and its child nodes
#'
#' Defines where in the instance the controlled vocabulary which is identified 
#' is utilized. A controlled vocabulary may occur either in the content of an 
#' element or in an attribute on an element. The usage can either point to a 
#' collection of elements using an XPath via the selector element or point to a 
#' more specific collection of elements via their identifier using the 
#' specificElements element. If the controlled vocabulary occurs in an 
#' attribute within the element, the attribute element identifies the specific 
#' attribute. When specific elements are specified, an authorized code value 
#' may also be provided. If the current value of the element or attribute 
#' identified is not in the controlled vocabulary or is not identical to a 
#' code value, the authorized code value identifies a valid code value 
#' corresponding to the meaning of the content in the element or attribute. 
#' More information on this element, especially the allowed attributes, can be 
#' found in the references.
#' 
#' \emph{Parent nodes}
#'
#' `usage` is contained in `controlledVocabUsed`.
#' 
#' \emph{usage specific child nodes}
#' 
#' * `ddi_attribute()` identifies an attribute within the element(s) identified 
#' by the selector or specificElements in which the controlled vocabulary is 
#' used. The fully qualified name used here must correspond to that in the 
#' instance, which is to say that if the attribute is namespace qualified, the 
#' prefix used here must match that which is defined in the instance.
#' 
#' * `ddi_selector()` identifies a collection of elements in which a controlled 
#' vocabulary is used. This is a simplified XPath which must correspond to the 
#' actual instance in which it occurs, which is to say that the fully qualified 
#' element names here must correspond to those in the instance. This XPath can 
#' only identify elements and does not allow for any predicates. The XPath must 
#' either be rooted or deep.
#' 
#' * `ddi_specificElements()` identifies a collection of specific elements via 
#' their identifiers in the refs attribute, which allows for a tokenized list 
#' of identifier values which must correspond to identifiers which exist in the 
#' instance. The authorizedCodeValue attribute can be used to provide a valid 
#' code value corresponding to the meaning of the content in the element or 
#' attribute when the identified element or attribute does not use an actual 
#' valid value from the controlled vocabulary.
#'
#' @param ... Child nodes or attributes. 
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/usage.html}{usage documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/attribute.html}{attribute documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/selector.html}{selector documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/specificElements.html}{specificElements documentation}
#' 
#' @export
ddi_usage <- function(...) {
  allowed_children <- c(
    "attribute",
    "selector",
    "specificElements"
  )
  
  components <- dots_to_xml_components(...)
  attribs <- components$attribs    
  
  if (!is.null(attribs)) {
    rddi_err( "No attributes are allowed in ddi_usage()")
  }
  
  if(check_cardinality(components$content, "attribute") > 1) rddi_err("Only 0 or 1 attribute children are allowed in usage")

  if(check_cardinality(components$content, "selector") > 0 & check_cardinality(components$content, "specificElements") > 0) {
    rddi_err("usage does not allow for selector and specificElements to both be child nodes.")
  }
  
  
  build_branch_node(
    "usage",
    allowed_children = allowed_children,
    components = components,
    attribs = attribs
  )
}

#' @rdname ddi_usage
#' @export
ddi_attribute <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if (!is.null(attribs)) {
    rddi_err( "No attributes are allowed in ddi_attribute()")
  }
  
  build_leaf_node(
    "attribute",
    attribs = attribs,
    content = components$content 
  )
}

#' @rdname ddi_usage
#' @export
ddi_selector <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if (!is.null(attribs)) {
    rddi_err( "No attributes are allowed in ddi_selector()")
  }
  
  build_leaf_node(
    "selector",
    attribs = attribs,
    content = components$content 
  )
}

#' @rdname ddi_usage
#' @export
ddi_specificElements <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("refs", "authorizedCodeValue")
    attribs <- validate_attributes(attribs, allowed_attribs, "specificElements")
  }
  
  
  build_leaf_node(
    "specificElements",
    attribs = attribs
  )
}