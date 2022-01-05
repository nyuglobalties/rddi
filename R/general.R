#' labl node
#' 
#' A short description of the parent element. In the variable label, the length of this phrase may depend on the statistical analysis system 
#' used (e.g., some versions of SAS permit 40-character labels, while some versions of SPSS permit 120 characters), although the DDI itself imposes no restrictions 
#' on the number of characters allowed.
#'
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/labl.html}{labl documentation}
#'  
#' @export
ddi_labl <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
 
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", 
                        "level", "vendor", "country", "sdatrefs")
    attribs <- validate_attributes(attribs, allowed_attribs, "labl")
  }

  build_leaf_node (
    "labl",
    attribs = attribs,
    content = components$content
  )
}

#' notes node
#' 
#' For clarifying information/annotation regarding the parent element
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/notes.html}
#'  
#' @export
ddi_notes <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", 
                        "type", "subject", "level", "resp", "sdatrefs", "parent", "sameNote")
    attribs <- validate_attributes(attribs, allowed_attribs, "notes")
  }

  build_leaf_node (
    "notes",
    attribs = attribs,
    content = components$content
  )
}

#' universe node
#'
#' The group of persons or other elements that are the object of research and to which any analytic results refer. Age,nationality, and residence 
#' commonly help to delineate a given universe, but any of a number of factors may be involved, such as sex, race, income, veteran status, criminal convictions, etc. 
#' The universe may consist of elements other than persons, such as housing units, court cases, deaths, countries, etc. In general, it should be possible to tell from 
#' the description of the universe whether a given individual or element (hypothetical or real) is a member of the population under study.
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/universe.html}
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#' 
#' @export
ddi_universe <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", 
                      "level", "clusion") 
    attribs <- validate_attributes(attribs, allowed_attribs, "universe")
  }
  browser()
  if(length(components$content) == 1 & is.character(components$content[[1]])) {
    build_leaf_node(
      "concept",
      attribs = attribs,
      content = components$content
    )
  } else {
    allowed_children = c("txt", "concept")
    build_branch_node(
      "universe",
      content = unwrap_content(components$content),
      attribs = components$attribs,
      allowed_children = allowed_children
    )
  }
}

#' concept node
#' 
#' The general subject to which the parent element may be seen as pertaining. This element serves the same purpose as the keywords and topic classification elements, 
#' but at the data description level. The "vocab" attribute is provided to indicate the controlled vocabulary, if any, used in the element, e.g., LCSH (Library of Congress Subject 
#' Headings), MeSH (Medical Subject Headings), etc. The "vocabURI" attribute specifies the location for the full controlled vocabulary.
#' 
#' @param ... Any other parameters from the DDI Codebook 2.5 schema
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/concept.html}
#'  
#' @export
ddi_concept <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", 
                        "vocab", "vocabURI")
    attribs <- validate_attributes(attribs, allowed_attribs, "concept")
  }

  build_leaf_node(
    "concept",
    attribs = attribs,
    content = components$content
  )
}

#' verStmt and its child nodes
#' 
#' This is the version statement for the work at the appropriate level: marked-up document; marked-up document source; study; study description, other material; 
#' other material for study. More information on verStmt, its children, and attributes can be found in hte references.
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#'
#' @section General children allowed:
#' * [ddi_notes()] 
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/verStmt.html}{verStmt documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/version.html}{version documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/verResp.html}{verResp documentation}
#'  
#' @export
ddi_verStmt <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn") 
    attribs <- validate_attributes(attribs, allowed_attribs, "verStmt")
  }

  allowed_children <- c(
    "version",
    "verResp",
    "notes"
  )

  build_branch_node(
    "verStmt",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_verStmt 
#' @export
ddi_version <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "date", "type") 
    attribs <- validate_attributes(attribs, allowed_attribs, "version")
  }

  build_leaf_node(
    "version",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_verStmt 
#' @export
ddi_verResp <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                      "affiliation") 
    attribs <- validate_attributes(attribs, allowed_attribs, "verResp")
  }

  build_leaf_node(
    "verResp",
    attribs = attribs,
    content = components$content
  )
}

#' txt node
#' 
#' Lengthier description of the parent element
#'
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/txt.html}{txt documentation}
#'  
#' @export
ddi_txt <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", 
                        "level", "sdatrefs")
    attribs <- validate_attributes(attribs, allowed_attribs, "txt")
  }

  build_leaf_node(
    "txt",
    attribs = attribs,
    content = components$content 
  )
}


#' producer node
#' 
#' The producer is the person or organization with the financial or administrative responsibility for the physical processes whereby the document was brought into 
#' existence. Use the "role" attribute to distinguish different stages of involvement in the production process, such as original producer. Producer of data collection 
#' (codeBook/stdyDscr/citation/prodStmt/producer) maps to Dublin Core Publisher element. The "producer" in the Document Description should be the agency or person that 
#' prepared the marked-up document.
#' 
#' @param ... Child nodes or attributes. 
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/producer.html}{producer documentation}
#' 
#' @export
ddi_producer <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "abbr", "role", "affiliation")
    attribs <- validate_attributes(attribs, allowed_attribs, "producer")
  }

  build_leaf_node(
    "producer",
    attribs = attribs,
    content = components$content
  )   
}

#' contact node
#' 
#' Names and addresses of individuals responsible for the work. Individuals listed as contact persons will be used as resource persons regarding problems or 
#' questions raised by the user community. The URI attribute should be used to indicate a URN or URL for the homepage of the contact individual. The 
#' email attribute is used to indicate an email address for the contact individual. 
#' 
#' @param ... Child nodes or attributes. 
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/contact.html}{contact documentation}
#' 
#' @export
ddi_contact <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "affiliation", "URI", "email")
    attribs <- validate_attributes(attribs, allowed_attribs, "contact")
  }

  build_leaf_node(
    "contact",
    attribs = attribs,
    content = components$content
  )
}

#' dataSrc node
#' 
#' @param ... Child nodes or attributes. 
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataSrc_1.html}
#' 
#' @export
ddi_dataSrc <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "dataSrc")
  }

  build_leaf_node(
    "dataSrc",
    attribs = attribs,
    content = components$content
  )
}

#' srcChar node
#' 
#' Assessment of characteristics and quality of source material. May not be relevant to survey data. This element may be repeated to support multiple 
#' language expressions of the content.
#' 
#' @param ... Child nodes or attributes. 
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataSrc.html}{dataSrc documentation when in sources}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataSrc_1.html}{dataSrc documentation when in resources}
#'  
#' @export
ddi_srcChar <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "srcChar")
  }

  build_leaf_node(
    "srcChar",
    attribs = attribs,
    content = components$content
  )
}

#' srcDocu node
#' 
#' Level of documentation of the original sources. May not be relevant to survey data. This element may be repeated to support multiple language expressions of the content.
#' 
#' @param ... Child nodes or attributes. 
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/srcDocu.html}{srcDocu documentation}
#' 
#' @export
ddi_srcDocu <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "srcDocu")
  }

  build_leaf_node(
    "srcDocu",
    attribs = attribs,
    content = components$content
  )
}

#' srcOrig documentation
#' 
#' For historical materials, information about the origin(s) of the sources and the rules followed in establishing the sources should be 
#' specified. May not be relevant to survey data. This element may be repeated to support multiple language expressions of the content.
#' 
#' @param ... Child nodes or attributes.
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/srcOrig.html}{srcOrig documentation}
#' 
#' @export
ddi_srcOrig <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "srcOrig")
  }

  build_leaf_node(
    "srcOrig",
    attribs = attribs,
    content = components$content
  )
}

#' useStmt and its children
#' 
#' Information on terms of use for the data collection. This element may be repeated only to support multiple language expressions of the content. 
#' For more information on useStmt, its attributes and its children see the references
#' 
#' @param ... Child nodes or attributes. 
#'
#' @section General children allowed:
#' * [ddi_contact()]
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/useStmt.html}{useStmt documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/citReq.html}{citReq documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/conditions.html}{conditions documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/confDec.html}{confDec documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/deposReq.html}{deposReq documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/disclaimer.html}{disclaimer documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/restrctn.html}{restrctn documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/specPerm.html}{specPerm documentation}
#' 
#' @export
ddi_useStmt <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  allowed_children <- c(
    "confDec",
    "specPerm",
    "restrctn",
    "contact",
    "citReq",
    "deposReq",
    "conditions",
    "disclaimer"
  )   
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "media", "callno", "label", "type")
    attribs <- validate_attributes(attribs, allowed_attribs, "useStmt")
  }

  build_branch_node(
    "useStmt",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_useStmt
#' @export
ddi_citReq <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "citReq")
  }

  build_leaf_node(
    "citReq",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_useStmt 
#' @export
ddi_conditions <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "conditions")
  }

  build_leaf_node(
    "conditions",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_useStmt
#' @export
ddi_confDec <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "required", "formNo", "URI")
    attribs <- validate_attributes(attribs, allowed_attribs, "confDec")
  }

  build_leaf_node(
    "confDec",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_useStmt
#' @export
ddi_deposReq <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "deposReq")
  }

  build_leaf_node(
    "deposReq",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_useStmt
#' @export
ddi_disclaimer <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "disclaimer")
  }

  build_leaf_node(
    "disclaimer",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_useStmt
#' @export
ddi_restrctn <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "restrctn")
  }

  build_leaf_node(
    "restrctn",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_useStmt
#' @export
ddi_specPerm <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                          "formNo", "URI")
    attribs <- validate_attributes(attribs, allowed_attribs, "specPerm")
  }

  build_leaf_node(
    "specPerm",
    attribs = attribs,
    content = components$content
  )
}