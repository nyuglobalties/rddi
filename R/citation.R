#' citation, sourceCitation, fileCitation and their child nodes
#'
#' Citation entities for the study including general citations and source citations. More information on citation and
#' source citation can be found below and in the references.
#'
#' @param ... Child nodes or attributes. 
#'
#' @section Branch node children allowed:
#' * [ddi_distStmt()]
#' * [ddi_prodStmt()]
#' * [ddi_rspStmt()]
#' * [ddi_serStmt()]
#' * [ddi_titlStmt()]
#' * [ddi_verstmt()]
#' 
#' @section General children allowed:
#' * [ddi_notes()]
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/citation.html}{citation documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/sourceCitation.html}{sourceCitation documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/fileCitation.html}{fileCitation documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/serInfo.html}{serInfo documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/holdings.html}{holdings documentation}
#' 
#' @export
ddi_citation <- function(...) {
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

  # rspStmt, prodStmt, and distStmt are only allowed once in citation according to DDI 2.5
  if(check_cardinality(components$content, "rspStmt") > 1) rddi_err("Only 0 or 1 rspStmt children are allowed in citation")
  if(check_cardinality(components$content, "prodStmt") > 1) rddi_err("Only 0 or 1 prodStmt children are allowed in citation")
  if(check_cardinality(components$content, "distStmt") > 1) rddi_err("Only 0 or 1 distStmt children are allowed in citation")
    
  if (!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                          "MARCURI")
    attribs <- validate_attributes(attribs, allowed_attribs, "citation")
  }

  build_branch_node(
    "citation",
    allowed_children = allowed_children,
    required_children = "titlStmt",
    components = components,
    attribs = attribs
  )
}

#' @rdname ddi_citation
#' @export
ddi_sourceCitation <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", 
                        "MARCURI")
    attribs <- validate_attributes(attribs, allowed_attribs, "sourceCitation")
  }

  allowed_children <- c(
    "titlStmt",
    "respStmt",
    "prodStmt",
    "distStmt",
    "serStmt",
    "verStmt",
    "biblCit",
    "holdings"
  )

  # rspStmt, prodStmt, and distStmt are only allowed once in citation according to DDI 2.5
  if(check_cardinality(components$content, "rspStmt") > 1) rddi_err("Only 0 or 1 rspStmt children are allowed in sourceCitation")
  if(check_cardinality(components$content, "prodStmt") > 1) rddi_err("Only 0 or 1 prodStmt children are allowed in sourceCitation")
  if(check_cardinality(components$content, "distStmt") > 1) rddi_err("Only 0 or 1 distStmt children are allowed in sourceCitation")
  
  build_branch_node(
    "sourceCitation",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_citation
#' @export
ddi_fileCitation <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", 
                        "MARCURI")
    attribs <- validate_attributes(attribs, allowed_attribs, "fileCitation")
  }

  allowed_children <- c(
    "titlStmt",
    "respStmt",
    "prodStmt",
    "distStmt",
    "serStmt",
    "verStmt",
    "biblCit",
    "holdings"
  )

  # rspStmt, prodStmt, and distStmt are only allowed once in citation according to DDI 2.5
  if(check_cardinality(components$content, "rspStmt") > 1) rddi_err("Only 0 or 1 rspStmt children are allowed in fileCitation")
  if(check_cardinality(components$content, "prodStmt") > 1) rddi_err("Only 0 or 1 prodStmt children are allowed in fileCitation")
  if(check_cardinality(components$content, "distStmt") > 1) rddi_err("Only 0 or 1 distStmt children are allowed in fileCitation")
  
  build_branch_node(
    "fileCitation",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}
#' titleStmt and its child nodes
#' 
#' Title statement for the work at the appropriate level: marked-up document; marked-up document source; study; study description, other materials; other materials for the study.
#' More information on the allowed attributes and child nodes can be found below and in the references.
#' 
#' @param ... Child nodes or attributes. 
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/titlStmt.html}{titlStmt documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/titl.html}{titl documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/subTitl.html}{subTitl documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/altTitl.html}{altTitl documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/parTitl.html}{parTitl documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/IDNo.html}{IDNo documentation}
#' 
#' @export
ddi_titlStmt <- function(...) {
  allowed_children <- c(
    "titl",
    "subTitl",
    "altTitl",
    "parTitl",
    "IDNo"
  )

  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if (!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "titlStmt")
  }

  build_branch_node(
    "titlStmt",
    allowed_children = allowed_children,
    required_children = "titl",
    components = components,
    attribs = attribs
  )
}

#' @rdname ddi_titlStmt
#' @export
ddi_titl <-  function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "titl")
  }

  build_leaf_node(
    "titl",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_titlStmt
#' @export
ddi_subTitl <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "subTitl")
  }

  build_leaf_node(
    "subTitl",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_titlStmt
#' @export
ddi_altTitl <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "altTitl")
  }

  build_leaf_node(
    "altTitl",
    attribs = attribs,
    content = components$content
  ) 
}

#' @rdname ddi_titlStmt
#' @export
ddi_parTitl <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "parTitl")
  }

  build_leaf_node(
    "parTitl",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_titlStmt
#' @export
ddi_IDNo <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "agency", "level")
    attribs <- validate_attributes(attribs, allowed_attribs, "IDNo")
    if(!is.null(attribs$level)) check_attribs_in_set(names(attribs), c("study", "file", "project"), field = "level (IDNo)")
  }

  build_leaf_node(
    "IDNo",
    attribs = attribs,
    content = components$content
  )   
}

#' rspStmt and its child nodes
#' 
#' Responsibility statement for the creation of the work at the appropriate level: marked-up document; marked-up document source; study; study description, other material; other 
#' material for study. More information on rspStmt, its allowed attributes, and its child nodes can be found below and in the references
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/rspStmt.html}{rspStmt documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/AuthEnty.html}{AuthEnty documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/othId.html}{othId documentation}
#' 
#' @export
ddi_rspStmt <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "rspStmt")
  }

  allowed_children <- c(
    "AuthEnty",
    "othId"
  )

  build_branch_node(
    "rspStmt",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_rspStmt
#' @export
ddi_AuthEnty <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "affiliation")
    attribs <- validate_attributes(attribs, allowed_attribs, "AuthEnty")
  }

  build_leaf_node(
    "AuthEnty",
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_rspStmt
#' @export
ddi_othId <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "type", "role", "affiliation")
    attribs <- validate_attributes(attribs, allowed_attribs, "othId")
  }

  build_leaf_node(
    "othId",
    attribs = attribs,
    content = components$content
  )   
}

#' prodStmt and its child nodes
#' 
#' Production statement for the work at the appropriate level: marked-up document; marked-up document source; study; study description, other material; 
#' other material for study. More information on prodStmt, its allowed children and attributes can be found below and in the references.
#' 
#' @param ... Child nodes or attributes. 
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/prodStmt.html}{prodStmt documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/copyright.html}{copyright documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/prodDate.html}{prodDate documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/prodPlac.html}{prodPlac documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/software.html}{software documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/fundAg.html}{fundAg documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/grantNo.html}{grantNo documentation}
#' 
#' @export
ddi_prodStmt <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "prodStmt")
  }

  allowed_children <- c(
    "producer",
    "copyright",
    "prodDate",
    "prodPlac",
    "software",
    "fundAg",
    "grantNo"
  )

  build_branch_node(
    "prodStmt",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_prodStmt
#' @export
ddi_copyright <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "copyright")
  }

  build_leaf_node(
    "copyright",
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_prodStmt
#' @export
ddi_prodDate <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "date")
    attribs <- validate_attributes(attribs, allowed_attribs, "prodDate")
  }

  build_leaf_node(
    "prodDate",
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_prodStmt
#' @export
ddi_prodPlac <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "prodPlac")
  }

  build_leaf_node(
    "prodPlac",
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_prodStmt
#' @export
ddi_software <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "date", "version")
    attribs <- validate_attributes(attribs, allowed_attribs, "software")
  }

  build_leaf_node(
    "software",
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_prodStmt
#' @export
ddi_fundAg <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "abbr", "role")
    attribs <- validate_attributes(attribs, allowed_attribs, "fundAg")
  }

  build_leaf_node(
    "fundAg",
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_prodStmt
#' @export
ddi_grantNo <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "agency", "role")
    attribs <- validate_attributes(attribs, allowed_attribs, "grantNo")
  }

  build_leaf_node(
    "grantNo",
    attribs = attribs,
    content = components$content
  )   
}

#' serStmt and its child nodes
#' 
#' Series statement for the work at the appropriate level: marked-up document; marked-up document source; study; study description, other material; other material 
#' for study. More information on its child nodes and attributes can be found below.
#' 
#' @param ... Child nodes or attributes. 
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/serStmt.html}{serStmt documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/serName.html}{serName documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/serInfo.html}{serInfo documentation}
#' 
#' @export
ddi_serStmt <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "URI")
    attribs <- validate_attributes(attribs, allowed_attribs, "serStmt")
  }

  allowed_children <- c(
    "serName",
    "serInfo"
  )

  build_branch_node(
    "serStmt",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_serStmt
#' @export
ddi_serName <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "abbr")
    attribs <- validate_attributes(attribs, allowed_attribs, "serName")
  }

  build_leaf_node(
    "serName",
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_serStmt
#' @export
ddi_serInfo <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "serInfo")
  }

  build_leaf_node(
    "serInfo",
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_citation
#' @export
ddi_biblCit <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "format")
    attribs <- validate_attributes(attribs, allowed_attribs, "biblCit")
  }

  build_leaf_node(
    "biblCit",
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_citation
#' @export
ddi_holdings <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "location", "callno", "URI", "media")
    attribs <- validate_attributes(attribs, allowed_attribs, "holdings")
  }

  build_leaf_node(
    "holdings",
    attribs = attribs,
    content = components$content
  )   
}

#' ddi_distStmt and its children
#'
#' Distribution statement for the work at the appropriate level: marked-up document; marked-up document source; study; study 
#' description, other material; other material for study. For more information see the references.
#'
#' @param ... Child nodes or attributes. 
#'
#' @section General children allowed:
#' * [ddi_contact()]
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/distStmt.html}{distStmt documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/depDate.html}{depDate documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/depositr.html}{depositr documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/distDate.html}{distDate documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/distrbtr.html}{distrbtr documentation}
#' 
#' @export
ddi_distStmt <- function(...) {
  allowed_children <- c(
    "distrbtr",
    "contact",
    "depositr",
    "depDate",
    "distDate"
  )

  components <- dots_to_xml_components(...)
  attribs <- components$attribs    
  
  if (!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "distStmt")
  }

  build_branch_node(
    "distStmt",
    allowed_children = allowed_children,
    components = components,
    attribs = attribs
  )
}

#' @rdname ddi_distStmt
#' @export
ddi_depDate <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "date")
    attribs <- validate_attributes(attribs, allowed_attribs, "depDate")
  }

  build_leaf_node(
    "depDate",
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_distStmt
#' @export
ddi_depositr <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "abbr", "affiliation")
    attribs <- validate_attributes(attribs, allowed_attribs, "depositr")
  }

  build_leaf_node(
    "depositr",
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_distStmt
#' @export
ddi_distDate <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "date")
    attribs <- validate_attributes(attribs, allowed_attribs, "distDate")
  }

  build_leaf_node(
    "distDate",
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_distStmt
#' @export
ddi_distrbtr <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "abbr", "affiliation", "URI")
    attribs <- validate_attributes(attribs, allowed_attribs, "distrbtr")
  }

  build_leaf_node(
    "distrbtr",
    attribs = attribs,
    content = components$content
  )   
}