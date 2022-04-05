#' var and its leaf node children
#'
#' Variables are specified with the var tag, which has a litany of 
#' attributes and children. More information on the allowed attributes for var 
#' and its child nodes can be found below and in the references. 
#'  
#' @section Branch node children allowed:
#' * [ddi_catgry()]
#' * [ddi_catgryGrp()]
#' * [ddi_derivation()]
#' * [ddi_invalrng()]
#' * [ddi_valrng()]
#' 
#' @section General children allowed:
#' * `ddi_concept()`
#' * `ddi_labl()` 
#' * `ddi_notes()` 
#' * `ddi_txt()`
#' * `ddi_universe()`
#' * `ddi_verStmt()` 
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/var.html}{var documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/anlysUnit.html}{anlysUnit documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/catLevel.html}{catlevel documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/codInstr.html}{codInstr documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/embargo.html}{embargo documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/geoMap.html}{geomap documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/imputation.html}{imputation documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/location.html}{location documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/qstn.html}{qstn documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/respUnit.html}{respUnit documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/security.html}{security documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/stdCatgry.html}{stdCatgry documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/sumStat.html}{sumStat documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/TotlResp.html}{TotlResp documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/undocCod.html}{undocCod documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/varFormat.html}{varFormat documentation}
#' 
#' @param varname The variable name
#' @param ... Any parameters from the DDI Codebook 2.5 schema. See the references or go the branch node functions for more information.
#'
#' @export
ddi_var <- function(varname, ...) {
  stopifnot(is.character(varname))
  stopifnot(length(varname) == 1)

  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  attribs$name <- varname

  allowed_attribs <- c(
    "ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", "name", "wgt", "wgt-var", "weight",
    "qstn", "files", "vendor", "dcml", "intrvl", "rectype", "sdatrefs", "methrefs", "pubrefs", "access", "aggrMeth", "otherAggrMeth", "measUnit", 
    "scale", "origin", "nature", "temporal", "geog", "geoVocab", "catQnty", "representationType", "otherRepresentationType"
  ) 

  attribs <- validate_attributes(attribs, allowed_attribs, "var")

  allowed_children <- c(
    "catgry", "catLevel", "labl", "qstn", "sumStat", "notes", "anlysUnit", "imputation", "txt",
    "codInstr", "TotlResp", "security", "embargo", "respUnit", "undocCod", "stdCatgry", "concept", "varFormat",
    "valrng", "invalrng", "universe", "catgryGrp", "verStmt", "derivation", "geomap", "location"
  )

  # derivation and varFormat are only allowed once in var according to DDI 2.5
  if(check_cardinality(components$content, "derivation") > 1) rddi_err("Only 0 or 1 derivation children are allowed in var")
  if(check_cardinality(components$content, "varFormat") > 1) rddi_err("Only 0 or 1 varFormat children are allowed in var")
   
  build_branch_node(
    "var",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' DDI varGrp specification and leaf node children
#'
#' A group of variables that may share a common subject, arise from the interpretation of a single question, or are linked by some other factor.
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#'
#' @section varGrp leaf node children allowed:
#' * `ddi_defntn` - \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/defntn.html}{DDI documentation}
#' 
#' @section General children allowed:
#' * `ddi_concept()`
#' * `ddi_labl()` 
#' * `ddi_notes()` 
#' * `ddi_txt()`
#' * `ddi_universe()`
#' 
#' @section DDI Codebook 2.5 Documentation:
#'
#' \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/varGrp.html}{varGrp documentation}
#'
#' @export
ddi_varGrp <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c(
      "ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", 
      "type", "otherType", "var", "varGroup", "name", "sdatrefs", "methrefs", "pubrefs", "access" 
    )
    attribs <- validate_attributes(attribs, allowed_attribs, "varGrp")    
    if("type" %in% names(attribs)) {
      check_attribs_in_set(attributes = attribs$type, vals = c("section", "multipleResp", "grid", "display", "repetition", "subject", "version", 
                      "iteration", "analysis", "pragmatic", "record", "file", "randomized", "other"), field = "type (varGrp)")
    }
  }

  allowed_children <- c(
    "labl",
    "txt",
    "concept",
    "defntn",
    "universe",
    "notes"
  )

  build_branch_node(
    "varGrp",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_var
#' @export
ddi_catLevel <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleURN", "ddiCodebookURN", 
                    "levelnm", "geoMap")
    attribs <- validate_attributes(attribs, allowed_attribs, "catLevel")
  }

  build_leaf_node(
    "catLevel",
    attribs = attribs 
  )
}

#' DDI catgry and catgryGrp specification and leaf node children
#'
#' `ddi_catgry()` is a description of a particluar categorical response. 
#' `ddi_catgryGrp()` groups the responses together.
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#' 
#' @section catgry leaf node children allowed:
#' catgry accepts the following children as leaf nodes. catgryGrp also accepts catStat.
#' * `ddi_catStat()` - \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/catStat.html}{DDI documentation}
#' * `ddi_catValue()` - \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/catValu.html}{DDI documentation}
#' 
#' @section Branch node children allowed:
#' * `ddi_mrow()`
#' 
#' @section General children allowed:
#' * `ddi_labl()` 
#' * `ddi_txt()`
#'  
#' @section DDI 2.5 Documentation:
#' * \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/catgry.html}{catgry documentation}
#' * \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/catgryGrp.html}{catgryGrp documentation}
#'
#' @export
ddi_catgry <- function(...)  {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  allowed_children <- c(
    "catStat",
    "catValu", # allowed once
    "labl",
    "txt",
    "mrow" # allowed once
  )
  
  # catValu and mrow are only allowed once in var according to DDI 2.5
  if(check_cardinality(components$content, "catValu") > 1) rddi_err("Only 0 or 1 catValu children are allowed in catgry")
  if(check_cardinality(components$content, "mrow") > 1) rddi_err("Only 0 or 1 mrow children are allowed in catgry")
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", "missing", "missType", "country",
                      "sdatrefs", "catgry", "level", "excls")
    attribs <- validate_attributes(attribs, allowed_attribs, "catgry")
  }

  build_branch_node(
    "catgry",
    allowed_children = allowed_children,
    attribs = components$attribs,
    content = components$content
  )
}

#' DDI mrow and mi specification 
#'
#' mrow or Mathematical Row is a wrapper containing the presentation expression `ddi_mi()`. It creates a single string 
#' without spaces consisting of the individual elements described within it. It can be used to create a single 
#' variable by concatenating other variables into a single string. It is used to create linking variables composed
#' of multiple non-contiguous parts, or to define unique strings for various category values of a single variable. `ddi_mi()`
#' requires the varRef variable
#' 
#' @section Branch node children allowed:
#' * `ddi_mi()` - \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/mi.html}{DDI documentation}
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/mrow.html}{mrow documentation}
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#' 
#' @export
ddi_mrow <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn") 
    attribs <- validate_attributes(attribs, allowed_attribs, "mrow")
  }

  allowed_children <- c(
    "mi"
  )

  build_branch_node(
    "mrow",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_var
#' @export
ddi_anlysUnit <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "anlysUnit")
  }

  build_leaf_node(
    "anlysUnit",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_catgry 
#' @export
ddi_catgryGrp <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", 
                        "missing", "missType", "catgry", "catGrp", "levelno", "levelnm", "compl", "excls") 
    attribs <- validate_attributes(attribs, allowed_attribs, "catgryGrp")
  }

  allowed_children <- c(
    "labl",
    "catStat",
    "txt"
  )

  build_branch_node(
    "catgryGrp",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' DDI derivation specification and leaf node children
#' 
#' Used only in the case of a derived variable, this element provides both a description of how the derivation was performed and the command used to generate 
#' the derived variable, as well as a specification of the other variables in the study used to generate the derivation. The "var" attribute provides the ID values of the 
#' other variables in the study used to generate this derived variable.
#' 
#' @section derivation leaf node children allowed:
#' * `ddi_drvcmd()` - \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/drvcmd.html}{DDI documentation}
#' * `ddi_drvdesc()` - \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/drvdesc.html}{DDI documentation}
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/derivation.html}{derivation documentation}
#'
#' @export
ddi_derivation <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                      "var") 
    attribs <- validate_attributes(attribs, allowed_attribs, "derivation")
  }

  allowed_children <- c(
    "drvdesc",
    "drvcmd"
  )

  build_branch_node(
    "derivation",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' Valid and invalid data values specification and leaf node children
#'
#' Values for a particular variable that represent legitimate responses (valrng) or illegitamate response (invalrng). Must include item or range as a child element
#'
#' @param ... Any other parameters from the DDI Codebook 2.5 schema
#'
#' @section valrng and invalrng leaf node children allowed:
#' * ddi_item: \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/item.html}{DDI documentation}
#' * ddi_range: \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/range.html}{DDI documentation}
#' * ddi_key: \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/key.html}{DDI documentation}
#' 
#' @section General children allowed:
#' * `ddi_notes()` 
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/valrng.html}{valrng documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/invalrng.html}{invalrng documentation} 
#' 
#' @export
ddi_valrng <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn") 
    attribs <- validate_attributes(attribs, allowed_attribs, "valrng")
  }

  if(check_cardinality(components$content, "item") > 0 & check_cardinality(components$content, "range") == 0) required_children <- "item"
  else if(check_cardinality(components$content, "item") == 0 & check_cardinality(components$content, "range") > 0) required_children <- "range"
  else rddi_err("valrng requires at least one item or at least one range child but cannot include an item and range child")
 
  allowed_children <- c(
    "item",
    "range",
    "key",
    "notes"
  )
  
  build_branch_node(
    "valrng",
    allowed_children = allowed_children,
    required_children = required_children,
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_valrng 
#' @export
ddi_invalrng <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn") 
    attribs <- validate_attributes(attribs, allowed_attribs, "invalrng")
  }

  if(check_cardinality(components$content, "item") > 0 & check_cardinality(components$content, "range") == 0) required_children <- "item"
  else if(check_cardinality(components$content, "item") == 0 & check_cardinality(components$content, "range") > 0) required_children <- "range"
  else rddi_err("valrng requires at least one item or at least one range child but cannot include an item and range child")
 
  allowed_children <- c(
    "item",
    "range",
    "key",
    "notes"
  )

  build_branch_node(
    "invalrng",
    allowed_children = allowed_children,
    required_children = required_children,
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_catgry 
#' @export
ddi_catStat <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "type", "otherType", "URI", "methrefs", "wgtd", "wgt_var", "weight", "sdatrefs")
    attribs <- validate_attributes(attribs, allowed_attribs, "catStat")
    if(!is.null(attribs$type)) check_attribs_in_set(attribs$type, c("freq", "percent", "crosstab", "other"), field = "type (catStat)")
  }

  build_leaf_node(
    "catValu",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_catgry 
#' @export
ddi_catValu <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "catValue")
  }

  build_leaf_node(
    "catValu",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_var 
#' @export
ddi_imputation <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "imputation")
  }

  build_leaf_node(
    "imputation",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_var 
#' @export
ddi_qstn <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", "qstn", "var", "seqNo", "sdatrefs",
                        "responseDomainType", "otherResponseDomainType")
    attribs <- validate_attributes(attribs, allowed_attribs, "var")
  }

  build_leaf_node(
    "qstn",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_var 
#' @export
ddi_sumStat <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs$type)) check_attribs_in_set(attribs$type, c("mean", "medn", "mode", "vald", "invd", "min", "max", "stdev", "other"), field = "type (sumStat")
  
  allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", "wgtd", "wgt-var", "weight",
                      "type", "otherType")

  attribs <- validate_attributes(attribs, allowed_attribs, "sumStat")  

  build_leaf_node(
    "sumStat",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_var 
#' @export
ddi_codInstr <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
    
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "codInstr")
  }

  build_leaf_node(
    "codInstr",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_var 
#' @export
ddi_TotlResp <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "TotlResp")
  }

  build_leaf_node(
    "TotlResp",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_var 
#' @export
ddi_security <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", "date")
    attribs <- validate_attributes(attribs, allowed_attribs, "security")
  }

  build_leaf_node(
    "security",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_var 
#' @export
ddi_embargo <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
    
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", "date", "event", "format")
    attribs <- validate_attributes(attribs, allowed_attribs, "embargo")
  }

  build_leaf_node(
    "embargo",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_var 
#' @export
ddi_respUnit <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "respUnit")
  }

  build_leaf_node(
    "respUnit",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_var 
#' @export
ddi_undocCod <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
 
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "undocCod")
  }

  build_leaf_node(
    "undocCod",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_var 
#' @export
ddi_stdCatgry <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
   
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", "date", "URI")
    attribs <- validate_attributes(attribs, allowed_attribs, "stdCatgry")
  }

  build_leaf_node(
    "stdCatgry",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_var 
#' @export
ddi_varFormat <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", "formatname",
                        "schema", "otherSchema", "type", "category", "otherCategory", "URI") 
    attribs <- validate_attributes(attribs, allowed_attribs, "varFormat")
    if("type" %in% names(attribs)) {
      check_attribs_in_set(vals = attribs$type, attributes = c("numeric", "character"), field = "varFormat(type)")
    }
  }

  build_leaf_node(
    "varFormat",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_derivation 
#' @export
ddi_drvdesc <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn") 
    attribs <- validate_attributes(attribs, allowed_attribs, "drvdesc")
  }

  build_leaf_node(
    "drvdesc",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_derivation 
#' @export
ddi_drvcmd <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "syntax") 
    attribs <- validate_attributes(attribs, allowed_attribs, "drvcmd")
  }

  build_leaf_node(
    "drvcmd",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_valrng 
#' @export
ddi_item <- function(...) {
  browser()
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(check_cardinality(components$content, "item") > 0 & check_cardinality(components$content, "range") == 0) required_children <- "item"
  else if(check_cardinality(components$content, "item") == 0 & check_cardinality(components$content, "range") > 0) required_children <- "range"
  else rddi_err("valrng requires at least one item or at least one range child but cannot include an item and range child")
  
  allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                      "UNITS", "VALUE") 
  attribs <- validate_attributes(attribs, allowed_attribs, "item")

  build_leaf_node(
    "item",
    attribs = attribs
  )
}

#' @rdname ddi_valrng 
#' @export
ddi_range <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                      "UNITS", "min", "minExclusive", "max", "maxExclusive") 
    attribs <- validate_attributes(attribs, allowed_attribs, "range")
  }

  build_leaf_node(
    "range",
    attribs = attribs
  )
}

#' @rdname ddi_valrng
#' @export
ddi_key <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn") 
    attribs <- validate_attributes(attribs, allowed_attribs, "key")
  }

  build_leaf_node(
    "key",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_var 
#' @export
ddi_geomap <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "URI","mapformat", "levelno") 
    attribs <- validate_attributes(attribs, allowed_attribs, "geomap")
  }

  build_leaf_node(
    "geomap",
    attribs = attribs
  )
}

#' @rdname ddi_var 
#' @export
ddi_location <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "StartPos","EndPos", "width", "RecSegNo", "fileid", "locMap") 
    attribs <- validate_attributes(attribs, allowed_attribs, "location")
  }

  build_leaf_node(
    "location",
    attribs = attribs
  )
}

#' @rdname ddi_varGrp
#' @export
ddi_defntn <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn") 
    attribs <- validate_attributes(attribs, allowed_attribs, "defntn")
  }

  build_leaf_node(
    "dfntn",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_mrow 
#' @export
ddi_mi <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                    "varRef") 

  attribs <- validate_attributes(attribs, allowed_attribs, "mi")

  build_leaf_node(
    "mi",
    attribs = attribs,
    content = components$content
  )
}