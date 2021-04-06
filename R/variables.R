#' DDI variable specification
#'
#' Variables are specified with the var tag, which has a litany of 
#' attributes. The basic necessities of a variable, however, are given by its name, and type. 
#' If the variable is categorical, then the variable will have as many 
#' catgry (`ddi_catgry()`) entries as category levels. Missingness codes are specified in `ddi_catgry()`.
#'
#' @section DDI Codebook 2.5 Documentation:
#'
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/var.html}
#'
#' @param varname The variable name
#' @param ... Any other parameters from the DDI Codebook 2.5 schema
#'
#' @export
ddi_var <- function(varname, database_id = NULL, ...) {
  stopifnot(is.character(varname))
  stopifnot(length(varname) == 1)

  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  attribs$name <- varname

  if (!is.null(database_id)) { # optional in DDI
    attribs$files <- database_id
  }

  allowed_attribs <- c(
    "ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", "name", "wgt", "wgt-var", "weight",
    "qstn", "files", "vendor", "dcml", "intrvl", "rectype", "sdatrefs", "methrefs", "pubrefs", "access", "aggrMeth", "otherAggrMeth", "measUnit", 
    "scale", "origin", "nature", "temporal", "geog", "geoVocab", "catQnty", "representationType", "otherRepresentationType"
  ) 

  check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "var")

  check_attribs(attribs = attribs)

  allowed_children <- c(
    "catgry", "catLevel", "labl", "qstn", "sumStat", "notes", "analysUnit", "imputation", "txt",
    "codInstr", "TotlResp", "security", "embargo", "respUnit", "undocCod", "stdCatgry", "concept", "varFormat",
    "valrng", "invalrng", "universe", "catgryGrp", "verStmt", "derivation", "geomap", "location"
  )

  # checking cardinality of derivation & varFormat
   
  build_branch_node(
    "var",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' DDI varGrp specification
#'
#' A group of variables that may share a common subject, arise from the interpretation of a single question, or are linked by some other factor.
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#'
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/varGrp.html}
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
    check_attribs_in_set(attributes = names(attribs), vals = allowed_attribs, field = "varGrp")
    check_attribs(attribs)
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

#' DDI catLevel specification
#'
#' Used to describe the levels of the category hierarchy. This field has no content.
#' 
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/catLevel.html} 
#' 
#' @param level_name A category level name for hierarchical categorical variables (e.g. ordered categories)
#' @param ... Any other parameters from the DDI Codebook 2.5 schema
#'
#' @export
ddi_catLevel <- function(level_name, ...) {
  stopifnot(is.character(level_name))

  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  attribs$levelnm <- level_name # optional in DDI

  if (!is.element("ID", names(attribs))) {
    rddi_err("Category levels must have an ID. Use the `id_object` parameter to set the ID.")
  }

  allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleURN", "ddiCodebookURN", "levelnm", "geoMap")
  
  check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "catLevel")
  check_attribs(attribs = attribs)

  build_leaf_node(
    "catLevel",
    attribs = attribs 
  )
}

#' DDI catgry specification
#'
#' A description of a particluar categorical response
#'  
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/catgry.html}
#' 
#' @param missing If a category represents a missing response. Default to `FALSE`
#' @param ... Any other parameters from the DDI Codebook 2.5 schema
#' 
#' @export
ddi_catgry <- function(missing = FALSE, ...)  {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  attribs$missing <- if (missing) "Y" else "N" # optional in DDI 2.5
  
  allowed_children <- c(
    "catStat",
    "catValu", # allowed once
    "labl",
    "txt",
    "mrow" # allowed once
  )
  
  allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", "missing", "missType", "country",
                      "sdatrefs", "catgry", "level", "excls")

  check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "catgry")
  check_attribs(attribs = attribs)

  build_branch_node(
    "catgry",
    allowed_children = allowed_children,
    attribs = components$attribs,
    content = components$content
  )
}

# WHAT'S THE DIFFERENCE BETWEEN THE BELOW AND THE ABOVE FUNCTIONS?
#ddi_catgry <- function(..., missing = FALSE) {
#  components <- dots_to_xml_components(...)
#  attribs <- components$attribs
#
#  attribs$missing <- if (missing) "Y" else "N"
#
#  allowed_children <- c(
#    "catStat",
#    "catValu",
#    "labl"
#  )
#
#  build_branch_node(
#    "catgry",
#    allowed_children = allowed_children,
#    attribs = attribs,
#    content = components$content
#  )
#}

#' DDI mrow specification
#'
#' mrow or Mathematical Row is a wrapper containing the presentation expression mi (ddi_mi()). It creates a single string 
#' without spaces consisting of the individual elements described within it. It can be used to create a single 
#' variable by concatenating other variables into a single string. It is used to create linking variables composed
#' of multiple non-contiguous parts, or to define unique strings for various category values of a single variable.
#' 
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/mrow.html}
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#' 
#' @export
ddi_mrow <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn") 
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "mrow")
    check_attribs(attribs = attribs)
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

#' DDI universe specification
#'
#' The group of persons or other elements that are the object of research and to which any analytic results refer. Age,nationality, and residence 
#' commonly help to delineate a given universe, but any of a number of factors may be involved, such as sex, race, income, veteran status, criminal convictions, etc. 
#' The universe may consist of elements other than persons, such as housing units, court cases, deaths, countries, etc. In general, it should be possible to tell from 
#' the description of the universe whether a given individual or element (hypothetical or real) is a member of the population under study.
#' 
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/universe.html}
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#' 
#' @export
ddi_universe <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", "level", "clusion") 
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "universe")
    check_attribs(attribs = attribs)
  }

  allowed_children <- c(
    "concept",
    "txt"
  )

  build_branch_node(
    "universe",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' DDI Analysis Unit specification
#'
#' Provides information regarding whom or what the variable/nCube describes. The element may be repeated only to support multiple language expressions of the content.
#' 
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/anlysUnit.html}
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#' 
#' @export
ddi_anlysUnit <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "anlysUnit")
    check_attribs(attribs = attribs)
  }

  allowed_children <- c(
    "concept",
    "txt"
  )

  build_branch_node(
    "anlysUnit",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' DDI Category Group specification
#'
#' A description of response categories that might be grouped together. 
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/catgryGrp.html}
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#' 
#' @export
ddi_catgryGrp <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", 
                        "missing", "missType", "catgry", "catGrp", "levelno", "levelnm", "compl", "excls") 
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "catgryGrp")
    check_attribs(attribs = attribs)
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

#' DDI Version Statement specification
#' 
#' This is the version statement for the work at the appropriate level: marked-up document; marked-up document source; study; study description, other material; 
#' other material for study. A version statement may also be included for a data file, a variable, or an nCube.
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/verStmt.html}
#'  
#' @export
ddi_verStmt <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn") 
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "verStmt")
    check_attribs(attribs = attribs)
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

#' DDI Derivation specification
#' 
#' Used only in the case of a derived variable, this element provides both a description of how the derivation was performed and the command used to generate 
#' the derived variable, as well as a specification of the other variables in the study used to generate the derivation. The "var" attribute provides the ID values of the 
#' other variables in the study used to generate this derived variable.
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/derivation.html}
#'  
#' @export
ddi_derivation <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                      "var") 
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "derviation")
    check_attribs(attribs = attribs)
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

#' DDI Range of Valid Data Values Specification
#' 
#' Values for a particular variable that represent legitimate responses. Must include item or range as a child element
#' 
#' @param ... Any other parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/valrng.html}
#'  
#' @export
ddi_valrng <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn") 
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "valrng")
    check_attribs(attribs = attribs)
  }

  # NEED TO CHECK THAT item OR range is included
  # if item or range in components$content {
  #   if(item) {
  #     required_children <- c("item")
  #   else required_children <- c("range")
  # } else {
  #  rddi_err message
  # }
  
  allowed_children <- c(
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

#' DDI Range of Invalid Data Values Specification
#' 
#' Values for a particular variable that represent missing data, not applicable responses, etc. Must include item or range as a child element
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/invalrng.html}
#'  
#' @export
ddi_invalrng <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn") 
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "invalrng")
    check_attribs(attribs = attribs)
  }

  # NEED TO CHECK THAT item OR range is included
  # if item or range in components$content {
  #   if(item) {
  #     required_children <- c("item")
  #   else required_children <- c("range")
  # } else {
  #  rddi_err message
  # }
  
  allowed_children <- c(
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

#' DDI Category Value spcification
#' 
#' The value of a specific category
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/catValu.html}
#'  
#' @export
ddi_catValu <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "catValu")
    check_attribs(attribs = attribs)
  }

  build_leaf_node(
    "catValu",
    attribs = attribs,
    content = components$content
  )
}

#' DDI Label specification
#' 
#' A short description of the parent element. In the variable label, the length of this phrase may depend on the statistical analysis system 
#' used (e.g., some versions of SAS permit 40-character labels, while some versions of SPSS permit 120 characters), although the DDI itself imposes no restrictions 
#' on the number of characters allowed.
#'
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/labl.html}
#'  
#' @export
ddi_labl <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
 
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", "level", "vendor", "country", "sdatrefs")
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "labl")
    check_attribs(attribs = attribs)
  }

  build_leaf_node (
    "labl",
    attribs = attribs,
    content = components$content
  )
}

#' DDI Notes and Comments specification
#' 
#' For clarifying information/annotation regarding the parent element
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/notes.html}
#'  
#' @export
ddi_notes <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", "type", "subject", "level", "resp",
                        "sdatrefs", "parent", "sameNote")
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "notes")
    check_attribs(attribs = attribs)
  }

  build_leaf_node (
    "notes",
    attribs = attribs,
    content = components$content
  )
}

#' DDI Descriptive Text specification
#' 
#' Lengthier description of the parent element
#'
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/txt.html}
#'  
#' @export
ddi_txt <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", "level", "sdatrefs")
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "txt")
    check_attribs(attribs = attribs)
  }

  build_leaf_node(
    "txt",
    attribs = attribs,
    content = components$content 
  )
}

#' DDI Imputation specification
#' 
#' According to the Statistical Terminology glossary maintained by the National Science Foundation, this is "the process by which one estimates missing values 
#' for items that a survey respondent failed to provide," and if applicable in this context, it refers to the type of procedure used. 
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/imputation.html}
#'  
#' @export
ddi_imputation <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "imputation")
    check_attribs(attribs = attribs)
  }

  build_leaf_node(
    "imputation",
    attribs = attribs,
    content = components$content
  )
}

#' DDI Question specification
#' 
#' The question element may have mixed content. The element itself may contain text for the question, with the subelements being used to provide further information 
#' about the question. Alternatively, the question element may be empty and only the subelements used. The element has a unique question ID attribute which can be used to link 
#' a variable with other variables where the same question has been asked. This would allow searching for all variables that share the same question ID, perhaps because the questions 
#' was asked several times in a panel design.
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/qstn.html}
#'  
#' @export
ddi_qstn <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", "qstn", "var", "seqNo", "sdatrefs",
                        "responseDomainType", "otherResponseDomainType")
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "qstn")
    check_attribs(attribs = attribs)
  }

  build_leaf_node(
    "qstn",
    attribs = attribs,
    content = components$content
  )
}

#' DDI Summary Statistics specification
#' 
#' One or more statistical measures that describe the responses to a particular variable and may include one or more standard summaries, e.g., minimum and 
#' maximum values, median, mode, etc. This element sumStat requires the type attribute with the value of mean, medn, mode, vald, invd, min, max, stdev, or other.
#' 
#' @param ... Any other parameters from the DDI Codebook 2.5 schema
#' @param type The statistics being shown:mean, median, mode, valid cases, invalid cases, minimum, maximum, or standard deviation. If a value of "other" is used here, a value taken from a controlled vocabulary should be put in the "otherType" attribute. This option should only be used when applying a controlled vocabulary to this attribute. 
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/sumStat.html}
#'  
#' @export
ddi_sumStat <- function(type, ...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  attribs$type <- type
  check_attribs_in_set(attribs$type, c("mean", "medn", "mode", "vald", "invd", "min", "max", "stdev", "other"), field = "type (sumStat")
  
  allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", "wgtd", "wgt-var", "weight",
                      "type", "otherType")

  check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "sumStat")
  check_attribs(attribs = attribs)
  

  build_leaf_node(
    "sumStat",
    attribs = attribs,
    content = components$content
  )
}

#' DDI Coder Instructions specification
#' 
#' One or more statistical measures that describe the responses to a particular variable and may include one or more standard summaries, e.g., minimum and 
#' maximum values, median, mode, etc.
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/codInstr.html}
#'  
#' @export
ddi_codInstr <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
    
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "codInstr")
    check_attribs(attribs = attribs)
  }

  build_leaf_node(
    "codInstr",
    attribs = attribs,
    content = components$content
  )
}

#' DDI Total Responses specifications
#' The number of responses to this variable. This element might be used if the number of responses does not match added case counts. It may also be used to sum the 
#' frequencies for variable categories.
#' 
#' @param ... Any other parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/TotlResp.html}
#'  
#' @export
ddi_TotlResp <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "TotlResp")
    check_attribs(attribs = attribs)
  }

  build_leaf_node(
    "TotlResp",
    attribs = attribs,
    content = components$content
  )
}

#' DDI Security specification
#' 
#' Provides information regarding levels of access, e.g., public, subscriber, need to know. The ISO standard for dates (YYYY-MM-DD) is recommended for use with the 
#' date attribute.
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/security.html}
#'  
#' @export
ddi_security <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", "date")
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "security")
    check_attribs(attribs = attribs)
  }
  build_leaf_node(
    "security",
    attribs = attribs,
    content = components$content
  )
}

#' DDI Embargo specification
#' 
#' Provides information on variables/nCubes which are not currently available because of policies established by the principal investigators and/or data producers. 
#' The ISO standard for dates (YYYY-MM-DD) is recommended for use with the "date" attribute. An "event" attribute is provided to specify "notBefore" or "notAfter" ("notBefore" is 
#' the default). A "format" attribute is provided to ensure that this information will be machine-processable, and specifies a format for the embargo element.
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/embargo.html}
#'  
#' @export
ddi_embargo <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
    
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", "date", "event", "format")
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "embargo")
    check_attribs(attribs = attribs)
  }

  build_leaf_node(
    "embargo",
    attribs = attribs,
    content = components$content
  )
}

#' DDI Response Unit specification
#' 
#' Provides information regarding who provided the information contained within the variable/nCube, e.g., respondent, proxy, interviewer. This element may be repeated 
#' only to support multiple language expressions of the content.
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/respUnit.html}
#'  
#' @export
ddi_respUnit <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "respUnit")
    check_attribs(attribs = attribs)
  }

  build_leaf_node(
    "respUnit",
    attribs = attribs,
    content = components$content
  )
}

#' DDI List of Undocumented Codes specification
#' 
#' Values whose meaning is unknown
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/undocCod.html}
#'  
#' @export
ddi_undocCod <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
 
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "undocCod")
    check_attribs(attribs = attribs)
  }

  build_leaf_node(
    "undocCod",
    attribs = attribs,
    content = components$content
  )
}

#' DDI Standard Categories specification
#' 
#' Standard category codes used in the variable, like industry codes, employment codes, or social class codes
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/stdCatgry.html}
#'  
#' @export
ddi_stdCatgry <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
   
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", "date", "URI")
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "stdCatgry")
    check_attribs(attribs = attribs)
  }

  build_leaf_node(
    "stdCatgry",
    attribs = attribs,
    content = components$content
  )
}

#' DDI Concept specification
#' 
#' The general subject to which the parent element may be seen as pertaining. This element serves the same purpose as the keywords and topic classification elements, 
#' but at the data description level. The "vocab" attribute is provided to indicate the controlled vocabulary, if any, used in the element, e.g., LCSH (Library of Congress Subject 
#' Headings), MeSH (Medical Subject Headings), etc. The "vocabURI" attribute specifies the location for the full controlled vocabulary.
#' 
#' @param ... Any other parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/concept.html}
#'  
#' @export
ddi_concept <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", "vocab", "vocabURI")
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "concept")
    check_attribs(attribs = attribs)
  }

  build_leaf_node(
    "concept",
    attribs = attribs,
    content = components$content
  )
}

#' DDI Variable Format specification
#' 
#' The technical format of the variable in question. ttributes for this element include: "type," which indicates if the variable is character or numeric; 
#' "formatname," which in some cases may provide the name of the particular, proprietary format actually used; "schema," which identifies the vendor or standards 
#' body that defined the format (acceptable choices are SAS, SPSS, IBM, ANSI, ISO, XML-data or other); "category," which describes what kind of data the format 
#' represents, and includes date, time, currency, or "other" conceptual possibilities; and "URI," which supplies a network identifier for the format definition.
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/varFormat.html}
#'  
#' @export
ddi_varFormat <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", "formatname",
                        "schema", "otherSchema", "type", "category", "otherCategory", "URI") 
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "varFormat")
    if("type" %in% names(attribs)) {
      check_attribs_in_set(vals = attribs$type, attributes = c("numeric", "character"), field = "varFormat(type)")
    }  
    check_attribs(attribs = attribs)
  }

  build_leaf_node(
    "varFormat",
    attribs = attribs,
    content = components$content
  )
}

#' DDI Version specification
#' Also known as release or edition. If there have been substantive changes in the data/documentation since their creation, this statement should be 
#' used at the appropriate level. 
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/version.html}
#'  
#' @export
ddi_version <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "date", "type") 
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "version")
    check_attribs(attribs = attribs)
  }

  build_leaf_node(
    "version",
    attribs = attribs,
    content = components$content
  )
}

#' DDI Version Responsibility Statement specification
#' 
#' The organization or person responsible for the version of the work.
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/verResp.html}
#'  
#' @export
ddi_verResp <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                      "affiliation") 
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "verResp")
    check_attribs(attribs = attribs)
  }

  build_leaf_node(
    "verResp",
    attribs = attribs,
    content = components$content
  )
}

#' DDI Derivation Description specification
#' 
#' A textual description of the way in which this variable was derived. The element may be repeated to support multiple language expressions of the content.
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/drvdesc.html}
#'  
#' @export
ddi_drvdesc <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn") 
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "drvdesc")
    check_attribs(attribs = attribs)
  }

  build_leaf_node(
    "drvdesc",
    attribs = attribs,
    content = components$content
  )
}

#' DDI Derivation Command specification
#' 
#' The actual command used to generate the derived variable. The "syntax" attribute is used to indicate the command language employed (e.g., SPSS, SAS, Fortran, etc.). 
#' The element may be repeated to support multiple language expressions of the content.
#' 
#' @param ... Any other parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/drvcmd.html}
#'  
#' @export
ddi_drvcmd <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "syntax") 
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "drvcmd")
    check_attribs(attribs = attribs)
  }

  build_leaf_node(
    "drvcmd",
    attribs = attribs,
    content = components$content
  )
}

#' DDI Value Item specification
#' 
#' The counterpart to Range; used to encode individual values. This is an empty element consisting only of its attributes. The "UNITS" attribute permits the 
#' specification of integer/real numbers. The "VALUE" attribute is required specifies the actual value.
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/item.html}
#'  
#' @export
ddi_item <- function(value, ...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  attribs$VALUE <- value
  
  allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                      "UNITS", "VALUE") 

  check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "item")

  check_attribs(attribs = attribs)

  build_leaf_node(
    "item",
    attribs = attribs
  )
}

#' DDI Value Range specification
#' 
#' This is the actual range of values. The "UNITS" attribute permits the specification of integer/real numbers. The "min" and "max" attributes specify the lowest 
#' and highest values that are part of the range. The "minExclusive" and "maxExclusive" attributes specify values that are immediately outside the range. This is an empty element 
#' consisting only of its attributes.
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/range.html}
#'  
#' @export
ddi_range <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
      allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                      "UNITS", "min", "minExclusive", "max", "maxExclusive") 
      check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "range")
      check_attribs(attribs = attribs)
  }

  build_leaf_node(
    "range",
    attribs = attribs
  )
}

#' DDI Range Key specification
#' 
#' This element permits a listing of the category values and labels. While this information is coded separately in the Category element, there may be some value in 
#' having this information in proximity to the range of valid and invalid values. 
#' 
#' @param ... Any other parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/key.html}
#'  
#' @export
ddi_key <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn") 
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "key")
    check_attribs(attribs = attribs)
  }

  build_leaf_node(
    "key",
    attribs = attribs,
    content = components$content
  )
}

#' DDI Geographic Map specification
#' 
#' This element is used to point, using a "URI" attribute, to an external map that displays the geography in question. The "levelno" attribute indicates the level 
#' of the geographic hierarchy relayed in the map. The "mapformat" attribute indicates the format of the map. This is an empty element consisting only of attributes
#' 
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/geoMap.html}
#'  
#' @export
ddi_geomap <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "URI","mapformat", "levelno") 
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "geomap")
    check_attribs(attribs = attribs)
  }

  build_leaf_node(
    "geomap",
    attribs = attribs
  )
}

#' DDI Location specification
#' 
#' This is an empty element containing only attributes. Attributes include "StartPos" (starting position of variable), "EndPos" (ending position of variable), "width" (number 
#' of columns the variable occupies), "RecSegNo" (the record segment number, deck or card number the variable is located on), and "fileid", an IDREF link to the fileDscr element 
#' for the file that this location is within (this is necessary in cases where the same variable may be coded in two different files, e.g., a logical record length type file and 
#' a card image type file). Note that if there is no width or ending position, then the starting position should be the ordinal position in the file, and the file would be 
#' described as free-format. The attribute "locMap" is an IDREF to the element locMap and serves as a pointer to indicate that the location information for the nCube's 
#' cells (aggregate data) is located in that section. 
#' 
#' @param ... Any other parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/location.html}
#'  
#' @export
ddi_location <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "StartPos","EndPos", "width", "RecSegNo", "fileid", "locMap") 
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "location")
    check_attribs(attribs = attribs)
  }

  build_leaf_node(
    "location",
    attribs = attribs
  )
}

#' DDI Definition specification
#' 
#' Rationale for why the group was constituted in this way.
#'  
#' @param ... Any parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/defntn.html}
#'  
#' @export
ddi_defntn <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn") 
    check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "dfntn")
    check_attribs(attribs = attribs)
  }

  build_leaf_node(
    "dfntn",
    attribs = attribs,
    content = components$content
  )
}

#' DDI Mathematical Identifier specification
#' 
#' Token element containing the smallest unit in the Mathematical Row (`mrow()`) that carries meaning.
#' 
#' @param varRef A required attribute consisting of a variable's IDREF
#' @param ... Any other parameters from the DDI Codebook 2.5 schema
#'
#' @section DDI Codebook 2.5 Documentation:
#' 
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/mi.html}
#'  
#' @export
ddi_mi <- function(varRef, ...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  attribs$varRef <- varRef
  
  allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                    "varRef") 

  check_attribs_in_set(attributes = allowed_attribs, vals = names(attribs), field = "mi")

  check_attribs(attribs = attribs)

  build_leaf_node(
    "mi",
    attribs = attribs,
    content = components$content
  )
}