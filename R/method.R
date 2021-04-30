#' method and its child nodes
#' 
#' This section describes the methodology and processing involved in a data collection. More information on allowed attributes and
#' child nodes can be found below and in hte references.
#' 
#' @param ... Child nodes or attributes. 
#'
#' @section Branch node children allowed:
#' * [ddi_anlyInfo()]
#' * [ddi_codingInstructions()]
#' * [ddi_dataColl()]
#' 
#' @section General children allowed:
#' * `ddi_notes()` 
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/method.html}{method documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataProcessing.html}{dataProcessing documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/stdyClas.html}{stdyClas documentation}
#' 
#' @export
ddi_method <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "method")
  }

  allowed_children <- c(
    "dataColl",
    "notes",
    "anlyInfo", 
    "stdyClas",
    "dataProcessing",
    "codingInstructions"
  )

  if(check_cardinality(components$content, "anlyInfo") > 1) rddi_err("Only 0 or 1 anlyInfo child is allowed in method")


  build_branch_node(
    "method",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' dataColl and its children
#' 
#' Information about the data collection methodology employed in the codebook. More information on dataColl, its attributes, and 
#' its allowed children can be found below and in the references.
#' 
#' @param ... Child nodes or attributes. 
#' 
#' @section Branch node children allowed:
#' * [ddi_sampleFrame()]
#' * [ddi_sources()]
#' * [ddi_targetSampleSize()]
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataColl.html}{dataColl documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/actMin.html}{actMin documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/cleanOps.html}{cleanOps documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/collectorTraining.html}{collectorTraining documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/collMode.html}{collMode documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/collSitu.html}{collSitu documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/ConOps.html}{ConOps documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataCollector.html}{dataCollector documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/deviat.html}{deviat documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/frequenc.html}{frequenc documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/instrumentDevelopment.html}{instrumentDevelopment documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/resInstru.html}{resInstru documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/sampProc.html}{sampProc documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/timeMeth.html}{timeMeth documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/weight.html}{weight documentation}
#'  
#' @export
ddi_dataColl <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "dataColl")
  }

  allowed_children <- c(
    "timeMeth",
    "dataCollector",
    "collectorTraining", 
    "frequenc",
    "sampProc",
    "sampleFrame",
    "targetSampleSize",
    "deviat",
    "collMode",
    "resInstru",
    "instrumentDevelopment",
    "sources",
    "collSitu",
    "actMin",
    "ConOps",
    "weight",
    "cleanOps"
  )

  if(check_cardinality(components$content, "sources") > 1) rddi_err("Only 0 or 1 sources child is allowed in dataColl")

  build_branch_node(
    "dataColl",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_dataColl
#' @export
ddi_actMin <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "actMin")
  }

  build_leaf_node(
    "actMin",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_dataColl
#' @export
ddi_cleanOps <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "agency")
    attribs <- validate_attributes(attribs, allowed_attribs, "cleanOps")
  }

  build_leaf_node(
    "cleanOps",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_dataColl
#' @export
ddi_collectorTraining <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "type")
    attribs <- validate_attributes(attribs, allowed_attribs, "collectorTraining")
  }

  build_leaf_node(
    "collectorTraining",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_dataColl
#' @export
ddi_collMode <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "collMode")
  }

  build_leaf_node(
    "collMode",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_dataColl
#' @export
ddi_collSitu <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "collSitu")
  }

  build_leaf_node(
    "collSitu",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_dataColl
#' @export
ddi_ConOps <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "agency")
    attribs <- validate_attributes(attribs, allowed_attribs, "ConOps")
  }

  build_leaf_node(
    "ConOps",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_dataColl
#' @export
ddi_dataCollector <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "abbr", "affiliation", "role")
    attribs <- validate_attributes(attribs, allowed_attribs, "dataCollector")
  }

  build_leaf_node(
    "dataCollector",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_dataColl
#' @export
ddi_deviat <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "deviat")
  }

  build_leaf_node(
    "deviat",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_dataColl
#' @export
ddi_frequenc <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                      "freq")
    attribs <- validate_attributes(attribs, allowed_attribs, "frequenc")
  }

  build_leaf_node(
    "frequenc",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_dataColl
#' @export
ddi_instrumentDevelopment <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                      "type")
    attribs <- validate_attributes(attribs, allowed_attribs, "instrumentDevelopment")
  }

  build_leaf_node(
    "instrumentDevelopment",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_dataColl
#' @export
ddi_resInstru <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                      "type")
    attribs <- validate_attributes(attribs, allowed_attribs, "resInstru")
  }

  build_leaf_node(
    "resInstru",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_dataColl
#' @export
ddi_sampProc <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "sampProc")
  }

  build_leaf_node(
    "sampProc",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_dataColl
#' @export
ddi_timeMeth <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "method")
    attribs <- validate_attributes(attribs, allowed_attribs, "timeMeth")
  }

  build_leaf_node(
    "timeMeth",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_dataColl
#' @export
ddi_weight <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "weight")
  }

  build_leaf_node(
    "weight",
    attribs = attribs,
    content = components$content
  )
}

#' sampleFrame and its children
#' 
#' Sample frame describes the sampling frame used for identifying the population from which the sample was taken. 
#' More information on sampleFrame, its leaf and branch nodes, and their attributes can be found below and in the 
#' references.
#' 
#' @param ... Child nodes or attributes. 
#'
#' @section Branch node children allowed:
#' * [ddi_frameUnit()]
#' 
#' @section General children allowed:
#' * `ddi_labl()`
#' * `ddi_txt()`
#' * `ddi_universe()`
#' * `ddi_useStmt()` 
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/sampleFrame.html}{sampleFrame documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/custodian.html}{custodian documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/referencePeriod.html}{referencePeriod documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/sampleFrameName.html}{sampleFrameName documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/updateProcedure.html}{updateProcedure documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/referencePeriod.html}{referencePeriod documentation}
#' 
#' @export
ddi_sampleFrame <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "sampleFrame")
  }

  allowed_children <- c(
    "sampleFrameName",
    "labl",
    "txt", 
    "validPeriod",
    "custodian",
    "useStmt",
    "universe",
    "frameUnit",
    "referncePeriod",
    "updateProcedure"
  )

  build_branch_node(
    "sampleFrame",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_sampleFrame
#' @export
ddi_custodian <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "abbr")
    attribs <- validate_attributes(attribs, allowed_attribs, "custodian")
  }

  build_leaf_node(
    "custodian",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_sampleFrame
#' @export
ddi_referencePeriod <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "event")
    attribs <- validate_attributes(attribs, allowed_attribs, "referencePeriod")
    check_attribs_in_set(attribs$event, c("start", "end", "single"), field = "event (referencePeriod")
  }

  check_content_date(components$content) 

  build_leaf_node(
    "referencePeriod",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_sampleFrame
#' @export
ddi_sampleFrameName <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "sampleFrameName")
  }

  build_leaf_node(
    "sampleFrameName",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_sampleFrame
#' @export
ddi_updateProcedure <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "updateProcedure")
  }

  build_leaf_node(
    "updateProcedure",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_sampleFrame
#' @export
ddi_validPeriod <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "event")
    attribs <- validate_attributes(attribs, allowed_attribs, "validPeriod")
    check_attribs_in_set(attribs$event, c("start", "end", "single"), field = "event (validPeriod")
  }

  check_content_date(components$content) 

  build_leaf_node(
    "validPeriod",
    attribs = attribs,
    content = components$content
  )
}

#' frameUnit and its children
#' 
#' Provides information about the sampling frame unit. More information on frameUnit, its children nodes, and attributes can be found
#' below and in the references
#' 
#' @param ... Child nodes or attributes. 
#'
#' @section General children allowed:
#' * `ddi_txt()`
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/frameUnit.html}{frameUnit documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/unitType.html}{unitType documentation}
#' 
#' @export
ddi_frameUnit <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "isPrimary")
    attribs <- validate_attributes(attribs, allowed_attribs, "frameUnit")
  }

  allowed_children <- c(
    "txt",
    "unitType"
  )

  build_branch_node(
    "frameUnit",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_frameUnit 
#' @export
ddi_unitType <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "numberOfUnits")
    attribs <- validate_attributes(attribs, allowed_attribs, "unitType")
  }

  build_leaf_node(
    "unitType",
    attribs = attribs,
    content = components$content
  )
}


#' targetSampleSize and its children nodes
#' 
#' Provides both the target size of the sample (this is the number in the original sample, not the number of respondents) as well as the formula used for determining the sample size.
#' More information on the allowed attributes and children can be found in the references
#' 
#' @param ... Child nodes or attributes. 
#'
#' @section DDI Codebook 2.5 Documentation:
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/targetSampleSize.html}{targetSampleSize documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/sampleSize.html}{sampleSize documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/sampleSizeFormula.html}{sampleSizeFormula documentation}
#' 
#' @export
ddi_targetSampleSize <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "targetSampleSize")
  }

  allowed_children <- c(
    "sampleSize",
    "sampleSizeFormula"
  )

  if(check_cardinality(components$content, "sampleSize") > 1) rddi_err("Only 0 or 1 simpleSize child is allowed in targetSampleSize")

  build_branch_node(
    "targetSampleSize",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_targetSampleSize
#' @export
ddi_sampleSize <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "sampleSize")
  }

  if(!is.integer(components$content)) rddi_err("sampleSize must be an integer")

  build_leaf_node(
    "sampleSize",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_targetSampleSize
#' @export
ddi_sampleSizeFormula <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "sampleSizeFormula")
  }

  if(!is.character(components$content)) rddi_err("sampleSizeFormula must be a character string")

  build_leaf_node(
    "sampleSizeFormula",
    attribs = attribs,
    content = components$content
  )
}

#' Sources Statement
#' 
#' Description of sources used for the data collection. The element is nestable so that the sources statement might encompass a series of discrete source 
#' statements, each of which could contain the facts about an individual source. This element maps to Dublin Core Source element.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section Branch node children allowed:
#' * [ddi_sourceCitation()]
#' 
#' @section General children allowed:
#' * `ddi_dataSrc()`
#' * `ddi_srcOrig()` 
#' * `ddi_srcChar()` 
#' * `ddi_srcDocu()`
#' 
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/sources.html}
#' 
#' @export
ddi_sources <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "sources")
  }

  allowed_children <- c(
    "dataSrc",
    "sourceCitation",
    "srcOrig",
    "srcChar",
    "srcDocu",
    "sources"
  )

  build_branch_node(
    "sources",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}


#' anlyInfo and its child nodes
#' 
#' Information on data appraisal.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/anlyInfo.html}{anylInfo documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataAppr.html}{dataAppr documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/respRate.html}{respRate documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/EstSmpErr.html}{EstSmpErr documentation}
#' 
#' @export
ddi_anlyInfo <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "anlyInfo")
  }

  allowed_children <- c(
    "respRate",
    "EstSmpErr",
    "dataAppr"
  )

  build_branch_node(
    "anlyInfo",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_anlyInfo
#' @export
ddi_dataAppr <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "type")
    attribs <- validate_attributes(attribs, allowed_attribs, "dataAppr")
  }

  build_leaf_node(
    "dataAppr",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_anlyInfo
#' @export
ddi_EstSmpErr <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "EstSmpErr")
  }

  build_leaf_node(
    "EstSmpErr",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_anlyInfo
#' @export
ddi_respRate <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "respRate")
  }

  build_leaf_node(
    "respRate",
    attribs = attribs,
    content = components$content
  )
}

#' codingInstructions and its child nodes
#' 
#' Describe specific coding instructions used in data processing, cleaning, assession, or tabulation. 
#' More information on codingInstructions allowed attributes and children can be found below and in the references.
#' 
#' @param ... Child nodes or attributes. 
#'
#' @section General children allowed:
#' * `ddi_txt()`
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/codingInstructions.html}{codingInstructions documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/command.html}{command documentation}
#' 
#' @export
ddi_codingInstructions <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "type", "relatedProcesses")
    attribs <- validate_attributes(attribs, allowed_attribs, "codingInstructions")
  }

  allowed_children <- c(
    "command",
    "txt"
  )

  build_branch_node(
    "codingInstructions",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_codingInstructions
#' @export
ddi_command <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                          "formalLanguage")
    attribs <- validate_attributes(attribs, allowed_attribs, "command")
  }

  build_leaf_node(
    "command",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_method 
#' @export
ddi_dataProcessing <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                          "type")
    attribs <- validate_attributes(attribs, allowed_attribs, "dataProcessing")
  }

  build_leaf_node(
    "dataProcessing",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_method
#' @export
ddi_stdyClas <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                          "type")
    attribs <- validate_attributes(attribs, allowed_attribs, "stdyClas")
  }

  build_leaf_node(
    "stdyClas",
    attribs = attribs,
    content = components$content
  )
}