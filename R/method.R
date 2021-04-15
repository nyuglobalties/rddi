#' Methodology and Processing
#' 
#' This section describes the methodology and processing involved in a data collection.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/method.html}
#' 
#' @export
ddi_method <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "method")
    check_attribs(attribs)
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

#' Data Collection Methodology
#' 
#' Information about the methodology employed in a data collection.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataColl.html}
#' 
#' @export
ddi_dataColl <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "dataColl")
    check_attribs(attribs)
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

#' Actions to Minimize Losses
#' 
#' Summary of actions taken to minimize data loss. Includes information on actions such as follow-up visits, supervisory checks, historical matching, estimation, etc.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/actMin.html}
#' 
#' @export
ddi_actMin <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "actMin")
    check_attribs(attribs)
  }

  build_leaf_node(
    "actMin",
    attribs = attribs,
    content = components$content
  )
}

#' Cleaning Operations
#' 
#' Methods used to "clean" the data collection, e.g., consistency checking, wild code checking, etc. The "agency" attribute permits specification of the 
#' agency doing the data cleaning.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/cleanOps.html}
#' 
#' @export
ddi_cleanOps <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "agency")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "cleanOps")
    check_attribs(attribs)
  }

  build_leaf_node(
    "cleanOps",
    attribs = attribs,
    content = components$content
  )
}

#' Collector Training
#' 
#' Describes the training provided to data collectors including internviewer training, process testing, compliance with standards etc. This is repeatable for language 
#' and to capture different aspects of the training process. The type attribute allows specification of the type of training being described.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/collectorTraining.html}
#' 
#' @export
ddi_collectorTraining <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "type")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "collectorTraining")
    check_attribs(attribs)
  }

  build_leaf_node(
    "collectorTraining",
    attribs = attribs,
    content = components$content
  )
}

#' Mode of Data Collection
#' 
#' The method used to collect the data; instrumentation characteristics.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/collMode.html}
#' 
#' @export
ddi_collMode <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "collMode")
    check_attribs(attribs)
  }

  build_leaf_node(
    "collMode",
    attribs = attribs,
    content = components$content
  )
}

#' Characteristics of Data Collection Situation
#' 
#' Description of noteworthy aspects of the data collection situation. Includes information on factors such as cooperativeness of respondents, duration of interviews, number of call-backs, etc.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/collSitu.html}
#' 
#' @export
ddi_collSitu <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "collSitu")
    check_attribs(attribs)
  }

  build_leaf_node(
    "collSitu",
    attribs = attribs,
    content = components$content
  )
}

#' Control Operations
#' 
#' Methods to facilitate data control performed by the primary investigator or by the data archive. Specify any special programs used for such operations. The "agency" 
#' attribute maybe used to refer to the agency that performed the control operation.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/ConOps.html}
#' 
#' @export
ddi_ConOps <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "agency")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "ConOps")
    check_attribs(attribs)
  }

  build_leaf_node(
    "ConOps",
    attribs = attribs,
    content = components$content
  )
}

#' Data Collector
#' 
#' The entity (individual, agency, or institution) responsible for administering the questionnaire or interview or compiling the data. This refers to the entity collecting 
#' the data, not to the entity producing the documentation. Attribute "abbr" may be used to list common abbreviations given to agencies, etc. Attribute "affiliation" may be 
#' used to record affiliation of the data collector. The role attribute specifies the role of person in the data collection process.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataCollector.html}
#' 
#' @export
ddi_dataCollector <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "abbr", "affiliation", "role")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "dataCollector")
    check_attribs(attribs)
  }

  build_leaf_node(
    "dataCollector",
    attribs = attribs,
    content = components$content
  )
}

#' Major Deviations from the Sample Design
#' 
#' Information indicating correspondence as well as discrepancies between the sampled units (obtained) and available statistics for the population (age, 
#' sex-ratio, marital status, etc.) as a whole.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/deviat.html}
#' 
#' @export
ddi_deviat <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "deviat")
    check_attribs(attribs)
  }

  build_leaf_node(
    "deviat",
    attribs = attribs,
    content = components$content
  )
}

#' Frequency of Data Collection
#' 
#' For data collected at more than one point in time, the frequency with which the data were collected. The "freq" attribute is included to permit the development 
#' of a controlled vocabulary for this element.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/frequenc.html}
#' 
#' @export
ddi_frequenc <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                      "freq")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "frequenc")
    check_attribs(attribs)
  }

  build_leaf_node(
    "frequenc",
    attribs = attribs,
    content = components$content
  )
}

#' Instrument Development
#' 
#' Describe any development work on the data collection instrument. Type attribute allows for the optional use of a defined development type with or without 
#' use of a controlled vocabulary.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/instrumentDevelopment.html}
#' 
#' @export
ddi_instrumentDevelopment <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                      "type")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "instrumentDevelopment")
    check_attribs(attribs)
  }

  build_leaf_node(
    "instrumentDevelopment",
    attribs = attribs,
    content = components$content
  )
}

#' Type of Research Instrument
#' 
#' The type of data collection instrument used. "Structured" indicates an instrument in which all respondents are asked the same questions/tests, possibly with 
#' precoded answers. If a small portion of such a questionnaire includes open-ended questions, provide appropriate comments. "Semi-structured" indicates that the 
#' research instrument contains mainly open-ended questions. "Unstructured" indicates that in-depth interviews were conducted. The "type" attribute is included to 
#' permit the development of a controlled vocabulary for this element.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/resInstru.html}
#' 
#' @export
ddi_resInstru <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                      "type")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "resInstru")
    check_attribs(attribs)
  }

  build_leaf_node(
    "resInstru",
    attribs = attribs,
    content = components$content
  )
}

#' Sampling Procedure
#' 
#' The type of sample and sample design used to select the survey respondents to represent the population. May include reference to the target 
#' sample size and the sampling fraction.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/sampProc.html}
#' 
#' @export
ddi_sampProc <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "sampProc")
    check_attribs(attribs)
  }

  build_leaf_node(
    "sampProc",
    attribs = attribs,
    content = components$content
  )
}

#' Time Method
#' 
#' The time method or time dimension of the data collection. The "method" attribute is included to permit the development of a controlled vocabulary 
#' for this element.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/timeMeth.html}
#' 
#' @export
ddi_timeMeth <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "method")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "timeMeth")
    check_attribs(attribs)
  }

  build_leaf_node(
    "timeMeth",
    attribs = attribs,
    content = components$content
  )
}

#' Weighting
#' 
#' The use of sampling procedures may make it necessary to apply weights to produce accurate statistical results. Describe here the criteria for using weights 
#' in analysis of a collection. If a weighting formula or coefficient was developed, provide this formula, define its elements, and indicate how the formula 
#' is applied to data. 
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/timeMeth.html}
#' 
#' @export
ddi_weight <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "weight")
    check_attribs(attribs)
  }

  build_leaf_node(
    "weight",
    attribs = attribs,
    content = components$content
  )
}

#' Sample Frame
#' 
#' Sample frame describes the sampling frame used for identifying the population from which the sample was taken. For example, a telephone book may be 
#' a sample frame for a phone survey. In addition to the name, label and text describing the sample frame, this structure lists who maintains the sample 
#' frame, the period for which it is valid, a use statement, the universe covered, the type of unit contained in the frame as well as the number of units 
#' available, the reference period of the frame and procedures used to update the frame. Use multiple use statements to provide different uses under 
#' different conditions. Repeat elements within the use statement to support multiple languages.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/sampleFrame.html}
#' 
#' @export
ddi_sampleFrame <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "sampleFrame")
    check_attribs(attribs)
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

#' Custodian
#' 
#' Custodian identifies the agency or individual who is responsible for creating or maintaining the sample frame. Attribute affiliation provides the 
#' affiliation of the custodian with an agency or organization. Attribute abbr provides an abbreviation for the custodian.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/custodian.html}
#' 
#' @export
ddi_custodian <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "abbr")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "custodian")
    check_attribs(attribs)
  }

  build_leaf_node(
    "custodian",
    attribs = attribs,
    content = components$content
  )
}

#' Reference Period
#' 
#' Indicates the period of time in which the sampling frame was actually used for the study in question. Use ISO 8601 date/time formats to enter the relevant date(s).
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/referencePeriod.html}
#' 
#' @export
ddi_referencePeriod <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "event")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "referencePeriod")
    check_attribs_in_set(attribs$event, c("start", "end", "single"), field = "event (referencePeriod")
    check_attribs(attribs)
  }

  check_content_date(components$content) 

  build_leaf_node(
    "referencePeriod",
    attribs = attribs,
    content = components$content
  )
}

#' Sample Frame Name
#' 
#' Name of the sample frame.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/sampleFrameName.html}
#' 
#' @export
ddi_sampleFrameName <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "sampleFrameName")
    check_attribs(attribs)
  }

  build_leaf_node(
    "sampleFrameName",
    attribs = attribs,
    content = components$content
  )
}

#' Instrument Development
#' 
#' Description of how and with what frequency the sample frame is updated.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/updateProcedure.html}
#' 
#' @export
ddi_updateProcedure <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "updateProcedure")
    check_attribs(attribs)
  }

  build_leaf_node(
    "updateProcedure",
    attribs = attribs,
    content = components$content
  )
}

#' Valid Period
#' 
#' Defines a time period for the validity of the sampling frame. Enter dates in YYYY-MM-DD format.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/referencePeriod.html}
#' 
#' @export
ddi_validPeriod <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "event")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "validPeriod")
    check_attribs_in_set(attribs$event, c("start", "end", "single"), field = "event (validPeriod")
    check_attribs(attribs)
  }

  check_content_date(components$content) 

  build_leaf_node(
    "validPeriod",
    attribs = attribs,
    content = components$content
  )
}

#' Frame Unit
#' 
#' Provides information about the sampling frame unit. The attribute "isPrimary" is boolean, indicating whether the unit is primary or not.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/frameUnit.html}
#' 
#' @export
ddi_frameUnit <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "isPrimary")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "frameUnit")
    check_attribs(attribs)
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

#' Unit Type
#' 
#' Describes the type of sampling frame unit. The attribute "numberOfUnits" provides the number of units in the sampling frame.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/unitType.html}
#' 
#' @export
ddi_unitType <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "numberOfUnits")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "unitType")
    check_attribs(attribs)
  }

  build_leaf_node(
    "unitType",
    attribs = attribs,
    content = components$content
  )
}

#' Use Statement
#' 
#' Information on terms of use for the data collection. This element may be repeated only to support multiple language expressions of the content.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/useStmt.html}
#' 
#' @export
ddi_useStmt <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "useStmt")
    check_attribs(attribs)
  }

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

  build_branch_node(
    "useStmt",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' Confidentiality Declaration
#' 
#' This element is used to determine if signing of a confidentiality declaration is needed to access a resource. The "required" attribute is used to aid machine 
#' processing of this element, and the default specification is "yes". The "formNo" attribute indicates the number or ID of the form that the user must fill out. 
#' The "URI" attribute may be used to provide a URN or URL for online access to a confidentiality declaration form.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/unitType.html}
#' 
#' @export
ddi_confDec <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "required", "formNo", "URI")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "confDec")
    check_attribs(attribs)
  }

  build_leaf_node(
    "confDec",
    attribs = attribs,
    content = components$content
  )
}

#' Citation Requirement
#' 
#' Text of requirement that a data collection should be cited properly in articles or other publications that are based on analysis of the data.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/citReq.html}
#' 
#' @export
ddi_citReq <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "citReq")
    check_attribs(attribs)
  }

  build_leaf_node(
    "citReq",
    attribs = attribs,
    content = components$content
  )
}

#' Conditions
#' 
#' Indicates any additional information that will assist the user in understanding the access and use conditions of the data collection.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/conditions.html}
#' 
#' @export
ddi_conditions <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "conditions")
    check_attribs(attribs)
  }

  build_leaf_node(
    "conditions",
    attribs = attribs,
    content = components$content
  )
}

#' Contact Persons
#' 
#' Names and addresses of individuals responsible for the work. Individuals listed as contact persons will be used as resource persons regarding problems or 
#' questions raised by the user community. The URI attribute should be used to indicate a URN or URL for the homepage of the contact individual. The 
#' email attribute is used to indicate an email address for the contact individual. 
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/contact.html}
#' 
#' @export
ddi_contact <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "affiliation", "URI", "email")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "contact")
    check_attribs(attribs)
  }

  build_leaf_node(
    "contact",
    attribs = attribs,
    content = components$content
  )
}

#' Deposit Requirement
#' 
#' Information regarding user responsibility for informing archives of their use of data through providing citations to the published work or providing copies of the manuscripts. 
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/deposReq.html}
#' 
#' @export
ddi_deposReq <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "deposReq")
    check_attribs(attribs)
  }

  build_leaf_node(
    "deposReq",
    attribs = attribs,
    content = components$content
  )
}

#' Disclaimer
#' 
#' Information regarding responsibility for uses of the data collection. This element may be repeated to support multiple language expressions of the content.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/disclaimer.html}
#' 
#' @export
ddi_disclaimer <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "disclaimer")
    check_attribs(attribs)
  }

  build_leaf_node(
    "disclaimer",
    attribs = attribs,
    content = components$content
  )
}

#' Restrictions
#' 
#' Any restrictions on access to or use of the collection such as privacy certification or distribution restrictions should be indicated here. These 
#' can be restrictions applied by the author, producer, or disseminator of the data collection. If the data are restricted to only a certain class of 
#' user, specify which type. 
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/restrctn.html}
#' 
#' @export
ddi_restrctn <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "restrctn")
    check_attribs(attribs)
  }

  build_leaf_node(
    "restrctn",
    attribs = attribs,
    content = components$content
  )
}

#' Special Permissions
#' 
#' This element is used to determine if any special permissions are required to access a resource. The "required" attribute is used to aid machine 
#' processing of this element, and the default specification is "yes". The "formNo" attribute indicates the number or ID of the form that the user 
#' must fill out. The "URI" attribute may be used to provide a URN or URL for online access to a special permissions form.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/specPerm.html}
#' 
#' @export
ddi_specPerm <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                          "formNo", "URI")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "specPerm")
    check_attribs(attribs)
  }

  build_leaf_node(
    "specPerm",
    attribs = attribs,
    content = components$content
  )
}

#' Target Sample Size
#' 
#' Provides both the target size of the sample (this is the number in the original sample, not the number of respondents) as well as the formula used for determining the sample size.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/targetSampleSize.html}
#' 
#' @export
ddi_targetSampleSize <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "targetSampleSize")
    check_attribs(attribs)
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

#' Sample Size
#' 
#' This element provides the targeted sample size in integer format.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/sampleSize.html}
#' 
#' @export
ddi_sampleSize <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "sampleSize")
    check_attribs(attribs)
  }

  if(!is.integer(components$content)) rddi_err("sampleSize must be an integer")

  build_leaf_node(
    "sampleSize",
    attribs = attribs,
    content = components$content
  )
}

#' Sample Size Formula
#' 
#' This element includes the formula that was used to determine the sample size.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/sampleSizeFormula.html}
#' 
#' @export
ddi_sampleSizeFormula <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "sampleSizeFormula")
    check_attribs(attribs)
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
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/sources.html}
#' 
#' @export
ddi_sources <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "sources")
    check_attribs(attribs)
  }

  allowed_children <- c(
    "dataSrc",
    "sourceCitation",
    "srcOrig",
    "srcChar",
    "srcDocu",
    "sources"
  )
  # dataSrc, srcOrig, srcChar, srcDocu are crosslisted in resource, a child node of developmentActivity

  build_branch_node(
    "sources",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}


#' Source Citation
#' 
#' This complex element allows the inclusion of a standard citation for the sources used in collecting and creating the dataset.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/sourceCitation.html}
#' 
#' @export
ddi_sourceCitation <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", 
                        "Marcuri")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "sourceCitation")
    check_attribs(attribs)
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
  #children are the same as citation

  # rspStmt, prodStmt, and distStmt are only allowed once in citation according to DDI 2.5
  if(check_cardinality(components$content, "rspStmt") > 1) rddi_err("Only 0 or 1 rspStmt children are allowed in citation")
  if(check_cardinality(components$content, "prodStmt") > 1) rddi_err("Only 0 or 1 prodStmt children are allowed in citation")
  if(check_cardinality(components$content, "distStmt") > 1) rddi_err("Only 0 or 1 distStmt children are allowed in citation")
  
  build_branch_node(
    "sources",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' Data Appraisal
#' 
#' Information on data appraisal.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/anlyInfo.html}
#' 
#' @export
ddi_anlyInfo <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "anlyInfo")
    check_attribs(attribs)
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

#' Other Forms of Data Appraisal
#' 
#' Other issues pertaining to data appraisal. Describe here issues such as response variance, nonresponse rate and testing for bias, interviewer and response bias, 
#' confidence levels, question bias, etc. Attribute type allows for optional typing of data appraisal processes and option for controlled vocabulary.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataAppr.html}
#' 
#' @export
ddi_dataAppr <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "type")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "dataAppr")
    check_attribs(attribs)
  }

  build_leaf_node(
    "dataAppr",
    attribs = attribs,
    content = components$content
  )
}

#' Estimates of Sampling Error
#' 
#' Measure of how precisely one can estimate a population value from a given sample.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/EstSmpErr.html}
#' 
#' @export
ddi_EstSmpErr <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "EstSmpErr")
    check_attribs(attribs)
  }

  build_leaf_node(
    "EstSmpErr",
    attribs = attribs,
    content = components$content
  )
}

#' Response Rate
#' 
#' The percentage of sample members who provided information. This may include a broader description of stratified response rates, information affecting resonse rates etc.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/respRate.html}
#' 
#' @export
ddi_respRate <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "respRate")
    check_attribs(attribs)
  }

  build_leaf_node(
    "respRate",
    attribs = attribs,
    content = components$content
  )
}

#' Coding Instructions
#' 
#' Describe specific coding instructions used in data processing, cleaning, assession, or tabulation. Element relatedProcesses allows linking a coding 
#' instruction to one or more processes such as dataProcessing, dataAppr, cleanOps, etc. Use the txt element to describe instructions in a human readable form.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/codingInstructions.html}
#' 
#' @export
ddi_codingInstructions <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "type", "relatedProcesses")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "codingInstructions")
    check_attribs(attribs)
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

#' Command
#' 
#' Provide command code for the coding instruction. The formalLanguage attribute identifies the language of the command code.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/command.html}
#' 
#' @export
ddi_command <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                          "formalLanguage")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "command")
    check_attribs(attribs)
  }

  build_leaf_node(
    "command",
    attribs = attribs,
    content = components$content
  )
}

#' Data Processing
#' 
#' Describes various data processing procedures not captured elsewhere in the documentation, such as topcoding, recoding, suppression, tabulation, etc. 
#' The "type" attribute supports better classification of this activity, including the optional use of a controlled vocabulary.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataProcessing.html}
#' 
#' @export
ddi_dataProcessing <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                          "type")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "dataProcessing")
    check_attribs(attribs)
  }

  build_leaf_node(
    "dataProcessing",
    attribs = attribs,
    content = components$content
  )
}

#' Class of the Study
#' 
#' Generally used to give the data archive's class or study status number, which indicates the processing status of the study. May also be used as a text 
#' field to describe processing status. This element may be repeated to support multiple language expressions of the content.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/stdyClas.html}
#' 
#' @export
ddi_stdyClas <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                          "type")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "stdyClas")
    check_attribs(attribs)
  }

  build_leaf_node(
    "stdyClas",
    attribs = attribs,
    content = components$content
  )
}