#' method and its child nodes
#' 
#' This section describes the methodology and processing involved in a data 
#' collection. More information on these elements, especially their allowed 
#' attributes, can be found in the references. 
#' 
#' \emph{Parent nodes}
#' 
#' `method` is contained in `stdyDscr`.
#' 
#' \emph{method specific child nodes}
#' 
#' * `ddi_dataProcessing()` describes various data processing procedures not 
#' captured elsewhere in the documentation, such as topcoding, recoding, 
#' suppression, tabulation, etc. The "type" attribute supports better 
#' classification of this activity, including the optional use of a controlled 
#' vocabulary.
#' 
#' * `ddi_stdyClas()` is generally used to give the data archive's class or 
#' study status number, which indicates the processing status of the study. May 
#' also be used as a text field to describe processing status. This element may 
#' be repeated to support multiple language expressions of the content.
#' 
#' @param ... Child nodes or attributes. 
#' 
#' @return A ddi_node object.
#'
#' @section Shared and complex child nodes:
#' * [ddi_anlyInfo()]
#' * [ddi_codingInstructions()]
#' * [ddi_dataColl()]
#' * [ddi_notes()] 
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/method.html}{method documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataProcessing.html}{dataProcessing documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/stdyClas.html}{stdyClas documentation}
#' 
#' @examples
#' ddi_method()
#' 
#' # Functions that need to be wrapped in ddi_method()
#' 
#' ddi_dataProcessing(type = "topcoding",
#'                    "The income variables in this study (RESP_INC, HHD_INC, and 
#'                    SS_INC) were topcoded to protect confidentiality.")
#' 
#' ddi_stdyClas("ICPSR Class II")
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
#' Information about the data collection methodology employed in the codebook. 
#' More information on these elements, especially their allowed attributes, can 
#' be found in the references. 
#' 
#' \emph{Parent nodes}
#' 
#' `dataColl` is contained in `method`.
#' 
#'  \emph{dataColl specific child nodes}
#'  
#'  * `ddi_actMin()`is the summary of actions taken to minimize data loss. 
#'  Includes information on actions such as follow-up visits, supervisory 
#'  checks, historical matching, estimation, etc.
#'  
#'  * `ddi_cleanOps()` are the methods used to "clean" the data collection, 
#'  e.g., consistency checking, wild code checking, etc. The "agency" attribute 
#'  permits specification of the agency doing the data cleaning.
#'  
#'  * `ddi_collectorTraining()` describes the training provided to data 
#'  collectors including interviewer training, process testing, compliance with 
#'  standards etc. This is repeatable for language and to capture different 
#'  aspects of the training process. The type attribute allows specification of 
#'  the type of training being described.
#'  
#'  * `ddi_collMode()` is the method used to collect the data; instrumentation 
#'  characteristics.
#'  
#'  * `ddi_collSitu()` is the description of noteworthy aspects of the data 
#'  collection situation. Includes information on factors such as 
#'  cooperativeness of respondents, duration of interviews, number of 
#'  call-backs, etc.
#'  
#'  * `ddi_ConOps()` are control operations. These are methods to facilitate 
#'  data control performed by the primary investigator or by the data archive. 
#'  Specify any special programs used for such operations. The "agency" 
#'  attribute maybe used to refer to the agency that performed the control 
#'  operation.
#'  
#'  * `ddi_dataCollector()` is the entity (individual, agency, or institution) 
#'  responsible for administering the questionnaire or interview or compiling 
#'  the data. This refers to the entity collecting the data, not to the entity 
#'  producing the documentation. 
#'  
#'  * `ddi_deviat()` are major deviations from the sample design. This is 
#'  information indicating correspondence as well as discrepancies between the 
#'  sampled units (obtained) and available statistics for the population (age, 
#'  sex-ratio, marital status, etc.) as a whole. 
#'  
#'  * `ddi_frequenc()` is the frequency of data collection. It's for data 
#'  collected at more than one point in time.
#'  
#'  * `ddi_instrumentDevelopment()` describes any development work on the data 
#'  collection instrument. 
#'  
#'  * `ddi_resInstru()` is the type of data collection instrument used.
#'  
#'  * `ddi_sampProc()` is the type of sample and sample design used to select 
#'  the survey respondents to represent the population. May include reference 
#'  to the target sample size and the sampling fraction.
#'  
#'  * `ddi_weight()` defines the weights used to produce accurate statistical 
#'  results within the sampling procedures. Describe here the criteria for 
#'  using weights in analysis of a collection. If a weighting formula or 
#'  coefficient was developed, provide this formula, define its elements, and 
#'  indicate how the formula is applied to data. 
#' 
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object. 
#' 
#' @section Shared and complex child nodes:
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
#' @examples 
#' ddi_dataColl()
#' 
#' # Functions that need to be wrapped in ddi_dataColl()
#' 
#' ddi_actMin("To minimize the number of unresolved cases and reduce the 
#'            potential nonresponse bias, four follow-up contacts were made with 
#'            agencies that had not responded by various stages of the data 
#'            collection process.")
#'            
#' ddi_cleanOps("Checks for undocumented codes were performed, and data were 
#'              subsequently revised in consultation with the principal investigator.")
#'              
#' ddi_collectorTraining(type = "interviewer training",
#'                       "Describe research project, describe population and 
#'                       sample, suggest methods and language for approaching 
#'                       subjects, explain questions and key terms of survey instrument.")
#'                       
#' ddi_collMode("telephone interviews")
#' 
#' ddi_collSitu("There were 1,194 respondents who answered questions in face-to-face 
#'              interviews lasting approximately 75 minutes each.")
#'              
#' ddi_ConOps(agency = "ICPSR",
#'            "Ten percent of data entry forms were reentered to check for accuracy.")
#'            
#' ddi_dataCollector(abbr = "SRC",
#'                   affiliation = "University of Michigan",
#'                   role = "questionnaire administration",
#'                   "Survey Research Center")
#'                   
#' ddi_deviat("The suitability of Ohio as a research site reflected its similarity 
#'            to the United States as a whole. The evidence extended by Tuchfarber 
#'            (1988) shows that Ohio is representative of the United States in 
#'            several ways: percent urban and rural, percent of the population 
#'            that is African American, median age, per capita income, percent 
#'            living below the poverty level, and unemployment rate. Although 
#'            results generated from an Ohio sample are not empirically 
#'            generalizable to the United States, they may be suggestive of what 
#'            might be expected nationally.")
#'            
#' ddi_frequenc("monthly")
#' 
#' ddi_instrumentDevelopment(type = "pretesting",
#'                          "The questionnaire was pre-tested with split-panel 
#'                          tests, as well as an analysis of non-response rates 
#'                          for individual items, and response distributions.")
#'                          
#' ddi_resInstru("structured")
#' 
#' ddi_sampProc("National multistage area probability sample")
#' 
#' ddi_weight("The 1996 NES dataset includes two final person-level analysis weights 
#'            which incorporate sampling, nonresponse, and post-stratification 
#'            factors. One weight (variable #4) is for longitudinal micro-level 
#'            analysis using the 1996 NES Panel. The other weight (variable #3) 
#'            is for analysis of the 1996 NES combined sample (Panel component 
#'            cases plus Cross-section supplement cases). In addition, a Time 
#'            Series Weight (variable #5) which corrects for Panel attrition was 
#'            constructed. This weight should be used in analyses which compare 
#'            the 1996 NES to earlier unweighted National Election Study data 
#'            collections.")
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

  if(length(components$content) == 1 & is.character(components$content[[1]])) {
    build_leaf_node(
      "collMode",
      attribs = attribs,
      content = components$content
    )
  } else {
    allowed_children = c("concept", "txt")
    build_branch_node(
      "collMode",
      content = unwrap_content(components$content),
      attribs = attribs,
      allowed_children = allowed_children
    )
  }
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

  if(length(components$content) == 1 & is.character(components$content[[1]])) {
    build_leaf_node(
      "resInstru",
      attribs = attribs,
      content = components$content
    )
  } else {
    allowed_children = c("concept", "txt")
    build_branch_node(
      "resInstru",
      content = unwrap_content(components$content),
      attribs = attribs,
      allowed_children = allowed_children
    )
  }
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

  if(length(components$content) == 1 & is.character(components$content[[1]])) {
    build_leaf_node(
      "sampProc",
      attribs = attribs,
      content = components$content
    )
  } else {
    allowed_children = c("concept", "txt")
    build_branch_node(
      "sampProc",
      content = unwrap_content(components$content),
      attribs = attribs,
      allowed_children = allowed_children
    )
  }
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

  if(length(components$content) == 1 & is.character(components$content[[1]])) {
    build_leaf_node(
      "timeMeth",
      attribs = attribs,
      content = components$content
    )
  } else {
    allowed_children = c("concept", "txt")
    build_branch_node(
      "timeMeth",
      content = unwrap_content(components$content),
      attribs = attribs,
      allowed_children = allowed_children
    )
  }
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
#' Sample frame describes the sampling frame used for identifying the population 
#' from which the sample was taken. More information on these elements, 
#' especially their allowed attributes, can be found in the references. 
#' 
#' \emph{Parent nodes}
#' 
#' `sampleFrame` is contained in `dataColl`.
#' 
#' \emph{sampleFrame specific child nodes}
#' 
#' * `ddi_custodian()` identifies the agency or individual who is responsible 
#' for creating or maintaining the sample frame.
#' 
#' * `ddi_referencePeriod()` indicates the period of time in which the sampling 
#' frame was actually used for the study in question. Use ISO 8601 date/time 
#' formats to enter the relevant date(s).
#' 
#' * `ddi_sampleFrameName()` is the name of the sample frame.
#' 
#' * `ddi_updateProcedure()` is the description of how and with what frequency 
#' the sample frame is updated.
#' 
#' * `ddi_validPeriod()` defines a time period for the validity of the sampling 
#' frame. Enter dates in YYYY-MM-DD format.
#' 
#' @param ... Child nodes or attributes. 
#' 
#' @return A ddi_node object.
#'
#' @section Shared and complex child nodes:
#' * [ddi_frameUnit()]
#' * [ddi_labl()]
#' * [ddi_txt()]
#' * [ddi_universe()]
#' * [ddi_useStmt()] 
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/sampleFrame.html}{sampleFrame documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/custodian.html}{custodian documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/referencePeriod.html}{referencePeriod documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/sampleFrameName.html}{sampleFrameName documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/updateProcedure.html}{updateProcedure documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/validPeriod.html}{validPeriod documentation}
#' 
#' @examples 
#' ddi_sampleFrame()
#' 
#' # Functions that need to be wrapped in ddi_sampleFrame()
#' 
#' ddi_custodian("DEX Publications")
#' 
#' ddi_referencePeriod(event = "single",
#'                     "2009-06-01")
#' 
#' ddi_sampleFrameName("City of St. Paul Directory")
#' 
#' ddi_updateProcedure("Changes are collected as they occur through registration 
#'                     and loss of phone number from the specified geographic 
#'                     area. Data are compiled for the date June 1st of odd 
#'                     numbered years, and published on July 1st for the following 
#'                     two-year period.")
#' 
#' ddi_validPeriod(event = "start", "2009-07-01")
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
#' Provides information about the sampling frame unit. More information on 
#' these elements, especially their allowed attributes, can be found in the 
#' references. 
#' 
#' \emph{Parent nodes}
#' 
#' `frameUnit` is contained in `sampleFrame`.
#' 
#' \emph{frameUnit specific child nodes}
#' 
#' * `ddi_unitType()` describes the type of sampling frame unit. The attribute 
#' "numberOfUnits" provides the number of units in the sampling frame.
#' 
#' @param ... Child nodes or attributes. 
#' 
#' @return A ddi_node object.
#'
#' @section Shared and complex child nodes:
#' * [ddi_txt()]
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/frameUnit.html}{frameUnit documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/unitType.html}{unitType documentation}
#' 
#' @examples 
#' ddi_frameUnit()
#' 
#' # Functions that need to be wrapped in ddi_frameUnit()
#' 
#' ddi_unitType(numberOfUnits = 150000,
#'             "Primary listed owners of published phone numbers in the City of St. Paul")
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
    "unitType",
    "txt"
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
#' Provides both the target size of the sample (this is the number in the 
#' original sample, not the number of respondents) as well as the formula used 
#' for determining the sample size. More information on these elements, 
#' especially their allowed attributes, can be found in the references. 
#' 
#' \emph{Parent nodes}
#' 
#' `targetSampleSize` is contained in `dataColl`.
#' 
#' \emph{targetSampleSize specific child nodes}
#' 
#' * `ddi_sampleSize()` provides the targeted sample size in integer format.
#' 
#' * `ddi_sampleSizeFormula()` includes the formula that was used to determine 
#' the sample size.
#' 
#' @param ... Child nodes or attributes. 
#' 
#' @return A ddi_node object.
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/targetSampleSize.html}{targetSampleSize documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/sampleSize.html}{sampleSize documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/sampleSizeFormula.html}{sampleSizeFormula documentation}
#' 
#' @examples 
#' ddi_targetSampleSize()
#' 
#' # Functions that need to be wrapped in ddi_targetSampleSize()
#' 
#' ddi_sampleSize(385)
#' 
#' ddi_sampleSizeFormula("n0=Z2pq/e2=(1.96)2(.5)(.5)/(.05)2=385 individuals")
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

  if(components$content[[1]] == round(components$content[[1]])) components$content[[1]] <- as.integer(components$content[[1]])
  if(!is.integer(components$content[[1]])) rddi_err("sampleSize must be an integer")

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

  if(!is.character(components$content[[1]])) rddi_err("sampleSizeFormula must be a character string")

  build_leaf_node(
    "sampleSizeFormula",
    attribs = attribs,
    content = components$content
  )
}

#' sources and its child nodes
#' 
#' Description of sources used for the data collection. The element is nestable 
#' so that the sources statement might encompass a series of discrete source 
#' statements, each of which could contain the facts about an individual source. 
#' This element maps to Dublin Core Source element. More information on this 
#' element, especially its allowed attributes, can be found in the references.
#' 
#' \emph{Parent nodes}
#' 
#' `sources` is contained in the following elements: `dataColl` and `sources`.
#' 
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#'
#' @section Shared and complex child nodes:
#' * [ddi_dataSrc()]
#' * [ddi_sourceCitation()]
#' * [ddi_sources()]
#' * [ddi_srcChar()] 
#' * [ddi_srcDocu()]
#' * [ddi_srcOrig()] 
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/sources.html}{sources documentation}
#' 
#' @examples 
#' ddi_sources()
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
#' \emph{Parent nodes}
#' 
#' `anlyInfo` is contained in `method`.
#' 
#' \emph{anlyInfo specific child nodes}
#' 
#' * `ddi_dataAppr()` are other issues pertaining to data appraisal. Describe 
#' here issues such as response variance, nonresponse rate and testing for bias, 
#' interviewer and response bias, confidence levels, question bias, etc. 
#' Attribute type allows for optional typing of data appraisal processes and 
#' option for controlled vocabulary.
#' 
#' * `ddi_EstSmpErr()` are estimates of sampling error. This element is a 
#' measure of how precisely one can estimate a population value from a given 
#' sample.
#' 
#' * `ddi_respRate()` is the response rate. The percentage of sample members 
#' who provided information. This may include a broader description of 
#' stratified response rates, information affecting resonse rates etc.
#' 
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/anlyInfo.html}{anylInfo documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataAppr.html}{dataAppr documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/respRate.html}{respRate documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/EstSmpErr.html}{EstSmpErr documentation}
#' 
#' @examples 
#' ddi_anlyInfo()
#' 
#' # Functions that need to be wrapped in ddi_anlyInfo()
#' 
#' ddi_dataAppr("These data files were obtained from the United States House of 
#'              Representatives, who received them from the Census Bureau 
#'              accompanied by the following caveats...")
#' 
#' ddi_EstSmpErr("To assist NES analysts, the PC SUDAAN program was used to 
#'               compute sampling errors for a wide-ranging example set of 
#'               proportions estimated from the 1996 NES Pre-election Survey 
#'               dataset...")
#' 
#' ddi_respRate("For 1993, the estimated inclusion rate for TEDS-eligible 
#'              providers was 91 percent, with the inclusion rate for all 
#'              treatment providers estimated at 76 percent (including privately 
#'              and publicly funded providers).")
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
#' Describe specific coding instructions used in data processing, cleaning, 
#' assession, or tabulation. Element relatedProcesses allows linking a coding 
#' instruction to one or more processes such as dataProcessing, dataAppr, 
#' cleanOps, etc. Use the txt element to describe instructions in a human 
#' readable form. More information on these elements, especially their allowed 
#' attributes, can be found in the references. 
#' 
#' \emph{Parent nodes}
#' 
#' `codingInstructions` is contained in `method`.
#' 
#' \emph{codingInstructions specific child nodes}
#' 
#' * `ddi_command()` provides command code for the coding instruction. The 
#' formalLanguage attribute identifies the language of the command code.
#' 
#' @param ... Child nodes or attributes. 
#' 
#' @return A ddi_node object.
#'
#' @section Shared and complex child nodes:
#' * [ddi_txt()]
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/codingInstructions.html}{codingInstructions documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/command.html}{command documentation}
#' 
#' @examples 
#' ddi_codingInstructions()
#' 
#' # Functions that need to be wrapped in ddi_codingInstructions()
#' 
#' ddi_command(formalLanguage = "SPSS",
#'             "RECODE V1 TO V100 (10 THROUGH HIGH = 0)")
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
    "txt",
    "command"
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