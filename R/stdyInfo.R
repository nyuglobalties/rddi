#' stdyInfo and its child nodes
#' 
#' stdyInfo is the study scope. It contains information about the data 
#' collection's scope across several dimensions, including substantive content, 
#' geography, and time. More information on these elements, especially their 
#' allowed attributes, can be found in the references. 
#' 
#' \emph{Parent nodes}
#' 
#' `stdyInfo` is contained in `stdyDscr`.
#' 
#' \emph{stdyInfo specific child nodes}
#' 
#' * `ddi_abstract()` is an unformatted summary describing the purpose, nature, 
#' and scope of the data collection, special characteristics of its contents, 
#' major subject areas covered, and what questions the PIs attempted to answer 
#' when they conducted the study. A listing of major variables in the study is 
#' important here. In cases where a codebook contains more than one abstract 
#' (for example, one might be supplied by the data producer and another 
#' prepared by the data archive where the data are deposited), the "source" 
#' and "date" attributes may be used to distinguish the abstract versions. Maps 
#' to Dublin Core Description element. Inclusion of this element in the 
#' codebook is recommended. The "date" attribute should follow ISO convention 
#' of YYYY-MM-DD. 
#' 
#' * `ddi_studyBudget()`is used to describe the budget of the project in as 
#' much detail as needed. 
#' 
#' @param ... Child nodes or attributes. 
#' 
#' @return A ddi_node object.
#' 
#' @section Shared and complex child nodes:
#' * [ddi_exPostEvaluation()]
#' * [ddi_notes()]
#' * [ddi_qualityStatement()]
#' * [ddi_subject()]
#' * [ddi_sumDscr()]
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/stdyInfo.html}{stdyInfo documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/abstract.html}{abstract documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/studyBudget.html}{studyBudget documentation}
#' 
#' @examples 
#' ddi_stdyInfo()
#' 
#' # Functions that need to be wrapped in ddi_stdyInfo()
#' 
#' ddi_abstract(date = "1999-01-28",
#'              contentType = "abstract",
#'              "Data on labor force activity for the week prior to the survey 
#'              are supplied in this collection. Information is available on the 
#'              employment status, occupation, and industry of persons 15 years 
#'              old and over. Demographic variables such as age, sex, race, marital 
#'              status, veteran status, household relationship, educational 
#'              background, and Hispanic origin are included. In addition to 
#'              providing these core data, the May survey also contains a 
#'              supplement on work schedules for all applicable persons aged 
#'              15 years and older who were employed at the time of the survey. 
#'              This supplement focuses on shift work, flexible hours, and work 
#'              at home for both main and second jobs.")
#' 
#' ddi_studyBudget("The budget for the study covers a 5 year award period 
#'                 distributed between direct and indirect costs including: 
#'                 Staff, ...")
#' 
#' @export
ddi_stdyInfo <- function(...) {
  allowed_children <- c(
    "studyBudget",
    "subject", 
    "abstract",
    "sumDscr",
    "qualityStatement",
    "notes",
    "exPostEvaluation"
  )

  components <- dots_to_xml_components(...)
  attribs <- components$attribs    

  if (!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "stdyInfo")
  }

  build_branch_node(
    "stdyInfo",
    allowed_children = allowed_children,
    components = components,
    attribs = attribs
  )
}

#' @rdname ddi_stdyInfo
#' @export
ddi_abstract <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                          "date", "contentType")
    attribs <- validate_attributes(attribs, allowed_attribs, "abstract")
  }

  if("contentType" %in% attribs) check_attribs_in_set(attribs$contentType, c("abstract", "purpose", "mixed"), field = "contentType")

  build_leaf_node(
    "abstract",
    attribs = attribs,
    content = components$content
  )
}

#' exPostEvaluation and its child nodes
#'
#' Post Evaluation Procedures describes evaluation procedures not addressed in 
#' data evaluation processes. These may include issues such as timing of the 
#' study, sequencing issues, cost/budget issues, relevance, institutional or 
#' legal arrangements etc. of the study. More information on these elements, 
#' especially their allowed attributes, can be found in the references. 
#'
#' \emph{Parent nodes}
#' 
#' `exPostEvaluation` is contained in `stdyInfo`.
#' 
#' \emph{exPostEvaluation specific child nodes}
#' 
#' * `ddi_evaluationProcess()` describes the evaluation process followed.
#' 
#' * `ddi_evaluator()` identifies persons or organizations involved in the 
#' evaluation. 
#' 
#' * `ddi_outcomes()` describes the outcomes of the evaluation.
#'
#' @param ... Child nodes or attributes. 
#' 
#' @return A ddi_node object.
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/exPostEvaluation.html}{exPostEvaluation documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/evaluationProcess.html}{evaluationProcess documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/evaluator.html}{evaluator documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/outcomes.html}{outcomes documentation}
#' 
#' @examples 
#' ddi_exPostEvaluation()
#' 
#' # Functions that need to be wrapped in ddi_exPostEvaluation()
#' 
#' ddi_evaluationProcess("This dataset was evaluated using the following methods...")
#' 
#' ddi_evaluator(affiliation = "United Nations",
#'               abbr = "UNSD",
#'               role = "consultant",
#'               "United Nations Statistical Division")
#'               
#' ddi_outcomes("The following steps were highly effective in increasing response 
#'              rates, and should be repeated in the next collection cycle...")
#' 
#' @export
ddi_exPostEvaluation <- function(...) {
  allowed_children <- c(
    "evaluator",
    "evaluationProcess", 
    "outcomes"
  )

  components <- dots_to_xml_components(...)
  attribs <- components$attribs    

  if (!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "completionDate", "type")
    attribs <- validate_attributes(attribs, allowed_attribs, "exPostEvaluation")
  }

  build_branch_node(
    "exPostEvaluation",
    allowed_children = allowed_children,
    components = components,
    attribs = attribs
  )
}

#' @rdname ddi_exPostEvaluation
#' @export
ddi_evaluationProcess <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "evaluationProcess")
  }

  build_leaf_node(
    "evaluationProcess",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_exPostEvaluation
#' @export
ddi_evaluator <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "affiliation", "abbr", "role")
    attribs <- validate_attributes(attribs, allowed_attribs, "evaluator")
  }

  build_leaf_node(
    "evaluator",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_exPostEvaluation
#' @export
ddi_outcomes <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "outcomes")
  }

  build_leaf_node(
    "outcomes",
    attribs = attribs,
    content = components$content
  )
}

#' qualityStatement and its child nodes
#'
#' The Quality Statement consists of two parts, standardsCompliance and 
#' otherQualityStatements. In standardsCompliance list all specific standards 
#' complied with during the execution of this study. Note the standard name and 
#' producer and how the study complied with the standard. More information on 
#' these elements, especially their allowed attributes, can be found in the 
#' references. 
#'
#' \emph{Parent nodes}
#' 
#' `qualiyStatement` is contained in `stdyInfo`.
#' 
#' \emph{qualityStatment specific child nodes}
#' 
#' * `ddi_otherQualityStatement()` holds additional quality statements. 
#'
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#'
#' @section Shared and complex child nodes:
#' * [ddi_standardsCompliance()]
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/qualityStatement.html}{qualityStatment documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/otherQualityStatement.html}{otherQualityStatement documentation}
#' 
#' @examples 
#' ddi_qualityStatement()
#' 
#' # Functions that need to be wrapped in ddi_qualityStatement()
#' 
#' ddi_otherQualityStatement("Additional quality statements not addressed in standardsCompliance.")
#' 
#' @export
ddi_qualityStatement <- function(...) {
  allowed_children <- c(
    "standardsCompliance",
    "otherQualityStatement"
  )

  components <- dots_to_xml_components(...)
  attribs <- components$attribs    

  if (!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "qualityStatement")
  }

  build_branch_node(
    "qualityStatement",
    allowed_children = allowed_children,
    components = components,
    attribs = attribs
  )
}

#' @rdname ddi_qualityStatement
#' @export
ddi_otherQualityStatement <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "otherQualityStatement")
  }

  build_leaf_node(
    "otherQualityStatement",
    attribs = attribs,
    content = components$content
  )
}

#' standardsCompliance and its child nodes
#'
#' The standards compliance section lists all specific standards complied with 
#' during the execution of this study.  More information on these elements, 
#' especially their allowed attributes, can be found in the references. 
#'
#' \emph{Parent nodes}
#' 
#' `standardsCompliance` is contained in `qualityStatement`.
#' 
#' \emph{standardsCompliance specific child nodes}
#' 
#' * `ddi_complianceDescription` describes how the study complied with each 
#' standard. 
#'
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#'
#' @section Shared and complex child nodes:
#' * [ddi_standard()]
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/standardsCompliance.html}{standardsCompliance documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/complianceDescription.html}{complianceDescription documentation}
#' 
#' @examples 
#' # Note: ddi_standard() is a required child for ddi_standardsCompliance()
#' ddi_standardsCompliance(ddi_standard())
#' 
#' # Functions that need to be wrapped in ddi_standardsCompliance()
#' 
#' ddi_complianceDescription("This study complied to X standard by...")
#' 
#' @export
ddi_standardsCompliance <- function(...) {
  allowed_children <- c(
    "standard",
    "complianceDescription"
  )

  components <- dots_to_xml_components(...)
  attribs <- components$attribs    

  if (!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "standardsCompliance")
  }

  build_branch_node(
    "standardsCompliance",
    allowed_children = allowed_children,
    required_children = "standard",
    components = components,
    attribs = attribs
  )
}

#' @rdname ddi_standardsCompliance 
#' @export
ddi_complianceDescription <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "complianceDescription")
  }

  build_leaf_node(
    "complianceDescription",
    attribs = attribs,
    content = components$content
  )
}

#' standard and its child nodes
#'
#' Standard describes a standard with which the study complies. More information 
#' on these elements, especially their allowed attributes, can be found in the 
#' references. 
#' 
#' \emph{Parent nodes}
#' 
#' `standard` is contained in `standardsCompliance`.
#' 
#' \emph{standard specific child nodes}
#' 
#' * `ddi_standardName()` contains the name of the standard with which the 
#' study complies. 
#'
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#'
#' @section Shared and complex child nodes:
#' * [ddi_producer()]
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/standard.html}{standard documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/standardName.html}{standardName documentation}
#' 
#' @examples 
#' ddi_standard()
#' 
#' # Functions that need to be wrapped in ddi_standard()
#' 
#' ddi_standardName(date = "2009-10-18",
#'                  version = "3.1",
#'                  URI = "http://www.ddialliance.org/Specification/DDI-Lifecycle/3.1/",
#'                  "Data Documentation Initiative")
#' 
#' @export
ddi_standard <- function(...) {
  allowed_children <- c(
    "standardName",
    "producer"
  )

  components <- dots_to_xml_components(...)
  attribs <- components$attribs    

  if (!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "standard")
  }

  build_branch_node(
    "standard",
    allowed_children = allowed_children,
    components = components,
    attribs = attribs
  )
}

#' @rdname ddi_standard 
#' @export
ddi_standardName <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "date", "version", "URI")
    attribs <- validate_attributes(attribs, allowed_attribs, "standardName")
  }

  build_leaf_node(
    "standardName",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_stdyInfo
#' @export
ddi_studyBudget <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "studyBudget")
  }

  build_leaf_node(
    "studyBudget",
    attribs = attribs,
    content = components$content
  )
}

#' subject and its child nodes
#'
#' Subject describes the data collection's intellectual content. More 
#' information on these elements, especially their allowed attributes, can be 
#' found in the references. 
#'
#' \emph{Parent nodes}
#' 
#' `subject` is contained in `stdyInfo`.
#' 
#' \emph{subject specific child nodes}
#' 
#' * `ddi_keyword()` are words or phrases that describe salient aspects of a 
#' data collection's content. Can be used for building keyword indexes and for 
#' classification and retrieval purposes. A controlled vocabulary can be 
#' employed. Maps to Dublin Core Subject element. 
#' 
#' * `ddi_topcClas()` indicates the broad substantive topic(s) that the data 
#' cover. Library of Congress subject terms may be used here. Maps to Dublin 
#' Core Subject element. Inclusion of this element in the codebook is 
#' recommended.
#' 
#' @param ... Child nodes or attributes. 
#' 
#' @return A ddi_node object.
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/subject.html}{subject documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/keyword.html}{keyword documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/topcClas.html}{topcClas documentation}
#' 
#' @examples 
#' ddi_subject()
#' 
#' # Functions that need to be wrapped in ddi_subject()
#' 
#' ddi_keyword(vocab = "ICPSR Subject Thesaurus",
#'             vocabURI = "http://www.icpsr.umich.edu/thesaurus/subject.html",
#'             "quality of life")
#' 
#' ddi_topcClas(vocab = "LOC Subject Headings",
#'              vocabURI = "http://www.loc.gov/catdir/cpso/lcco/lcco.html",
#'              "Public opinion -- California -- Statistics")
#' 
#' @export
ddi_subject <- function(...) {
  allowed_children <- c(
    "keyword",
    "topcClas"
  )

  components <- dots_to_xml_components(...)
  attribs <- components$attribs    

  if (!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "subject")
  }

  build_branch_node(
    "subject",
    allowed_children = allowed_children,
    components = components,
    attribs = attribs
  )
}

#' @rdname ddi_subject
#' @export
ddi_keyword <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "vocab", "vocabURI")
    attribs <- validate_attributes(attribs, allowed_attribs, "keyword")
  }

  build_leaf_node(
    "keyword",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_subject
#' @export
ddi_topcClas <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "vocab", "vocabURI")
    attribs <- validate_attributes(attribs, allowed_attribs, "topcClas")
  }

  build_leaf_node(
    "topcClas",
    attribs = attribs,
    content = components$content
  )
}

#' sumDscr and its child nodes
#'
#' This is the summary data description and it contains information about the 
#' geographic coverage of the study and unit of analysis. More information on 
#' these elements, especially their allowed attributes, can be found in the 
#' references. 
#' 
#' \emph{Parent nodes}
#' 
#' `sumDscr` is contained in `stdyInfo`.
#' 
#' \emph{sumDscr specific child nodes}
#' 
#' * `ddi_anlyUnit()` is the basic unit of analysis or observation that the file 
#' describes: individuals, families/households, groups, 
#' institutions/organizations, administrative units, etc. 
#' 
#' * `ddi_collDate()` contains the date(s) when the data were collected. Maps to 
#' Dublin Core Coverage element. Inclusion of this element in the codebook is 
#' recommended.
#' 
#' * `ddi_dataKind()` is the type of data included in the file: survey data, 
#' census/enumeration data, aggregate data, clinical data, event/transaction 
#' data, program source code, machine-readable text, administrative records 
#' data, experimental data, psychological test, textual data, coded textual, 
#' coded documents, time budget diaries, observation data/ratings, 
#' process-produced data, etc. This element maps to Dublin Core Type element. 
#' 
#' * `ddi_geogCover()` is information on the geographic coverage of the data. 
#' Includes the total geographic scope of the data, and any additional levels 
#' of geographic coding provided in the variables. Maps to Dublin Core Coverage 
#' element. 
#' 
#' * `ddi_geogUnit()` is the lowest level of geographic aggregation covered by 
#' the data.
#' 
#' * `ddi_nation()` indicates the country or countries covered in the file. 
#' Attribute "abbr" may be used to list common abbreviations; use of ISO country 
#' codes is recommended. Maps to Dublin Core Coverage element. Inclusion of 
#' this element is recommended.
#' 
#' * `ddi_timePrd()` is the time period to which the data refer. This item 
#' reflects the time period covered by the data, not the dates of coding or 
#' making documents machine-readable or the dates the data were collected. Also 
#' known as span. Maps to Dublin Core Coverage element. Inclusion of this 
#' element is recommended. 
#'
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#'
#' @section Shared and complex child nodes:
#' * [ddi_boundPoly()]
#' * [ddi_geoBndBox()]
#' * [ddi_universe()]
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/sumDscr.html}{sumDscr documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/anlyInfo.html}{anlyInfo documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/collDate.html}{collDate documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataKind.html}{dataKind documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/geogCover.html}{geogCover documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/geogUnit.html}{geogUnit documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/nation.html}{nation documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/timePrd.html}{timePrd documentation}
#' 
#' @examples 
#' ddi_sumDscr()
#' 
#' # Functions that need to be wrapped in ddi_sumDscr()
#' 
#' ddi_anlyUnit("individuals")
#' 
#' ddi_collDate(event = "single",
#'              date = "1998-11-10",
#'              "10 November 1998")
#'              
#' ddi_dataKind(type = "numeric",
#'              "survey data")
#'              
#' ddi_geogCover("State of California")
#' 
#' ddi_geogUnit("state")
#' 
#' ddi_nation(abbr = "GB",
#'            "United Kingdom")
#'            
#' ddi_timePrd(event = "start",
#'             date = "1998-05-01",
#'             "May 1, 1998")
#' 
#' @export
ddi_sumDscr <- function(...) {
  allowed_children <- c(
    "timePrd",
    "collDate",
    "nation",
    "geogCover",
    "geogUnit",
    "geoBndBox",
    "boundPoly",
    "anlyUnit",
    "universe",
    "dataKind"        
  )

  components <- dots_to_xml_components(...)
  attribs <- components$attribs    

  if(check_cardinality(components$content, "geoBndBox") > 1) rddi_err("Only 0 or 1 geoBndBox child is allowed in sumDscr")

  if (!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "sumDscr")
  }

  build_branch_node(
    "sumDscr",
    allowed_children = allowed_children,
    components = components,
    attribs = attribs
  )
}

#' @rdname ddi_sumDscr
#' @export
ddi_anlyUnit <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "unit")
    attribs <- validate_attributes(attribs, allowed_attribs, "anlyUnit")
  }

  if(length(components$content) == 1 & is.character(components$content[[1]])) {
    build_leaf_node(
      "anlyUnit",
      attribs = attribs,
      content = components$content
    )
  } else {
    allowed_children = c("concept", "txt")
    build_branch_node(
      "anlyUnit",
      content = unwrap_content(components$content),
      attribs = components$attribs,
      allowed_children = allowed_children
    )
  }
  
}

#' boundPoly and its child nodes
#'
#' The geographic bounding polygon field allows the creation of multiple 
#' polygons to describe in a more detailed manner the geographic area covered 
#' by the dataset. It should only be used to define the outer boundaries of a 
#' covered area. For example, in the United States, such polygons can be created 
#' to define boundaries for Hawaii, Alaska, and the continental United States, 
#' but not interior boundaries for the contiguous states. This field is used to 
#' refine a coordinate-based search, not to actually map an area. If the 
#' boundPoly element is used, then geoBndBox MUST be present, and all points 
#' enclosed by the boundPoly MUST be contained within the geoBndBox. Elements 
#' westBL, eastBL, southBL, and northBL of the geoBndBox should each be 
#' represented in at least one point of the boundPoly description. More 
#' information on these elements, especially their allowed attributes, can be 
#' found in the references. 
#'
#' \emph{Parent nodes}
#' 
#' `boundPoly` is contained in `sumDscr`.
#'
#' @param ... Child nodes or attributes. 
#' 
#' @return A ddi_node object.
#'
#' @section Shared and complex child nodes:
#' * [ddi_polygon()] 
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/boundPoly.html}{boundPoly documentation}
#' 
#' @examples 
#' # ddi_boundPoly requires ddi_polygon(). ddi_polygon then requires ddi_point()
#' # which requires ddi_gringLat() and ddi_gringLon()
#' 
#' ddi_boundPoly(ddi_polygon(
#'                 ddi_point(
#'                    ddi_gringLat("42.002207"), 
#'                    ddi_gringLon("-120.005729004")
#'                  )
#'                )
#'              )
#' 
#' @export
ddi_boundPoly <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs    

  if (!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "boundPoly")
  }

  build_branch_node(
    "boundPoly",
    allowed_children = "polygon",
    required_children = "polygon",
    components = components,
    attribs = attribs
  )
}

#' polygon and its child nodes
#' 
#' The minimum polygon that covers a geographical area, and is delimited by at 
#' least 4 points (3 sides), in which the last point coincides with the first 
#' point.More information on these elements, especially their allowed 
#' attributes, can be found in the references. 
#' 
#' \emph{Parent nodes}
#' 
#' `polygon` is contained in `boundPoly`.
#' 
#' @param ... Child nodes or attributes. 
#' 
#' @return A ddi_node object. 
#'
#' @section Shared and complex child nodes:
#' * [ddi_point()] 
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/polygon.html}{polygon documentation}
#' 
#' @examples 
#' # ddi_polygon requires ddi_point() which requires ddi_gringLat() and ddi_gringLon()
#' ddi_polygon(ddi_point(
#'                ddi_gringLat("42.002207"), 
#'                ddi_gringLon("-120.005729004")
#'              )
#'            )
#'            
#' @export
ddi_polygon <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "polygon")
  }

  build_branch_node(
    "polygon",
    allowed_children = "point",
    required_children = "point",
    attribs = attribs,
    content = components$content
  )
}

#' point and its child nodes
#' 
#' 0-dimensional geometric primitive, representing a position, but not having 
#' extent. In this declaration, point is limited to a longitude/latitude 
#' coordinate system.
#' 
#' \emph{Parent nodes}
#' 
#' `point` is contained in `polygon`.
#' 
#' \emph{point specific child nodes}
#' 
#' * `ddi_gringLat()` is the latitude (y coordinate) of a point. Valid range 
#' expressed in decimal degrees is as follows: -90,0 to 90,0 degrees (latitude).
#' 
#' * `ddi_gringLon()` is the longitude (x coordinate) of a point. Valid range 
#' expressed in decimal degrees is as follows: -180,0 to 180,0 degrees (longitude).
#' 
#' @param ... Child nodes or attributes. 
#' 
#' @return A ddi_node object.
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/point.html}{point documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/gringLat.html}{gringLat documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/gringLon.html}{gringLon documentation}
#' 
#' @examples 
#' # ddi_point() which requires ddi_gringLat() and ddi_gringLon()
#' 
#' ddi_point(ddi_gringLat("42.002207"), ddi_gringLon("-120.005729004"))
#'            
#' @export
ddi_point <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "point")
  }

  build_branch_node(
    "point",
    allowed_children = c("gringLat", "gringLon"),
    required_children = c("gringLat", "gringLon"),
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_point
#' @export
ddi_gringLat <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "gringLat")
  }

  if(components$content < -90 || components$content > 90) rddi_err("gringLat must be between -90.0 and 90.0")

  build_leaf_node(
    "gringLat",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_point 
#' @export
ddi_gringLon <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "gringLon")
  }

  if(components$content < -180 || components$content > 180) rddi_err("gringLat must be between -180.0 and 180.0")

  build_leaf_node(
    "gringLon",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_sumDscr
#' @export
ddi_collDate <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "date", "event", "cycle")
    attribs <- validate_attributes(attribs, allowed_attribs, "collDate")
    if("event" %in% attribs$event) check_attribs_in_set(attribs$event, c("start", "end", "single"), field = "event (collDate")
  }

  build_leaf_node(
    "collDate",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_sumDscr
#' @export
ddi_dataKind<- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "type")
    attribs <- validate_attributes(attribs, allowed_attribs, "dataKind")
  }

  if(length(components$content) == 1 & is.character(components$content[[1]])) {
    build_leaf_node(
      "dataKind",
      attribs = attribs,
      content = components$content
    )
  } else {
    allowed_children = c("concept", "txt")
    build_branch_node(
      "dataKind",
      content = unwrap_content(components$content),
      attribs = components$attribs,
      allowed_children = allowed_children
    )
  }
  
}

#' geoBndBox and its child nodes
#' 
#' The fundamental geometric description for any dataset that models geography. 
#' GeoBndBox is the minimum box, defined by west and east longitudes and north 
#' and south latitudes, that includes the largest geographic extent of the 
#' dataset's geographic coverage. This element is used in the first pass of a 
#' coordinate-based search. If the boundPoly element is included, then the 
#' geoBndBox element MUST be included. More information on these elements, 
#' especially their allowed attributes, can be found in the references. 
#' 
#' \emph{Parent nodes}
#' 
#' `geoBndBox` is contained in `sumDscr`.
#' 
#' \emph{geoBndBox specific child nodes}
#' 
#' * `ddi_eastBL()` is the easternmost coordinate delimiting the geographic 
#' extent of the dataset. A valid range of values, expressed in decimal degrees 
#' (positive east and positive north), is: -180,0 <= East Bounding Longitude 
#' Value <= 180,0.
#' 
#' * `ddi_northBL()` is the northernmost coordinate delimiting the geographic 
#' extent of the dataset. A valid range of values, expressed in decimal degrees 
#' (positive east and positive north), is: -90,0 <= North Bounding Latitude 
#' Value <= 90,0 ; North Bounding Latitude Value >= South Bounding Latitude 
#' Value.
#' 
#' * `ddi_southBL()` is the southernmost coordinate delimiting the geographic 
#' extent of the dataset. A valid range of values, expressed in decimal degrees 
#' (positive east and positive north), is: -90,0 <=South Bounding Latitude 
#' Value <= 90,0 ; South Bounding Latitude Value <= North Bounding Latitude 
#' Value.
#' 
#' * `ddi_westBL()` is the westernmost coordinate delimiting the geographic 
#' extent of the dataset. A valid range of values, expressed in decimal degrees 
#' (positive east and positive north), is: -180,0 <=West Bounding Longitude 
#' Value <= 180,0.
#' 
#' @param ... Child nodes or attributes. 
#' 
#' @return A ddi_node object.
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/geoBndBox.html}{geoBndBox documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/eastBL.html}{eastBL documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/northBL.html}{northBL documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/southBL.html}{southBL documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/westBL.html}{westBL documentation}
#' 
#' @examples 
#' ddi_geoBndBox()
#' 
#' # Functions that need to be wrapped in ddi_geoBndBox()
#' 
#' ddi_eastBL("90")
#' 
#' ddi_northBL("17")
#' 
#' ddi_southBL("45")
#' 
#' ddi_westBL("-10")
#' 
#' @export
ddi_geoBndBox <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "geoBndBox")
  }

  allowed_children = c("westBL", "eastBL", "southBL", "northBL")
  
  build_branch_node(
    "geoBndBox",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_geoBndBox
#' @export
ddi_eastBL <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "eastBL")
  }

  if(components$content < -180 || components$content > 180) rddi_err("eastBL must be between -180.0 and 180.0")

  build_leaf_node(
    "eastBL",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_geoBndBox
#' @export
ddi_northBL <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "northBL")
  }

  if(components$content < -90 || components$content > 90) rddi_err("northBL must be between -90.0 and 90.0")

  build_leaf_node(
    "northBL",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_geoBndBox 
#' @export
ddi_southBL <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "southBL")
  }

  if(components$content < -90 || components$content > 90) rddi_err("southBL must be between -90.0 and 90.0")

  build_leaf_node(
    "southBL",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_geoBndBox
#' @export
ddi_westBL <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "westBL")
  }

  if(components$content < -180 || components$content > 100) rddi_err("westBL must be between -180.0 and 180.0")

  build_leaf_node(
    "westBL",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_sumDscr
#' @export
ddi_geogCover <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "geogCover")
  }

  if(length(components$content) == 1 & is.character(components$content[[1]])) {
    build_leaf_node(
      "geogCover",
      attribs = attribs,
      content = components$content
    )
  } else {
    allowed_children = c("concept", "txt")
    build_branch_node(
      "geogCover",
      content = unwrap_content(components$content),
      attribs = components$attribs,
      allowed_children = allowed_children
    )
  }
}

#' @rdname ddi_sumDscr
#' @export
ddi_geogUnit <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "geogUnit")
  }

  if(length(components$content) == 1 & is.character(components$content[[1]])) {
    build_leaf_node(
      "geogUnit",
      attribs = attribs,
      content = components$content
    )
  } else {
    allowed_children = c("concept", "txt")
    build_branch_node(
      "geogUnit",
      content = unwrap_content(components$content),
      attribs = components$attribs,
      allowed_children = allowed_children
    )
  }
}

#' @rdname ddi_sumDscr
#' @export
ddi_nation <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "abbr")
    attribs <- validate_attributes(attribs, allowed_attribs, "nation")
  }

  if(length(components$content) == 1 & is.character(components$content[[1]])) {
    build_leaf_node(
      "nation",
      attribs = attribs,
      content = components$content
    )
  } else {
    allowed_children = c("concept", "txt")
    build_branch_node(
      "nation",
      content = unwrap_content(components$content),
      attribs = components$attribs,
      allowed_children = allowed_children
    )
  }
}

#' @rdname ddi_sumDscr
#' @export
ddi_timePrd <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "date", "event", "cycle")
    attribs <- validate_attributes(attribs, allowed_attribs, "timePrd")
    if("event" %in% attribs) check_attribs_in_set(c("start", "end", "single"), field = "event (timePrd)")
  }

  build_leaf_node(
    "timePrd",
    attribs = attribs,
    content = components$content
  )
}