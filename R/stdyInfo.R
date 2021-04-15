#' Study Scope
#'
#' This section contains information about the data collection's scope across several dimensions, including substantive content, geography, and time.
#'
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/stdyInfo.html}
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
        check_attribs_in_set(names(attribs), allowed_attribs, field = "stdyInfo")
        check_attribs(attribs)
    }

    build_branch_node(
        "stdyInfo",
        allowed_children = allowed_children,
        components = components,
        attribs = attribs
    )
}

#' Abstract
#' 
#' An unformatted summary describing the purpose, nature, and scope of the data collection, special characteristics of its contents, major subject 
#' areas covered, and what questions the PIs attempted to answer when they conducted the study. A listing of major variables in the study is important 
#' here. In cases where a codebook contains more than one abstract (for example, one might be supplied by the data producer and another prepared by the 
#' data archive where the data are deposited), the "source" and "date" attributes may be used to distinguish the abstract versions. Maps to Dublin Core 
#' Description element. Inclusion of this element in the codebook is recommended. The "date" attribute should follow ISO convention of YYYY-MM-DD. The 
#' contentType attribute provides forward-compatibility with DDI 3 by describing where the content fits in that structure, or if is mixed in terms of 
#' what is contained.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/abstract.html}
#' 
#' @export
ddi_abstract <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                          "date", "contentType")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "abstract")
    check_attribs(attribs)
  }

  if("contentType" %in% attribs) check_attribs_in_set(attribs$contentType, c("abstract", "purpose", "mixed"), field = "contentType")

  build_leaf_node(
    "abstract",
    attribs = attribs,
    content = components$content
  )
}

#' Post Evaluation Procedures
#'
#' Use this section to describe evaluation procedures not address in data evaluation processes. These may include issues such as timing of the study, sequencing 
#' issues, cost/budget issues, relevance, instituional or legal arrangments etc. of the study. The completionDate attribute holds the date the evaluation was 
#' completed. The type attribute is an optional type to identify the type of evaluation with or without the use of a controlled vocabulary.
#'
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/exPostEvaluation.html}
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
        check_attribs_in_set(names(attribs), allowed_attribs, field = "exPostEvaluation")
        check_attribs(attribs)
    }

    build_branch_node(
        "exPostEvaluation",
        allowed_children = allowed_children,
        components = components,
        attribs = attribs
    )
}

#' Evaluation Process
#' 
#' Describes the evaluation process followed.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/evaluationProcess.html}
#' 
#' @export
ddi_evaluationProcess <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "evaluationProcess")
    check_attribs(attribs)
  }

  build_leaf_node(
    "evaluationProcess",
    attribs = attribs,
    content = components$content
  )
}

#' Evaluator Type
#' 
#' The evaluator element identifies persons or organizations involved in the evaluation. The affiliation attribute contains the affiliation of the individual 
#' or organization. The abbr attribute holds an abbreviation for the individual or organization. The role attribute indicates the role played by the individual 
#' or organization in the evaluation process.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/evaluator.html}
#' 
#' @export
ddi_evaluator <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "affiliation", "abbr", "role")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "evaluator")
    check_attribs(attribs)
  }

  build_leaf_node(
    "evaluator",
    attribs = attribs,
    content = components$content
  )
}

#' Evaluator Type
#' 
#' The evaluator element identifies persons or organizations involved in the evaluation. The affiliation attribute contains the affiliation of the individual 
#' or organization. The abbr attribute holds an abbreviation for the individual or organization. The role attribute indicates the role played by the individual 
#' or organization in the evaluation process.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/outcomes.html}
#' 
#' @export
ddi_outcomes <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "outcomes")
    check_attribs(attribs)
  }

  build_leaf_node(
    "outcomes",
    attribs = attribs,
    content = components$content
  )
}

#' Quality Statement
#'
#' This structure consists of two parts, standardsCompliance and otherQualityStatements. In standardsCompliance list all specific standards complied with during 
#' the execution of this study. Note the standard name and producer and how the study complied with the standard. Enter any additional quality statements in 
#' otherQualityStatements.
#'
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/exPostEvaluation.html}
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
        check_attribs_in_set(names(attribs), allowed_attribs, field = "qualityStatement")
        check_attribs(attribs)
    }

    build_branch_node(
        "qualityStatement",
        allowed_children = allowed_children,
        components = components,
        attribs = attribs
    )
}

#' otherQualityStatement
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/otherQualityStatement.html}
#' 
#' @export
ddi_otherQualityStatement <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "otherQualityStatement")
    check_attribs(attribs)
  }

  build_leaf_node(
    "otherQualityStatement",
    attribs = attribs,
    content = components$content
  )
}

#' Standards Compliance
#'
#' This section lists all specific standards complied with during the execution of this study. Specify the standard(s)' name(s) and producer(s) and describe how 
#' the study complied with each standard in complianceDescription. Enter any additional quality statements in otherQualityStatement.
#'
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/exPostEvaluation.html}
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
        check_attribs_in_set(names(attribs), allowed_attribs, field = "standardsCompliance")
        check_attribs(attribs)
    }

    build_branch_node(
        "standardsCompliance",
        allowed_children = allowed_children,
        required_children = "standard",
        components = components,
        attribs = attribs
    )
}

#' Compliance Description
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/complianceDescription.html}
#' 
#' @export
ddi_complianceDescription <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "complianceDescription")
    check_attribs(attribs)
  }

  build_leaf_node(
    "complianceDescription",
    attribs = attribs,
    content = components$content
  )
}

#' Standard
#'
#' Describes a standard with which the study complies.
#'
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/standard.html}
#' 
#' @export
ddi_standard <- function(...) {
    allowed_children <- c(
        "producer",
        "standardName"
    )

    components <- dots_to_xml_components(...)
    attribs <- components$attribs    

    if (!is.null(attribs)) {
        allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
        check_attribs_in_set(names(attribs), allowed_attribs, field = "standard")
        check_attribs(attribs)
    }

    build_branch_node(
        "standard",
        allowed_children = allowed_children,
        components = components,
        attribs = attribs
    )
}

#' Producer
#' 
#' The producer is the person or organization with the financial or administrative responsibility for the physical processes whereby the document was brought into 
#' existence. Use the "role" attribute to distinguish different stages of involvement in the production process, such as original producer. Producer of data collection 
#' (codeBook/stdyDscr/citation/prodStmt/producer) maps to Dublin Core Publisher element. The "producer" in the Document Description should be the agency or person that 
#' prepared the marked-up document.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/producer.html}
#' 
#' @export
ddi_producer <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "abbr", "affiliation", "role")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "producer")
    check_attribs(attribs)
  }

  build_leaf_node(
    "producer",
    attribs = attribs,
    content = components$content
  )
}

#' Standard Name
#' 
#' Contains the name of the standard with which the study complies. The "date" attribute specifies the date when the standard was published, the "version" 
#' attribute includes the specific version of the standard with which the study is compliant, and the "URI" attribute includes the URI for the actual standard.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/producer.html}
#' 
#' @export
ddi_standardName <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "date", "version", "URI")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "standardName")
    check_attribs(attribs)
  }

  build_leaf_node(
    "standardName",
    attribs = attribs,
    content = components$content
  )
}

#' Study Budget
#' 
#' Describe the budget of the project in as much detail as needed. Use XHTML structure elements to identify discrete pieces of information in a way that 
#' facilitates direct transfer of information on the study budget between DDI 2 and DDI 3 structures.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/producer.html}
#' 
#' @export
ddi_studyBudget <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "studyBudget")
    check_attribs(attribs)
  }

  build_leaf_node(
    "studyBudget",
    attribs = attribs,
    content = components$content
  )
}

#' Subject
#'
#' Subject information describing the data collection's intellectual content.
#'
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/subject.html}
#' 
#' @export
ddi_subject <- function(...) {
    allowed_children <- c(
        "keyword",
        "TopcClas"
    )

    components <- dots_to_xml_components(...)
    attribs <- components$attribs    

    if (!is.null(attribs)) {
        allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
        check_attribs_in_set(names(attribs), allowed_attribs, field = "subject")
        check_attribs(attribs)
    }

    build_branch_node(
        "subject",
        allowed_children = allowed_children,
        components = components,
        attribs = attribs
    )
}

#' Keywords
#' 
#' Words or phrases that describe salient aspects of a data collection's content. Can be used for building keyword indexes and for classification and retrieval 
#' purposes. A controlled vocabulary can be employed. Maps to Dublin Core Subject element. The "vocab" attribute is provided for specification of the controlled 
#' vocabulary in use, e.g., LCSH, MeSH, etc. The "vocabURI" attribute specifies the location for the full controlled vocabulary. 
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/keyword.html}
#' 
#' @export
ddi_keyword <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "vocab", "vocabURI")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "keyword")
    check_attribs(attribs)
  }

  build_leaf_node(
    "keyword",
    attribs = attribs,
    content = components$content
  )
}

#' Topic Classification
#' 
#' The classification field indicates the broad substantive topic(s) that the data cover. Library of Congress subject terms may be used here. The 
#' "vocab" attribute is provided for specification of the controlled vocabulary in use, e.g., LCSH, MeSH, etc. The "vocabURI" attribute specifies the 
#' location for the full controlled vocabulary. Maps to Dublin Core Subject element. Inclusion of this element in the codebook is recommended.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/topcClas.html}
#' 
#' @export
ddi_topcClas <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "vocab", "vocabURI")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "topcClas")
    check_attribs(attribs)
  }

  build_leaf_node(
    "topcClas",
    attribs = attribs,
    content = components$content
  )
}

#' Summary Data Description
#'
#' Information about the and geographic coverage of the study and unit of analysis.
#'
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/sumDscr.html}
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
        check_attribs_in_set(names(attribs), allowed_attribs, field = "sumDscr")
        check_attribs(attribs)
    }

    build_branch_node(
        "sumDscr",
        allowed_children = allowed_children,
        components = components,
        attribs = attribs
    )
}

#' Unit of Analysis
#' 
#' Basic unit of analysis or observation that the file describes: individuals, families/households, groups, institutions/organizations, administrative units, 
#' etc. The "unit" attribute is included to permit the development of a controlled vocabulary for this element.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/anlyInfo.html}
#' 
#' @export
ddi_anlyUnit <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "unit")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "anlyUnit")
    check_attribs(attribs)
  }

  build_leaf_node(
    "anylUnit",
    attribs = attribs,
    content = components$content
  )
}

#' Geographic Bounding Polygon
#'
#' This field allows the creation of multiple polygons to describe in a more detailed manner the geographic area covered by the dataset. It should only 
#' be used to define the outer boundaries of a covered area. For example, in the United States, such polygons can be created to define boundaries for Hawaii, 
#' Alaska, and the continental United States, but not interior boundaries for the contiguous states. This field is used to refine a coordinate-based search, 
#' not to actually map an area.
#' 
#' If the boundPoly element is used, then geoBndBox MUST be present, and all points enclosed by the boundPoly MUST be contained within the geoBndBox. Elements 
#' westBL, eastBL, southBL, and northBL of the geoBndBox should each be represented in at least one point of the boundPoly description.
#'
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/sumDscr.html}
#' 
#' @export
ddi_boundPoly <- function(...) {
    components <- dots_to_xml_components(...)
    attribs <- components$attribs    

    if (!is.null(attribs)) {
        allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
        check_attribs_in_set(names(attribs), allowed_attribs, field = "boundPoly")
        check_attribs(attribs)
    }

    build_branch_node(
        "boundPoly",
        allowed_children = "polygon",
        required_children = "polygon",
        components = components,
        attribs = attribs
    )
}

#' Polygon
#' 
#' The minimum polygon that covers a geographical area, and is delimited by at least 4 points (3 sides), in which the last point coincides with the first point.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/polygon.html}
#' 
#' @export
ddi_polygon <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "polygon")
    check_attribs(attribs)
  }

  build_branch_node(
    "polygon",
    allowed_children = "point",
    required_children = "point",
    attribs = attribs,
    content = components$content
  )
}

#' Point
#' 
#' 0-dimensional geometric primitive, representing a position, but not having extent. In this declaration, point is limited to a longitude/latitude coordinate system.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/point.html}
#' 
#' @export
ddi_point <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "point")
    check_attribs(attribs)
  }

  build_branch_node(
    "point",
    allowed_children = c("gringLat", "gringLong"),
    required_children = c("gringLat", "gringLong"),
    attribs = attribs,
    content = components$content
  )
}

#' G-Ring Latitude
#' 
#' Latitude (y coordinate) of a point. Valid range expressed in decimal degrees is as follows: -90,0 to 90,0 degrees (latitude).
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/gringLat.html}
#' 
#' @export
ddi_gringLat <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "gringLat")
    check_attribs(attribs)
  }

  if(components$content < -90 || components$content > 90) rddi_err("gringLat must be between -90.0 and 90.0")

  build_leaf_node(
    "gringLat",
    attribs = attribs,
    content = components$content
  )
}

#' G-Ring Longitude
#' 
#' Longitude (x coordinate) of a point. Valid range expressed in decimal degrees is as follows: -180,0 to 180,0 degrees (longitude)
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/gringLon.html}
#' 
#' @export
ddi_gringLon <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "gringLon")
    check_attribs(attribs)
  }

  if(components$content < -180 || components$content > 180) rddi_err("gringLat must be between -180.0 and 180.0")

  build_leaf_node(
    "gringLon",
    attribs = attribs,
    content = components$content
  )
}

#' Date of Collection
#' 
#' Contains the date(s) when the data were collected. Use the event attribute to specify "start", "end", or "single" for each date entered. The ISO 
#' standard for dates (YYYY-MM-DD) is recommended for use with the "date" attribute. The "cycle" attribute permits specification of the relevant cycle, 
#' wave, or round of data. Maps to Dublin Core Coverage element. Inclusion of this element in the codebook is recommended.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/collDate.html}
#' 
#' @export
ddi_collDate <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "date", "event", "cycle")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "collDate")
    check_attribs(attribs)
    if("event" %in% attribs$event) check_attribs_in_set(attribs$event, c("start", "end", "single"), field = "event (collDate")
  }

  build_leaf_node(
    "collDate",
    attribs = attribs,
    content = components$content
  )
}

#' Kind of Data
#' 
#' The type of data included in the file: survey data, census/enumeration data, aggregate data, clinical data, event/transaction data, program 
#' source code, machine-readable text, administrative records data, experimental data, psychological test, textual data, coded textual, coded 
#' documents, time budget diaries, observation data/ratings, process-produced data, etc. This element maps to Dublin Core Type element. The type 
#' attribute can be used for forward-compatibility with DDI 3, by providing a type for use of controlled vocabulary, as this is descriptive in 
#' DDI 2 and CodeValue in DDI 3.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataKind.html}
#' 
#' @export
ddi_dataKind<- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "type")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "dataKind")
    check_attribs(attribs)
  }

  build_leaf_node(
    "dataKind",
    attribs = attribs,
    content = components$content
  )
}

#' Geographic Bounding Box
#' 
#' The fundamental geometric description for any dataset that models geography. GeoBndBox is the minimum box, defined by west and east 
#' longitudes and north and south latitudes, that includes the largest geographic extent of the dataset's geographic coverage. This 
#' element is used in the first pass of a coordinate-based search. If the boundPoly element is included, then the geoBndBox element MUST be included.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/point.html}
#' 
#' @export
ddi_geoBndBox <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "geoBndBox")
    check_attribs(attribs)
  }

  allowed_children (
      "westBL",
      "eastBL",
      "southBL",
      "northBL"
  )
  build_branch_node(
    "geoBndBox",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' East Bounding Longitude
#' 
#' The easternmost coordinate delimiting the geographic extent of the dataset. A valid range of values, expressed in decimal degrees (positive east and 
#' positive north), is: -180,0 <= East Bounding Longitude Value <= 180,0
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/eastBL.html}
#' 
#' @export
ddi_eastBL <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "eastBL")
    check_attribs(attribs)
  }

  if(components$content < -180 || components$content > 180) rddi_err("eastBL must be between -180.0 and 180.0")

  build_leaf_node(
    "eastBL",
    attribs = attribs,
    content = components$content
  )
}

#' North Bounding Longitude
#' 
#' The northernmost coordinate delimiting the geographic extent of the dataset. A valid range of values, expressed in decimal degrees (positive east and 
#' positive north), is: -90,0 <= North Bounding Latitude Value <= 90,0 ; North Bounding Latitude Value = South Bounding Latitude Value
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/northBL.html}
#' 
#' @export
ddi_northBL <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "northBL")
    check_attribs(attribs)
  }

  if(components$content < -90 || components$content > 90) rddi_err("northBL must be between -90.0 and 90.0")

  build_leaf_node(
    "northBL",
    attribs = attribs,
    content = components$content
  )
}

#' South Bounding Longitude
#' 
#' The southernmost coordinate delimiting the geographic extent of the dataset. A valid range of values, expressed in decimal degrees (positive 
#' east and positive north), is: -90,0 <=South Bounding Latitude Value <= 90,0 ; South Bounding Latitude Value <= North Bounding Latitude Value
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/southBL.html}
#' 
#' @export
ddi_southBL <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "southBL")
    check_attribs(attribs)
  }

  if(components$content < -90 || components$content > 90) rddi_err("southBL must be between -90.0 and 90.0")

  build_leaf_node(
    "southBL",
    attribs = attribs,
    content = components$content
  )
}

#' West Bounding Longitude
#' 
#' The westernmost coordinate delimiting the geographic extent of the dataset. A valid range of values, expressed in decimal degrees (positive 
#' east and positive north), is: -180,0 <=West Bounding Longitude Value <= 180,0
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/westBL.html}
#' 
#' @export
ddi_westBL <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "westBL")
    check_attribs(attribs)
  }

  if(components$content < -180 || components$content > 100) rddi_err("westBL must be between -180.0 and 180.0")

  build_leaf_node(
    "westBL",
    attribs = attribs,
    content = components$content
  )
}

#' Geographic Coverage
#' 
#' Information on the geographic coverage of the data. Includes the total geographic scope of the data, and any additional levels of geographic 
#' coding provided in the variables. Maps to Dublin Core Coverage element. Inclusion of this element in the codebook is recommended. For forward-compatibility, 
#' DDI 3 XHTML tags may be used in this element.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/geogCover.html}
#' 
#' @export
ddi_geogCover <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "geogCover")
    check_attribs(attribs)
  }

  build_leaf_node(
    "geogCover",
    attribs = attribs,
    content = components$content
  )
}

#' Geographic Unit
#' 
#' Lowest level of geographic aggregation covered by the data.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/geogUnit.html}
#' 
#' @export
ddi_geogUnit <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "geogUnit")
    check_attribs(attribs)
  }

  build_leaf_node(
    "geogUnit",
    attribs = attribs,
    content = components$content
  )
}

#' Country
#' 
#' Indicates the country or countries covered in the file. Attribute "abbr" may be used to list common abbreviations; use of ISO country codes is 
#' recommended. Maps to Dublin Core Coverage element. Inclusion of this element is recommended. For forward-compatibility, DDI 3 XHTML tags may 
#' be used in this element.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/nation.html}
#' 
#' @export
ddi_nation <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "abbr")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "nation")
    check_attribs(attribs)
  }

  build_leaf_node(
    "nation",
    attribs = attribs,
    content = components$content
  )
}

#' Time Period Covered
#' 
#' The time period to which the data refer. This item reflects the time period covered by the data, not the dates of coding or making documents 
#' machine-readable or the dates the data were collected. Also known as span. Use the event attribute to specify "start", "end", or "single" for 
#' each date entered. The ISO standard for dates (YYYY-MM-DD) is recommended for use with the "date" attribute. The "cycle" attribute permits 
#' specification of the relevant cycle, wave, or round of data. Maps to Dublin Core Coverage element. Inclusion of this element is recommended. 
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/timePrd.html}
#' 
#' @export
ddi_timePrd <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "date", "event", "cycle")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "timePrd")
    if("event" %in% attribs) check_attribs_in_set(c("start", "end", "single"), field = "event (timePrd)")
    check_attribs(attribs)
  }

  build_leaf_node(
    "timePrd",
    attribs = attribs,
    content = components$content
  )
}