#' stdyInfo and its leaf node children
#' 
#'
#' stdyInfo is the study scope. It contains information about the data collection's scope 
#' across several dimensions, including substantive content, geography, and time. More information
#' on the allowed attributes for stdyINfo and its child nodes can be found below and in the references.
#' #'
#' @param ... Child nodes or attributes. 
#' 
#' @section Branch node children allowed:
#' * [ddi_exPostEvaluation()]
#' * [ddi_qualityStatmeent()]
#' * [ddi_subject()]
#' * [ddi_sumDscr()]
#' 
#' @section General children allowed:
#' * [ddi_notes()]
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/stdyInfo.html}{stdyInfo documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/abstract.html}{abstract documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/studyBudget.html}{studyBudget documentation}
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

#' exPostEvaluation and its leaf node children
#'
#' Post Evaluation Procedures describes evaluation procedures not address in data evaluation processes. These may include issues such as timing of the study, sequencing 
#' issues, cost/budget issues, relevance, instituional or legal arrangments etc. of the study. More information on hte allowed attributes and child nodes can be found below
#' and in the references
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/exPostEvaluation.html}{exPostEvaluation documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/evaluationProcess.html}{evaluationProcess documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/evaluator.html}{evaluator documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/outcomes.html}{outcomes documentation}
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

#' qualityStatement
#'
#' The Quality Statment consists of two parts, standardsCompliance and otherQualityStatements. In standardsCompliance list all specific standards complied with during 
#' the execution of this study. Note the standard name and producer and how the study complied with the standard. Enter any additional quality statements in 
#' otherQualityStatements. More information on the allowed attributes for qualityStatement and its child nodes can be found below 
#' and in the references
#'
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section Branch node children allowed:
#' * [ddi_standardsCompliance()]
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/qualityStatement.html}{qualityStatment documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/otherQualityStatement.html}{otherQualityStatement documentation}
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

#' standardsCompliance and its leaf node chidlren
#'
#' The standards compliance section lists all specific standards complied with during the execution of this study. Specify the standard(s)' name(s) and producer(s) and describe how 
#' the study complied with each standard in complianceDescription. More information can be found below and in the references.
#'
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section Branch node children allowed:
#' *[ddi_standard()]
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/standardsCompliance.html}{standardsCompliance documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/complianceDescription.html}{complianceDescription documentation}
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

#' standard
#'
#' Standard describes a standard with which the study complies. More information on the element, its attributes and its child nodes can be found
#' below and in the references
#'
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section General children allowed:
#' *[ddi_producer()]
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/standard.html}{standard documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/standardName.html}{standardName documentation}
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

#' subject
#'
#' Subject describes the data collection's intellectual content. More information on the allowed attributes and child nodes for subject
#' can be found below and in the references.
#'
#' @param ... Child nodes or attributes. 
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/subject.html}{subject documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/keyword.html}{keyword documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/topcClas.html}{topcClas documentation}
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

#' sumDscr and its children
#'
#' This is the summary data description and it contains information about the geographic coverage of the study and 
#' unit of analysis.
#'
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section Branch node children allowed:
#' * [ddi_boundPoly()]
#' * [ddi_geoBndBox()]
#' 
#' @section General children allowed:
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
    allowed_children = c("txt", "concept")
    build_branch_node(
      "anlyUnit",
      content = unwrap_content(components$content),
      attribs = components$attribs,
      allowed_children = allowed_children
    )
  }
  
}

#' boundPoly and its leaf node children
#'
#' The geographic bounding polygon field allows the creation of multiple polygons to describe in a more detailed manner the geographic area covered by the dataset. It should only 
#' be used to define the outer boundaries of a covered area. For example, in the United States, such polygons can be created to define boundaries for Hawaii, 
#' Alaska, and the continental United States, but not interior boundaries for the contiguous states. This field is used to refine a coordinate-based search, 
#' not to actually map an area.
#' 
#' If the boundPoly element is used, then geoBndBox MUST be present, and all points enclosed by the boundPoly MUST be contained within the geoBndBox. Elements 
#' westBL, eastBL, southBL, and northBL of the geoBndBox should each be represented in at least one point of the boundPoly description.
#'
#' @param ... Child nodes or attributes. 
#'
#' @section Branch node children:
#' * [ddi_polygon()] - [ddi_polygon()] has [ddi_point()] as a branch node child
#'
#' @section ddi_point leaf nodes:
#' * [ddi_gringLat()]
#' * [ddi_gringLong()]
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/boundPoly.html}{boundPoly documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/polygon.html}{polygon documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/point.html}{point documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/gringLat.html}{gringLat documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/gringLon.html}{gringLon documentation}
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

#' @rdname ddi_boundPoly
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

#' @references ddi_boundPoly
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
    allowed_children = c("gringLat", "gringLong"),
    required_children = c("gringLat", "gringLong"),
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_boundPoly
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

#' @rdname ddi_boundPoly 
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
    allowed_children = c("txt", "concept")
    build_branch_node(
      "dataKind",
      content = unwrap_content(components$content),
      attribs = components$attribs,
      allowed_children = allowed_children
    )
  }
  
}

#' geoBndBox and its leaf node children
#' 
#' The fundamental geometric description for any dataset that models geography. GeoBndBox is the minimum box, defined by west and east 
#' longitudes and north and south latitudes, that includes the largest geographic extent of the dataset's geographic coverage. This 
#' element is used in the first pass of a coordinate-based search. If the boundPoly element is included, then the geoBndBox element MUST be included.
#' More informaiton on geoBndBox, its attributes and its children can be found in the references
#' 
#' @param ... Child nodes or attributes. 
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/geoBndBox.html}{geoBndBox documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/eastBL.html}{eastBL documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/northBL.html}{northBL documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/southBL.html}{southBL documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/westBL.html}{westBL documentation}
#' 
#' @export
ddi_geoBndBox <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "geoBndBox")
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
    allowed_children = c("txt", "concept")
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
    allowed_children = c("txt", "concept")
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
    allowed_children = c("txt", "concept")
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