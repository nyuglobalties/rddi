#' var and its child nodes
#'
#' This element describes all of the features of a single variable in a social 
#' science data file. The following elements are repeatable to support 
#' multi-language content: anlysUnit, embargo, imputation, respUnit, security, 
#' TotlResp. More information on these elements, especially their allowed 
#' attributes, can be found in the references. 
#' 
#' \emph{Parent nodes}
#' 
#' `var` is contained in `dataDscr`.
#' 
#' \emph{var specific child nodes}
#' 
#' * `ddi_catLevel()` is used to describe the levels of the category hierarchy. 
#' 
#' * `ddi_codInstr()` are coder instructions. These are any special instructions 
#' to those who converted information from one form to another for a particular 
#' variable. This might include the reordering of numeric information into 
#' another form or the conversion of textual information into numeric information.
#' 
#' * `ddi_geomap()` is a geographic map. This element is used to point, using a 
#' "URI" attribute, to an external map that displays the geography in question.
#' 
#' * `ddi_stdCatgry()` are standard category codes used in the variable, like 
#' industry codes, employment codes, or social class codes.
#' 
#' * `ddi_sumStat()` is one or more statistical measures that describe the 
#' responses to a particular variable and may include one or more standard 
#' summaries, e.g., minimum and maximum values, median, mode, etc. 
#' 
#' * `ddi_TotlResp()` are the number of responses to this variable. This element 
#' might be used if the number of responses does not match added case counts. 
#' It may also be used to sum the frequencies for variable categories.
#' 
#' * `ddi_undocCod()` is the list of undocumented codes where the meaning of the 
#' values are unknown.
#' 
#' * `ddi_varFormat()` is the technical format of the variable in question. 
#'  
#' @section Shared and complex child nodes:
#' * [ddi_anlysUnit()]
#' * [ddi_catgry()]
#' * [ddi_catgryGrp()]
#' * [ddi_concept()]
#' * [ddi_derivation()]
#' * [ddi_embargo()]
#' * [ddi_imputation()]
#' * [ddi_invalrng()]
#' * [ddi_labl()] 
#' * [ddi_location()]
#' * [ddi_notes()] 
#' * [ddi_qstn()]
#' * [ddi_respUnit()]
#' * [ddi_security()]
#' * [ddi_txt()]
#' * [ddi_universe()]
#' * [ddi_valrng()]
#' * [ddi_verStmt()] 
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/var.html}{var documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/catLevel.html}{catLevel documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/codInstr.html}{codInstr documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/geoMap.html}{geomap documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/stdCatgry.html}{stdCatgry documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/sumStat.html}{sumStat documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/TotlResp.html}{TotlResp documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/undocCod.html}{undocCod documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/varFormat.html}{varFormat documentation}
#' 
#' @param varname The variable name.
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#' 
#' @examples 
#' ddi_var(varname = "var01")
#' 
#' # Functions that need to be wrapped in ddi_var()
#' 
#' ddi_catLevel(ID = "Level1", levelnm = "Broader sectors")
#' 
#' ddi_codInstr("Use the standard classification tables to present responses to 
#'               the question: What is your occupation? into numeric codes.")
#'               
#' ddi_geomap(URI = "https://mapURL.com")
#'          
#' ddi_stdCatgry(date = "1981",
#'               "U. S. Census of Population and Housing, Classified Index of 
#'               Industries and Occupations")
#'               
#' ddi_sumStat(type = "min", "0")
#' 
#' ddi_TotlResp("1,056")
#' 
#' ddi_undocCod("Responses for categories 9 and 10 are unavailable.")
#' 
#' ddi_varFormat(type = "numeric",
#'               formatname = "date.iso8601",
#'               schema = "XML-Data",
#'               category = "date",
#'               URI = "http://www.w3.org/TR/1998/NOTE-XML-data/",
#'               "19541022")
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

  allowed_children <- c("location",
                        "labl",
                        "imputation",
                        "security", 
                        "embargo",
                        "respUnit",
                        "anlysUnit",
                        "qstn",
                        "valrng",
                        "invalrng",
                        "UndocCod",
                        "universe",
                        "TotlResp",
                        "sumStat",
                        "txt",
                        "stdCatgry",
                        "catgryGrp",
                        "catgry",
                        "codInstr",
                        "verStmt",
                        "concept",
                        "derivation",
                        "varFormat",
                        "geoMap",
                        "catLevel",
                        "notes"
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

#' varGrp and its child nodes
#'
#' A group of variables that may share a common subject, arise from the 
#' interpretation of a single question, or are linked by some other factor. 
#' Variable groups are created this way in order to permit variables to belong 
#' to multiple groups, including multiple subject groups such as a group of 
#' variables on sex and income, or to a subject and a multiple response group, 
#' without causing overlapping groups. Variables that are linked by use of the 
#' same question need not be identified by a Variable Group element because they 
#' are linked by a common unique question identifier in the Variable element. 
#' Note that as a result of the strict sequencing required by XML, all Variable 
#' Groups must be marked up before the Variable element is opened. That is, the 
#' mark-up author cannot mark up a Variable Group, then mark up its constituent 
#' variables, then mark up another Variable Group. More information on these 
#' elements, especially their allowed attributes, can be found in the 
#' references. 
#' 
#' \emph{Parent nodes}
#' 
#' `varGrp` is contained in `dataDscr`.
#' 
#' \emph{varGrp specific child nodes}
#' 
#' * `ddi_defntn()` is the rationale for why the variable group was constituted. 
#' 
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#'
#' @section Shared and complex child nodes:
#' * [ddi_concept()]
#' * [ddi_labl()]
#' * [ddi_notes()] 
#' * [ddi_txt()]
#' * [ddi_universe()]
#' 
#' @examples 
#' ddi_varGrp()
#' 
#' # Functions that need to be wrapped in ddi_varGrp()
#' 
#' ddi_defntn("The following eight variables were only asked in Ghana.")
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/varGrp.html}{varGrp documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/defntn.html}{defntn documentation}
#'
#' @export
ddi_varGrp <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c(
      "ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", 
      "type", "otherType", "var", "varGrp", "name", "sdatrefs", "methrefs", "pubrefs", "access" 
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

#' nCube and its child nodes
#'
#' Describes the logical structure of an n-dimensional array, in which each 
#' coordinate intersects with every other dimension at a single point. The 
#' nCube has been designed for use in the markup of aggregate data. Repetition 
#' of the following elements is provided to support multi-language content: 
#' anlysUnit, embargo, imputation, purpose, respUnit, and security. More 
#' information on these elements, especially their allowed attributes, can be 
#' found in the references. 
#' 
#' \emph{Parent nodes}
#' 
#' `nCube` is contained in `dataDscr`.
#' 
#' \emph{nCube specific child nodes}
#' 
#' * `ddi_measure()` indicates the measurement features of the cell content: type of 
#' aggregation used, measurement unit, and measurement scale. An origin point 
#' is recorded for anchored scales, to be used in determining relative movement 
#' along the scale. Additivity indicates whether an aggregate is a stock 
#' (like the population at a given point in time) or a flow (like the number of 
#' births or deaths over a certain period of time). The non-additive flag is to 
#' be used for measures that for logical reasons cannot be aggregated to a 
#' higher level - for instance, data that only make sense at a certain level of 
#' aggregation, like a classification. Two nCubes may be identical except for 
#' their measure - for example, a count of persons by age and percent of 
#' persons by age. Measure is an empty element.
#' 
#' * `ddi_purpose()` explains the purpose for which a particular nCube was created.
#' 
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#'
#' @section Shared and complex child nodes:
#' 
#' * [ddi_anlysUnit()]
#' * [ddi_embargo()]
#' * [ddi_imputation()]
#' * [ddi_labl()]
#' * [ddi_location()]
#' * [ddi_notes()]
#' * [ddi_respUnit()]
#' * [ddi_security()]
#' * [ddi_txt()]
#' * [ddi_universe()]
#' * [ddi_verStmt()]
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/nCube.html}{nCube documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/measure.html}{measure documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/purpose.html}{purpose documentation}
#' 
#' @examples 
#' ddi_nCube()
#' 
#' # Functions that need to be wrapped in ddi_nCube()
#' 
#' ddi_measure(aggrMeth = "sum", additivity = "stock")
#' 
#' ddi_purpose("Meets reporting requirements for the Federal Reserve Board")
#' 
#' @export
ddi_nCube <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c(
      "ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", 
      "name", "sdatrefs", "methrefs", "pubrefs", "access", "dmnsQnty", "cellQnty"
    )
    attribs <- validate_attributes(attribs, allowed_attribs, "nCube")    
  }
  
  allowed_children <- c(
    "location",
    "labl",
    "txt",
    "universe",
    "imputation",
    "security",
    "embargo",
    "respUnit",
    "anlysUnit",
    "verStmt",
    "purpose",
    "dmns",
    "measure",
    "notes"
  )
  
  build_branch_node(
    "nCube",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' nCubeGrp and its child nodes
#'
#' A group of nCubes that may share a common subject, arise from the 
#' interpretation of a single question, or are linked by some other factor. 
#' This element makes it possible to identify all nCubes derived from a simple 
#' presentation table, and to provide the original table title and universe, 
#' as well as reference the source. Specific nesting patterns can be described 
#' using the attribute nCubeGrp. nCube groups are also created this way in 
#' order to permit nCubes to belong to multiple groups, including multiple 
#' subject groups, without causing overlapping groups. nCubes that are linked 
#' by the same use of the same variable need not be identified by an nCubeGrp 
#' element because they are already linked by a common variable element. Note 
#' that as a result of the strict sequencing required by XML, all nCube Groups 
#' must be marked up before the Variable element is opened. That is, the 
#' mark-up author cannot mark up a nCube Group, then mark up its constituent 
#' nCubes, then mark up another nCube Group. More information on these elements, 
#' especially their allowed attributes, can be found in the references. 
#' 
#' \emph{Parent nodes}
#' 
#' `nCubeGrp` is contained in `dataDscr`.
#' 
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#'
#' @section Shared and complex child nodes:
#' 
#' * [ddi_concept()]
#' * [ddi_defntn()]
#' * [ddi_labl()]
#' * [ddi_notes()]
#' * [ddi_txt()]
#' * [ddi_universe()]
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/nCubeGrp.html}{nCubeGrp documentation}
#'
#' @examples 
#' ddi_nCubeGrp(name = "Group 1")
#'
#' @export
ddi_nCubeGrp <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c(
      "ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", 
      "type", "otherType", "nCube", "nCubeGrp", "name", "sdatrefs", "methrefs", "pubrefs", "access"
    )
    attribs <- validate_attributes(attribs, allowed_attribs, "nCubeGrp")
    if(!is.null(attribs$type)) {
      check_attribs_in_set(attribs$type, c("section", 
                                           "multipleResp", 
                                           "grid", 
                                           "display", 
                                           "repetition", 
                                           "subject", 
                                           "version", 
                                           "iteration", 
                                           "analysis",
                                           "pragmatic",
                                           "record",
                                           "file",
                                           "randomized",
                                           "other"
                                           ), 
                           field = "type (nCubeGrp)")
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
    "nCubeGrp",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_nCube
#' @export
ddi_measure <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                         "varRef", "aggrMeth", "otherAggrMeth", "measUnit", "scale", "origin", "additivity"
                         )
    attribs <- validate_attributes(attribs, allowed_attribs, "measure")
  }
  
  build_leaf_node(
    "measure",
    attribs = attribs
  )
}

#' @rdname ddi_nCube
#' @export
ddi_purpose <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                         "sdatrefs", "methrefs", "pubrefs", "URI"
    )
    attribs <- validate_attributes(attribs, allowed_attribs, "purpose")
  }
  
  build_leaf_node(
    "purpose",
    attribs = attribs,
    content = components$content
  )
}

#' dmns and its child nodes
#'
#' This element defines a variable as a dimension of the nCube, and should be 
#' repeated to describe each of the cube's dimensions. The attribute "rank" is 
#' used to define the coordinate order (rank="1", rank="2", etc.). The attribute 
#' "varRef" is an IDREF that points to the variable that makes up this dimension 
#' of the nCube. More information on these elements, especially their allowed 
#' attributes, can be found in the references. 
#' 
#' \emph{Parent nodes}
#' 
#' `dmns` is contained in `nCube`.
#' 
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#'
#' @section Shared and complex child nodes:
#' 
#' * [ddi_cohort()]
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dmns.html}{dmns documentation}
#' 
#' @examples 
#' ddi_dmns(rank = "1", varRef = "var01")
#' 
#' @export
ddi_dmns <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c(
      "ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", 
      "rank", "varRef"
    )
    attribs <- validate_attributes(attribs, allowed_attribs, "dmns")    
  }
  
  allowed_children <- c(
    "cohort"
  )
  
  build_branch_node(
    "dmns",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' cohort and its child nodes
#'
#' The element cohort is used when the nCube contains a limited number of 
#' categories from a particular variable, as opposed to the full range of 
#' categories. The attribute "catRef" is an IDREF to the actual category 
#' being used. The attribute "value" indicates the actual value attached to 
#' the category that is being used. More information on these elements, 
#' especially their allowed attributes, can be found in the references. 
#' 
#' \emph{Parent nodes}
#' 
#' `cohort` is contained in `dmns`.
#' 
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#'
#' @section Shared and complex child nodes:
#' 
#' * [ddi_range()]
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/cohort.html}{cohort documentation}
#' 
#' @examples 
#' ddi_cohort(catRef = "CV24_1", value = "1")
#'
#' @export
ddi_cohort <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c(
      "ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", 
      "catRef", "value"
    )
    attribs <- validate_attributes(attribs, allowed_attribs, "cohort")    
  }
  
  allowed_children <- c(
    "range"
  )
  
  build_branch_node(
    "cohort",
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


#' catgry, catgryGrp and their child nodes
#'
#' `catgry` is a description of a particular categorical response. 
#' `ddi_catgryGrp()` groups the responses together. More information on these 
#' elements, especially their allowed attributes, can be found in the references. 
#' 
#' \emph{Parent nodes}
#' 
#' `catgry` and `catgryGrp` is contained in `var`.
#' 
#' \emph{catgry and catgryGrp specific child nodes}
#' 
#' * `ddi_catStat()` is a category level statistic. May include frequencies, 
#' percentages, or crosstabulation results. The attribute "type" indicates the 
#' type of statistics presented - frequency, percent, or crosstabulation. If a 
#' value of "other" is used for this attribute, the "otherType" attribute should 
#' take a value from a controlled vocabulary. This option should only be used 
#' when applying a controlled vocabulary to this attribute. Use the complex 
#' element controlledVocabUsed to identify the controlled vocabulary to which 
#' the selected term belongs.
#' 
#' \emph{catgry specific child nodes}
#' 
#' * `ddi_catValu()` is the category value.
#' 
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#' 
#' @section Shared and complex child nodes:
#' * [ddi_labl()] 
#' * [ddi_mrow()]
#' * [ddi_txt()]
#'  
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/catgry.html}{catgry documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/catgryGrp.html}{catgryGrp documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/catStat.html}{catStat documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/catValu.html}{catValu documentation}
#' 
#' @examples 
#' ddi_catgry(missing = "Y", missType = "inap")
#' ddi_catgryGrp(missing = "N")
#' 
#' # Functions that need to be wrapped in ddi_catgry() or ddi_catgryGrp()
#' 
#' ddi_catStat(type = "freq", "256")
#' 
#' # Functions that need to be wrapped in ddi_catgry()
#' 
#' ddi_catValu("9")
#'
#' @export
ddi_catgry <- function(...)  {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  allowed_children <- c(
    "catValu", 
    "labl",
    "txt",
    "catStat",
    "mrow" 
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

#' mrow and its child nodes 
#'
#' mrow or Mathematical Row is a wrapper containing the presentation expression 
#' `mi`. It creates a single string without spaces consisting of the individual 
#' elements described within it. It can be used to create a single variable by 
#' concatenating other variables into a single string. It is used to create 
#' linking variables composed of multiple non-contiguous parts, or to define 
#' unique strings for various category values of a single variable. More 
#' information on these elements, especially their allowed attributes, can be 
#' found in the references. 
#' 
#' \emph{Parent nodes}
#' 
#' `mrow` is contained in `catgry`.
#' 
#' \emph{mrow specific child nodes}
#' 
#' * `ddi_mi()` is the mathematical identifier. This is the token element 
#' containing the smallest unit in the mrow that carries meaning.
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/mrow.html}{mrow documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/mi.html}{mi documentation}
#' 
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#' 
#' @examples 
#' ddi_mrow()
#' 
#' # Functions that need to be wrapped in ddi_mrow()
#' 
#' ddi_mi("1")
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

#' anlysUnit node
#' 
#' Provides information regarding whom or what the variable/nCube describes. 
#' The element may be repeated only to support multiple language expressions of 
#' the content. More information on this element, especially its allowed 
#' attributes, can be found in the references.
#' 
#' \emph{Parent nodes}
#' 
#' `anlysUnit` is contained in `nCube` and `var`.
#' 
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/anlysUnit.html}{anlysUnit documentation}
#' 
#' @examples 
#' ddi_anlysUnit("This variable reports election returns at the constituency level.")
#' 
#' @export
ddi_anlysUnit <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "anlysUnit")
  }

  if(length(components$content) == 1 & is.character(components$content[[1]])) {
    build_leaf_node(
      "anlysUnit",
      attribs = attribs,
      content = components$content
    )
  } else {
    allowed_children = c("concept", "txt")
    build_branch_node(
      "anlysUnit",
      content = unwrap_content(components$content),
      attribs = attribs,
      allowed_children = allowed_children
    )
  }
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

#' derivation and its child nodes
#' 
#' Used only in the case of a derived variable, this element provides both a 
#' description of how the derivation was performed and the command used to 
#' generate the derived variable, as well as a specification of the other 
#' variables in the study used to generate the derivation. The "var" attribute 
#' provides the ID values of the other variables in the study used to generate 
#' this derived variable. More information on these elements, especially their 
#' allowed attributes, can be found in the references. 
#' 
#' \emph{Parent nodes}
#' 
#' `derivation` is included in `var`.
#' 
#' \emph{derivation specific child nodes}
#' 
#' * `ddi_drvcmd()` is the actual command used to generate the derived variable. 
#' The "syntax" attribute is used to indicate the command language employed 
#' (e.g., SPSS, SAS, Fortran, etc.). The element may be repeated to support 
#' multiple language expressions of the content.
#' 
#' * `ddi_drvdesc()` is a textual description of the way in which this variable 
#' was derived. The element may be repeated to support multiple language 
#' expressions of the content.
#' 
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/derivation.html}{derivation documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/drvcmd.html}{drvcmd documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/drvdesc.html}{drvdesc documentation}
#' 
#' @examples 
#' ddi_derivation()
#' 
#' # Functions that need to be wrapped in ddi_derivation()
#' 
#' ddi_drvcmd(syntax = "SPSS",
#'            "RECODE V1 TO V3 (0=1) (1=0) (2=-1) INTO DEFENSE WELFARE HEALTH.")
#' 
#' ddi_drvdesc("VAR215.01 'Outcome of first pregnancy' (1988 NSFG=VAR611 PREGOUT1) 
#'             If R has never been pregnant (VAR203 PREGNUM EQ 0) then OUTCOM01 is 
#'             blank/inapplicable. Else, OUTCOM01 is transferred from VAR225 
#'             OUTCOME for R's 1st pregnancy.")
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

#' valrng, invalrng, and their child nodes
#'
#' Values for a particular variable that represent legitimate responses 
#' (valrng) or illegitimate response (invalrng). Must include item or range as 
#' a child element.
#' 
#' \emph{Parent nodes}
#' 
#' `valrng` and `invalrng` are contained in `var`.
#' 
#' \emph{valrng and invalrng specific child nodes}
#' 
#' `ddi_item()` is the counterpart to range; used to encode individual values. 
#' This is an empty element consisting only of its attributes. The "UNITS" 
#' attribute permits the specification of integer/real numbers. The "VALUE" 
#' attribute specifies the actual value.
#' 
#' `ddi_key()` is the range key. This element permits a listing of the category 
#' values and labels. While this information is coded separately in the Category 
#' element, there may be some value in having this information in proximity to 
#' the range of valid and invalid values. A table is permissible in this element. 
#' 
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#'
#' @section Shared and complex child nodes:
#' * [ddi_notes()] 
#' * [ddi_range()]
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/valrng.html}{valrng documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/invalrng.html}{invalrng documentation} 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/item.html}{item documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/key.html}{key documentation}
#' 
#' @examples 
#' # ddi_valrng() and ddi_invalrng() requires either the ddi_item() or ddi_range() child node.
#' 
#' ddi_valrng(ddi_item())
#' 
#' ddi_invalrng(ddi_item())
#' 
#' ddi_valrng(ddi_range())
#' 
#' ddi_invalrng(ddi_range())
#' 
#' # Functions that must be wrapped in ddi_valrng() or ddi_invalrng()
#' 
#' ddi_item(VALUE = "1")
#' 
#' ddi_key("05 (PSU) Parti Socialiste Unifie et extreme gauche (Lutte Ouvriere) 
#'         [United Socialists and extreme left (Workers Struggle)] 50 Les Verts 
#'         [Green Party] 80 (FN) Front National et extreme droite [National Front 
#'         and extreme right]")
#' 
#' @export
ddi_valrng <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn") 
    attribs <- validate_attributes(attribs, allowed_attribs, "valrng")
  }

  if(check_cardinality(components$content, "item") > 0 & check_cardinality(components$content, "range") == 0) {
    required_children <- "item"
  } else if(check_cardinality(components$content, "item") == 0 & check_cardinality(components$content, "range") > 0) {
    required_children <- "range"
  }
  else {
    rddi_err("valrng requires at least one item or at least one range child but cannot include an item and range child")
  }
  
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

#' imputation node
#' 
#' According to the Statistical Terminology glossary maintained by the National 
#' Science Foundation, this is "the process by which one estimates missing 
#' values for items that a survey respondent failed to provide," and if 
#' applicable in this context, it refers to the type of procedure used. When 
#' applied to an nCube, imputation takes into consideration all of the 
#' dimensions that are part of that nCube. This element may be repeated to 
#' support multiple language expressions of the content. More information on 
#' this element, especially its allowed attributes, can be found in the 
#' references.
#' 
#' \emph{Parent nodes}
#' 
#' `imputation` is contained in `nCube` and `var`.
#' 
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/imputation.html}{imputation documentation}
#' 
#' @examples 
#' ddi_imputation("This variable contains values that were derived by substitution.")
#' 
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

#' qstn and its child nodes
#' 
#' * `ddi_qstn()` is the question asked. The element may have mixed content. The 
#' element itself may contain text for the question, with the subelements being 
#' used to provide further information about the question. Alternatively, the 
#' question element may be empty and only the subelements used. The element has 
#' a unique question ID attribute which can be used to link a variable with 
#' other variables where the same question has been asked. This would allow 
#' searching for all variables that share the same question ID, perhaps because 
#' the questions was asked several times in a panel design.
#' 
#' #' \emph{Parent nodes}
#' 
#' `qstn` is contained in `var`.
#' 
#' \emph{qstn specific child nodes}
#' 
#' * `ddi_backward()` contains a reference to IDs of possible preceding questions. 
#' The "qstn" IDREFS may be used to specify the question IDs.
#' 
#' * `ddi_forward()` contains a reference to IDs of possible following questions.
#' The "qstn" IDREFS may be used to specify the question IDs.
#' 
#' * `ddi_ivuInstr()` are specific instructions to the individual conducting an 
#' interview.
#' 
#' * `ddi_postQTxt()` is the text describing what occurs after the literal question
#' has been asked.
#' 
#' * `ddi_preQTxt()` is the pre-question text. This is the text describing a set 
#' of conditions under which a question might be asked.
#' 
#' * `ddi_qstnLit()` is the text of the actual, literal question asked.
#' 
#' @param ... Child nodes or attributes. 
#' 
#' @return A ddi_node object.
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/qstn.html}{qstn documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/backward.html}{backward documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/forward.html}{forward documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/ivuInstr.html}{ivuInstr documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/postQTxt.html}{postQTxt documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/preQTxt.html}{preQTxt documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/qstnLit.html}{qstnLit documentation}
#' 
#' @examples 
#' ddi_qstn("When you get together with your friends, would you say you discuss 
#' political matters frequently, occasionally, or never", ID = "Q125")
#' 
#' # Functions that need to be wrapped in ddi_qstn()
#' 
#' # Including ddi_preQTxt within a ddi_qstn with content
#' ddi_qstn("When you get together with your friends, would you say you discuss 
#' political matters frequently, occasionally, or never", ID = "Q125",
#'          ddi_preQTxt("For those who did not go away on a holiday of four days or more in 1985..."))
#' 
#` # Including ddi_postQTxt within a ddi_qstn without content
#' ddi_qstn(ddi_postQTxt("The next set of questions will ask about your financial situation")) 
#' 
#' # Using IDREFS in ddi_backward() and ddi_forward()
#' 
#' ddi_backward(qstn = "Q143")
#' 
#' ddi_forward("If yes, please ask questions 120-124", qstn = "Q120 Q121 Q122 Q123 Q124")
#' 
#' # Other child elements
#' 
#' ddi_ivuInstr("Please prompt the respondent if they are reticent to answer this question.", 
#'               lang = "en")
#' 
#' ddi_qstnLit("Why didn't you go away in 1985?")
#' 
#' @export
ddi_qstn <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", "qstn", "var", "seqNo", "sdatrefs",
                        "responseDomainType", "otherResponseDomainType")
    attribs <- validate_attributes(attribs, allowed_attribs, "var")
  }
  
  allowed_children = c("preQTxt", "qstnLit", "postQTxt", "forward", "backward", "ivuInstr")
  if(length(components$content) == 1 & is.character(components$content[[1]])) {
    build_leaf_node(
      "qstn",
      attribs = attribs,
      content = components$content
    )
  } else if(any(sapply(components$content, is.character))) {
    build_branch_node(
      "qstn",
      content = unwrap_content(components$content),
      attribs = attribs,
      allowed_children = allowed_children
    )
  } else {
    build_branch_node(
      "qstn",
      content = components$content,
      attribs = components$attribs,
      allowed_children = allowed_children
    )
  }
}

#' @rdname ddi_qstn 
#' @export
ddi_backward <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                         "qstn")
    attribs <- validate_attributes(attribs, allowed_attribs, "backward")
  }
  
  build_leaf_node(
    "backward",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_qstn 
#' @export
ddi_forward <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                         "qstn")
    attribs <- validate_attributes(attribs, allowed_attribs, "forward")
  }
  
  build_leaf_node(
    "forward",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_qstn 
#' @export
ddi_ivuInstr <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "ivuInstr")
  }
  
  build_leaf_node(
    "ivuInstr",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_qstn 
#' @export
ddi_postQTxt <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "postQTxt")
  }
  
  build_leaf_node(
    "postQTxt",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_qstn 
#' @export
ddi_preQTxt <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "preQTxt")
  }
  
  build_leaf_node(
    "preQTxt",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_qstn 
#' @export
ddi_qstnLit <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                         "sdatrefs")
    attribs <- validate_attributes(attribs, allowed_attribs, "qstnLit")
  }
  
  build_leaf_node(
    "qstnLit",
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

#' security node
#' 
#' Provides information regarding levels of access, e.g., public, subscriber, 
#' need to know. The ISO standard for dates (YYYY-MM-DD) is recommended for use
#' with the date attribute. More information on this element, especially its 
#' allowed attributes, can be found in the references.
#' 
#' \emph{Parent nodes}
#' 
#' `security` is contained in `nCube` and `var`.
#' 
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/security.html}{security documentation}
#' 
#' @examples 
#' ddi_security(date = "1998-05-10",
#'              "This variable has been recoded for reasons of confidentiality. 
#'              Users should contact the archive for information on obtaining access.")
#' 
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

#' embargo node
#' 
#' Provides information on variables/nCubes which are not currently available 
#' because of policies established by the principal investigators and/or data 
#' producers. This element may be repeated to support multiple language 
#' expressions of the content.
#' 
#' \emph{Parent nodes}
#' 
#' `embargo` is contained in `nCube` and `var`.
#' 
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/embargo.html}{embargo documentation}
#' 
#' @examples 
#' ddi_embargo(event = "notBefore",
#'             date = "2001-09-30",
#'             "The data associated with this variable/nCube will not become 
#'             available until September 30, 2001, because of embargo provisions 
#'             established by the data producers.")
#' 
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

#' respUnit node
#' 
#' Provides information regarding who provided the information contained within 
#' the variable/nCube, e.g., respondent, proxy, interviewer. This element may be 
#' repeated only to support multiple language expressions of the content. More 
#' information on this element, especially its allowed attributes, can be found 
#' in the references.
#' 
#' \emph{Parent nodes}
#' 
#' `respUnit` is contained in `nCube` and `var`.
#' 
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/respUnit.html}{respUnit documentation}
#' 
#' @examples 
#' ddi_respUnit("Head of household")
#' 
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
ddi_varFormat <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", "formatname",
                        "schema", "otherSchema", "type", "category", "otherCategory", "URI") 
    attribs <- validate_attributes(attribs, allowed_attribs, "varFormat")
    if("type" %in% names(attribs)) {
      check_attribs_in_set(attribs$type, c("numeric", "character"), field = "varFormat(type)")
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
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                      "UNITS", "VALUE") 
  attribs <- validate_attributes(attribs, allowed_attribs, "item")

  build_leaf_node(
    "item",
    attribs = attribs
  )
}

#' range node
#' 
#' This is the actual range of values. The "UNITS" attribute permits 
#' the specification of integer/real numbers. The "min" and "max" attributes 
#' specify the lowest and highest values that are part of the range. The 
#' "minExclusive" and "maxExclusive" attributes specify values that are 
#' immediately outside the range. This is an empty element consisting only of 
#' its attributes. More information on this element, especially its allowed 
#' attributes, can be found in the references.
#' 
#' \emph{Parent nodes}
#' 
#' `range` is contained in the following elements: `valrng`; `invalrng`; and 
#' `cohort`.
#' 
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/range.html}{range documentation}
#' 
#' @examples 
#' ddi_range(min = "1", maxExclusive = "20")
#'
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

#' location node
#' 
#' The physical or digital location of the variable. It is an empty element. 
#' More information on these elements, especially their allowed attributes, 
#' can be found in the references. 
#' 
#' \emph{Parent nodes}
#' 
#' `location` is contained in `nCube` and `var`.
#' 
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/location.html}{location documentation}
#' 
#' @examples 
#' ddi_location(StartPos = "55",
#'              EndPos = "57",
#'              RecSegNo = "2",
#'              fileid = "CARD-IMAGE")
#' 
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
    "defntn",
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