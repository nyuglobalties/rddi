#' fileTxt and its children
#'
#' Provides descriptive information about the data file. More information on 
#' these elements, especially their allowed attributes, can be found in the 
#' references. 
#'  
#' \emph{Parent nodes}
#' 
#' `fileTxt` is contained in `fileDscr`.
#' 
#' \emph{fileTxt specific child nodes}
#' 
#' * [ddi_dataChck()] are the types of checks and operations performed on the 
#' data file at the file level.
#' 
#' * [ddi_dataMsng()] can be used to give general information about missing 
#' data, e.g., that missing data have been standardized across the collection, 
#' missing data are present because of merging, etc.
#' 
#' * [ddi_fileCont()] are the file contents. It is the abstract or description 
#' of the file. A summary describing the purpose, nature, and scope of the data 
#' file, special characteristics of its contents, major subject areas covered, 
#' and what questions the PIs attempted to answer when they created the file. 
#' A listing of major variables in the file is important here. In the case of 
#' multi-file collections, this uniquely describes the contents of each file.
#' 
#' * [ddi_fileName()] contains a short title that will be used to distinguish a 
#' particular file/part from other files/parts in the data collection. The 
#' element may be repeated to support multiple language expressions of the 
#' content.
#' 
#' * [ddi_filePlac()] indicates where the file was produced, whether at an 
#' archive or elsewhere.
#' 
#' * [ddi_fileType()] are the types of data files. These include raw data 
#' (ASCII, EBCDIC, etc.) and software-dependent files such as SAS datasets, 
#' SPSS export files, etc. If the data are of mixed types (e.g., ASCII and 
#' packed decimal), state that here.
#' 
#' * [ddi_format()] is the physical format of the data file: Logical record 
#' length format, card-image format (i.e., data with multiple records per case), 
#' delimited format, free format, etc. The element may be repeated to support 
#' multiple language expressions of the content.
#' 
#' * [ddi_ProcStat()] is the processing status of the file. Some data producers 
#' and social science data archives employ data processing strategies that 
#' provide for release of data and documentation at various stages of processing.
#' 
#' @section Shared and complex child nodes:
#' * [ddi_dataFingerprint()]
#' * [ddi_dimensns()]
#' * [ddi_fileCitation()]
#' * [ddi_fileStrc()]
#' * [ddi_software()]
#' * [ddi_verStmt()]
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/fileTxt.html}{fileTxt documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataChck.html}{dataChck documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataMsng.html}{dataMsng documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/fileCont.html}{fileCont documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/fileName.html}{fileName documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/filePlac.html}{filePlac documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/fileType.html}{fileType documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/format.html}{format documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/ProcStat.html}{ProcStat documentation}
#' 
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#' 
#' @examples 
#' ddi_fileTxt()
#'
#' # Functions that need to be wrapped in ddi_fileTxt()
#'
#' ddi_dataChck("Consistency checks were performed by Data Producer/ Principal Investigator.")
#'
#' ddi_dataMsng('The codes "-1" and "-2" are used to represent missing data.')
#'
#' ddi_fileCont("Part 1 contains both edited and constructed variables describing demographic...")
#'
#' ddi_fileName(ID = "File1", "Second-Generation Children Data")
#'
#' ddi_filePlac("Washington, DC: United States Department of Commerce, Bureau of the Census")
#'
#' ddi_fileType(charset = "US-ASCII", "ASCII data file")
#'
#' ddi_format("comma-delimited")
#'
#' ddi_ProcStat("Available from the DDA. Being processed.")
#'
#' @export
ddi_fileTxt <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn") 
    attribs <- validate_attributes(attribs, allowed_attribs, "fileTxt")
  }

  allowed_children <- c(
      "fileName", 
      "fileCitation", 
      "dataFingerprint", 
      "fileCont", 
      "fileStrc", 
      "dimensns", 
      "fileType",
      "format", 
      "filePlac", 
      "dataChck", 
      "ProcStat", 
      "dataMsng", 
      "software", 
      "verStmt"
  )

  if(check_cardinality(components$content, "fileCitation") > 1) rddi_err("Only 0 or 1 fileCitation children are allowed in fileTxt")
  if(check_cardinality(components$content, "fileCont") > 1) rddi_err("Only 0 or 1 fileCont children are allowed in fileTxt")
  if(check_cardinality(components$content, "fileStrc") > 1) rddi_err("Only 0 or 1 fileStrc children are allowed in fileTxt")
  if(check_cardinality(components$content, "dimensns") > 1) rddi_err("Only 0 or 1 dimensns children are allowed in fileTxt")


  build_branch_node(
    "fileTxt",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_fileTxt
#' @export
ddi_dataChck <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleURN", "ddiCodebookURN")
    attribs <- validate_attributes(attribs, allowed_attribs, "dataChck")
  }

  build_leaf_node(
    "dataChck",
    attribs = attribs,
    content = components$content 
  )
}

#' @rdname ddi_fileTxt
#' @export
ddi_dataMsng <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleURN", "ddiCodebookURN")
    attribs <- validate_attributes(attribs, allowed_attribs, "dataMsng")
  }

  build_leaf_node(
    "dataMsng",
    attribs = attribs,
    content = components$content 
  )
}

#' @rdname ddi_fileTxt
#' @export
ddi_fileCont <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleURN", "ddiCodebookURN")
    attribs <- validate_attributes(attribs, allowed_attribs, "fileCont")
  }

  build_leaf_node(
    "fileCont",
    attribs = attribs,
    content = components$content 
  )
}

#' @rdname ddi_fileTxt
#' @export
ddi_fileName <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleURN", "ddiCodebookURN")
    attribs <- validate_attributes(attribs, allowed_attribs, "fileName")
  }

  build_leaf_node(
    "fileName",
    attribs = attribs,
    content = components$content 
  )
}

#' @rdname ddi_fileTxt
#' @export
ddi_filePlac <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleURN", "ddiCodebookURN")
    attribs <- validate_attributes(attribs, allowed_attribs, "filePlac")
  }

  build_leaf_node(
    "filePlac",
    attribs = attribs,
    content = components$content 
  )
}

#' @rdname ddi_fileTxt
#' @export
ddi_fileType <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleURN", "ddiCodebookURN",
                        "charset")
    attribs <- validate_attributes(attribs, allowed_attribs, "fileType")
  }

  build_leaf_node(
    "fileType",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_fileTxt
#' @export
ddi_format <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleURN", "ddiCodebookURN")
    attribs <- validate_attributes(attribs, allowed_attribs, "format")
  }

  build_leaf_node(
    "format",
    attribs = attribs,
    content = components$content  
  )
}

#' @rdname ddi_fileTxt
#' @export
ddi_ProcStat <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleURN", "ddiCodebookURN")
    attribs <- validate_attributes(attribs, allowed_attribs, "ProcStat")
  }

  build_leaf_node(
    "ProcStat",
    attribs = attribs,
    content = components$content 
  )
}

#' dataFingerprint and its child nodes
#'
#' Allows for assigning a hash value (digital fingerprint) to the data or 
#' data file. More information on these elements, especially their allowed 
#' attributes, can be found in the references.
#' 
#'
#' \emph{Parent nodes}
#' 
#' `dataFingerprint` is contained in `fileDscr`.
#' 
#' \emph{dataFingerprint specific child nodes}
#' 
#' * `ddi_algorithmSpecification()`
#' * `ddi_algorithmVersion()`
#' * `ddi_digitalFingerprintValue()`
#' 
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataFingerprint.html}{dataFingerprint documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/algorithmSpecification.html}{algorithmSpecification documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/algorithmVersion.html}{algorithmVersion documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/digitalFingerprintValue.html}{digitalFingerprintValue documentation}
#'
#' @examples 
#' ddi_dataFingerprint()
#' 
#' # Functions that need to be wrapped in ddi_Fingerprint()
#' 
#' ddi_algorithmSpecification()
#' 
#' ddi_algorithmVersion()
#' 
#' ddi_digitalFingerprintValue()
#' 
#' @export
ddi_dataFingerprint <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn") 
    attribs <- validate_attributes(attribs, allowed_attribs, "dataFingerprint")
    if("type" %in% names(attribs)) check_attribs_in_set(attributes = attribs$type, vals = c("data", "dataFile"), field = "type (dataFingerprint)")
    
  }

  allowed_children <- c(
      "digitalFingerprintValue",
      "algorithmSpecification",
      "algorithmVersion"
      )

  if(check_cardinality(components$content, "algorithmSpecification") > 1) rddi_err("Only 0 or 1 algorithmSpecification children are allowed in dataFingerprint")
  if(check_cardinality(components$content, "algorithmVersion") > 1) rddi_err("Only 0 or 1 algorithmVersion children are allowed in dataFingerprint")

  build_branch_node(
    "dataFingerprint",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_dataFingerprint
#' @export
ddi_algorithmSpecification <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    rddi_err("No attributes are allowed in algorithmSpecification")
  }

  build_leaf_node(
    "algorithmSpecification",
    content = components$content 
  )
}

#' @rdname ddi_dataFingerprint
#' @export
ddi_algorithmVersion <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    rddi_err("No attributes are allowed in algorithmVersion")
  }

  build_leaf_node(
    "algorithmVersion",
    content = components$content 
  )
}

#' @rdname ddi_dataFingerprint
#' @export
ddi_digitalFingerprintValue <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    rddi_err("No attributes are allowed in digitalFingerprintValue")
  }

  build_leaf_node(
    "digitalFingerprintValue",
    content = components$content 
  )
}

#' dimensns, recDimnsn and their child nodes
#'
#' Dimensions of the overall digital or physical file. More information on 
#' these elements, especially their allowed attributes, can be found in the references.
#' 
#' \emph{Parent nodes}
#' 
#' `dimensns` is contained in `fileTxt`. `recDimensn` is contained in `recGrp`.
#' 
#' \emph{dimnsns and recDimnsn shared nodes}
#' 
#' * `ddi_caseQnty()` is the number of cases, observations, or records.
#' 
#' * `ddi_logRecL()` is the logical record length, i.e., number of characters of 
#' data in the record.
#' 
#' * `ddi_varQnty()` is the overall variable count.
#' 
#' \emph{dimnsns specific nodes}
#' 
#' * `ddi_recNumTot()` is the overall record count in file. Particularly 
#' helpful in instances such as files with multiple cards/decks or records per 
#' case.
#' 
#' * `ddi_recPrCas()` is the number of records per case in the file. This 
#' element should be used for card-image data or other files in which there 
#' are multiple records per case. 
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dimensns.html}{dimensns documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/caseQnty.html}{caseQnty documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/logRecL.html}{logRecL documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/recDimnsn.html}{recDimnsn documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/recNumTot.html}{recNumTot documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/recPrCas.html}{recPrCas documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/varQnty.html}{varQnty documentation}
#' 
#' @param ... Child nodes or attributes.
#'
#' @return A ddi_node object.
#' 
#' @examples 
#' ddi_dimensns()
#' 
#' ddi_recDimnsn()
#' 
#' # Functions that need to be wrapped in ddi_dimensns() or ddi_recDimnsn()
#' 
#' ddi_caseQnty("1011")
#' 
#' ddi_logRecL("27")
#' 
#' ddi_varQnty("27")
#' 
#' # Functions that need to be wrapped in ddi_dimensns
#' 
#' ddi_recNumTot("2400")
#' 
#' ddi_recPrCas("5")
#' 
#' @export
ddi_dimensns <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn") 
    attribs <- validate_attributes(attribs, allowed_attribs, "dimensns")
  }

  allowed_children <- c(
      "caseQnty",
      "varQnty",
      "logRecL",
      "recPrCas",
      "recNumTot"
      )

  build_branch_node(
    "dimensns",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_dimensns
#' @export
ddi_caseQnty <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleURN", "ddiCodebookURN")
    attribs <- validate_attributes(attribs, allowed_attribs, "caseQnty")
  }

  build_leaf_node(
    "caseQnty",
    attribs = attribs,
    content = components$content 
  )
}

#' @rdname ddi_dimensns
#' @export
ddi_logRecL <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleURN", "ddiCodebookURN")
    attribs <- validate_attributes(attribs, allowed_attribs, "logRecL")
  }

  build_leaf_node(
    "logRecL",
    attribs = attribs,
    content = components$content 
  )
}

#' @rdname ddi_dimensns
#' @export
ddi_recDimnsn <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                         "level") 
    attribs <- validate_attributes(attribs, allowed_attribs, "recDimnsn")
  }
  
  allowed_children <- c(
    "varQnty",
    "caseQnty",
    "logRecL"
  )
  
  if(check_cardinality(components$content, "caseQnty") > 1) rddi_err("Only 0 or 1 caseQnty children are allowed in recDimnsn")
  if(check_cardinality(components$content, "logRecL") > 1) rddi_err("Only 0 or 1 logRecL children are allowed in recDimnsn")
  if(check_cardinality(components$content, "varQnty") > 1) rddi_err("Only 0 or 1 varQnty children are allowed in recDimnsn")
  
  build_branch_node(
    "recDimnsn",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_dimensns
#' @export
ddi_recNumTot <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleURN", "ddiCodebookURN")
    attribs <- validate_attributes(attribs, allowed_attribs, "recNumTot")
  }

  build_leaf_node(
    "recNumTot",
    attribs = attribs,
    content = components$content 
  )
}

#' @rdname ddi_dimensns
#' @export
ddi_recPrCas <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleURN", "ddiCodebookURN")
    attribs <- validate_attributes(attribs, allowed_attribs, "recPrCas")
  }

  build_leaf_node(
    "recPrCas",
    attribs = attribs,
    content = components$content 
  )
}

#' @rdname ddi_dimensns
#' @export
ddi_varQnty <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleURN", "ddiCodebookURN")
    attribs <- validate_attributes(attribs, allowed_attribs, "varQnty")
  }

  build_leaf_node(
    "varQnty",
    attribs = attribs,
    content = components$content 
  )
}

#' fileStrc and its child nodes
#'
#' Type of file structure. The file structure is fully described in the first 
#' `fileTxt` within the `fileDscr` and then the `fileStrc` in subsequent 
#' `fileTxt` descriptions would reference the first fileStrcRef attribute rather 
#' than repeat the details. More information on these elements, especially 
#' their allowed attributes, can be found in the references. 
#'
#' \emph{Parent node}
#' 
#' `fileStrc` is contained in `fileTxt`.
#'
#' @section Shared and complex child nodes:
#' * [ddi_notes()] 
#' * [ddi_recGrp()]
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/fileStrc.html}{fileStrc documentation}
#' 
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#' 
#' @examples 
#' ddi_fileStrc()
#'
#' @export
ddi_fileStrc <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "type", "otherType", "fileStrcRef") 
    if("type" %in% names(attribs)) {
        check_attribs_in_set(attribs$type, c("rectangular", "hierarchical", "relational", "nested", "other"), "type (fileStrc)")
    }
    attribs <- validate_attributes(attribs, allowed_attribs, "fileStrc")
  }

  allowed_children <- c(
      "recGrp",
      "notes"
      )

  build_branch_node(
    "fileStrc",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' recGrp and its child nodes
#'
#' Used to describe record groupings if the file is hierarchical or relational. 
#' More information on these elements, especially their allowed attributes,
#' can be found in the references. 
#'
#' \emph{Parent nodes}
#' 
#' `recGrp` is contained in `fileStrc`.
#'
#' @section Shared and complex child nodes:
#' * [ddi_labl()] 
#' * [ddi_recDimnsn()]
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/recGrp.html}{recGrp documentation}
#' 
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#' 
#' @examples 
#' ddi_recGrp()
#'
#' @export
ddi_recGrp <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "recGrp", "rectype", "keyvar", "rtypeloc", "rtypewidth", "rtypevtype", "recidvar") 
    attribs <- validate_attributes(attribs, allowed_attribs, "recGrp")
  }

  allowed_children <- c(
    "labl",
    "recDimnsn"
    )

  if(check_cardinality(components$content, "recDimnsn") > 1) rddi_err("Only 0 or 1 recDimnsn children are allowed in recGrp")

  build_branch_node(
    "recGrp",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

