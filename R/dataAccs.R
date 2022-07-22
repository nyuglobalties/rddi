#' dataAccs and its children
#'
#' This section describes data access conditions and terms of use for the data 
#' collection. In cases where access conditions differ across individual files 
#' or variables, multiple access conditions can be specified.  More information 
#' on this element, especially the allowed attributes, can be found in the 
#' references.
#' 
#' \emph{Parent node}
#'
#' `dataAccs` is contained in `stdyDscr`.
#'
#' @param ... Child nodes or attributes. 
#' 
#' @return A ddi_node object.
#'
#' @section Shared and complex child nodes:
#' * [ddi_notes()]
#' * [ddi_setAvail()]
#' * [ddi_useStmt()]
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataAccs.html}{dataAccs documentation}
#' 
#' @examples 
#' ddi_dataAccs()
#' 
#' @export
ddi_dataAccs <- function(...) {
  allowed_children <- c(
    "setAvail", 
    "useStmt",
    "notes"
  )

  components <- dots_to_xml_components(...)
  attribs <- components$attribs    

  if (!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "dataAccs")
  }

  build_branch_node(
    "dataAccs",
    allowed_children = allowed_children,
    components = components,
    attribs = attribs
  )
}

#' setAvail and its children
#' 
#' Information on availability and storage of the data set collection. More 
#' information on this element, especially the allowed attributes, can 
#' be found in the references.
#' 
#' \emph{Parent node}
#'
#' `setAvail` is contained in `dataAccs`.
#' 
#' \emph{setAvail specific child nodes}
#' 
#' `ddi_accsPlac()` is the location where the data collection is currently stored. 
#' Use the URI attribute to provide a URN or URL for the storage site or the 
#' actual address from which the data may be downloaded.
#' 
#' `ddi_avlStatus()` is the statement of collection availability. An archive may 
#' need to indicate that a collection is unavailable because it is embargoed 
#' for a period of time, because it has been superseded, because a new edition
#' is imminent, etc. 
#' 
#' `ddi_collSize()` summarizes the number of physical files that exist in a 
#' collection, recording the number of files that contain data and noting 
#' whether the collection contains machine-readable documentation and/or other 
#' supplementary files and information such as data dictionaries, data 
#' definition statements, or data collection instruments. 
#' 
#' `ddi_complete()` is the completeness of study stored. This item indicates the 
#' relationship of the data collected to the amount of data coded and stored 
#' in the data collection. Information as to why certain items of collected 
#' information were not included in the data file stored by the archive should 
#' be provided.
#' 
#' `ddi_fileQnty()` is the total number of physical files associated with a 
#' collection.
#' 
#' `ddi_origArch()` is the archive from which the data collection was obtained; 
#' the originating archive. 
#' 
#' @param ... Child nodes or attributes. 
#' 
#' @return A ddi_node object.
#'
#' @section Shared and complex child nodes:
#' * [ddi_notes()]
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/setAvail.html}{setAvail documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/accsPlac.html}{accsPlac documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/avlStatus.html}{avlStatus documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/collSize.html}{collSize documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/complete.html}{complete documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/fileQnty.html}{fileQnty documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/origArch.html}{origArch documentation}
#' 
#' @examples 
#' ddi_setAvail()
#' 
#' # Functions that need to be wrapped in ddi_setAvail()
#' 
#' ddi_accsPlac(URI = "https://dataverse.harvard.edu/",
#'              "Harvard Dataverse")
#' 
#' ddi_avlStatus("This collection is superseded by CENSUS OF POPULATION, 1880...")
#' 
#' ddi_collSize("1 data file + machine-readable documentation (PDF) + SAS data definition statements.")
#' 
#' ddi_complete("Because of embargo provisions, data values for some variables have been masked...")
#' 
#' ddi_fileQnty("5 files")
#' 
#' ddi_origArch("Zentralarchiv fuer empirische Sozialforschung")
#' 
#' @export
ddi_setAvail <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "media", "callno", "label", "type")
    attribs <- validate_attributes(attribs, allowed_attribs, "setAvail")
  }

  allowed_children <- c(
    "accsPlac",
    "origArch", 
    "avlStatus",
    "collSize",
    "complete",
    "fileQnty",
    "notes"
  )  

  build_branch_node(
    "setAvail",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_setAvail
#' @export
ddi_accsPlac <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "URI")
    attribs <- validate_attributes(attribs, allowed_attribs, "accsPlac")
  }

  build_leaf_node(
    "accsPlac",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_setAvail
#' @export
ddi_avlStatus <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "avlStatus")
  }

  build_leaf_node(
    "avlStatus",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_setAvail
#' @export
ddi_collSize <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "collSize")
  }

  build_leaf_node(
    "collSize",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_setAvail
#' @export
ddi_complete <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "complete")
  }

  build_leaf_node(
    "complete",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_setAvail
#' @export
ddi_fileQnty <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "fileQnty")
  }

  build_leaf_node(
    "fileQnty",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_setAvail
#' @export
ddi_origArch <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "origArch")
  }

  build_leaf_node(
    "origArch",
    attribs = attribs,
    content = components$content
  )
}