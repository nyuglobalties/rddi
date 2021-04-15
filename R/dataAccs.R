#' Data Access
#'
#' This section describes access conditions and terms of use for the data collection. In cases where access conditions differ across individual files 
#' or variables, multiple access conditions can be specified. The access conditions applying to a study, file, variable group, or variable can be indicated by an IDREF attribute on the study, file, variable group, or variable elements called "access".
#'
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataAccs.html}
#' 
#' @export
ddi_dataAccs <- function(...) {
    allowed_children <- c(
        "notes",
        "setAvail", 
        "useStmt"
    )

    components <- dots_to_xml_components(...)
    attribs <- components$attribs    

    if (!is.null(attribs)) {
        allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
        check_attribs_in_set(names(attribs), allowed_attribs, field = "dataAccs")
        check_attribs(attribs)
    }

    build_branch_node(
        "dataAccs",
        allowed_children = allowed_children,
        components = components,
        attribs = attribs
    )
}

#' Data Set Availability
#' 
#' Information on availability and storage of the collection. The "media" attribute may be used in combination with any of the subelements. See Location of Data Collection.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/respRate.html}
#' 
#' @export
ddi_setAvail <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "media", "callno", "label", "type")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "setAvail")
    check_attribs(attribs)
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

#' Location of Data Collection
#' 
#' Location where the data collection is currently stored. Use the URI attribute to provide a URN or URL for the storage site or the actual address from which the data may be downloaded.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/accsPlac.html}
#' 
#' @export
ddi_accsPlac <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "URI")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "accsPlac")
    check_attribs(attribs)
  }

  build_leaf_node(
    "accsPlac",
    attribs = attribs,
    content = components$content
  )
}

#' Availability Status
#' 
#' Statement of collection availability. An archive may need to indicate that a collection is unavailable because it is embargoed for a period of time, because it 
#' has been superseded, because a new edition is imminent, etc. It is anticipated that a controlled vocabulary will be developed for this element.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/accsPlac.html}
#' 
#' @export
ddi_avlStatus <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "avlStatus")
    check_attribs(attribs)
  }

  build_leaf_node(
    "avlStatus",
    attribs = attribs,
    content = components$content
  )
}

#' Extent of Collection
#' 
#' Summarizes the number of physical files that exist in a collection, recording the number of files that contain data and noting whether the collection 
#' contains machine-readable documentation and/or other supplementary files and information such as data dictionaries, data definition statements, or data 
#' collection instruments. 
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/collSize.html}
#' 
#' @export
ddi_collSize <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "collSize")
    check_attribs(attribs)
  }

  build_leaf_node(
    "collSize",
    attribs = attribs,
    content = components$content
  )
}

#' Completeness of Study Stored
#' 
#' This item indicates the relationship of the data collected to the amount of data coded and stored in the data collection. Information as to why certain 
#' items of collected information were not included in the data file stored by the archive should be provided. 
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/complete.html}
#' 
#' @export
ddi_complete <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "complete")
    check_attribs(attribs)
  }

  build_leaf_node(
    "complete",
    attribs = attribs,
    content = components$content
  )
}

#' Number of Files
#' 
#' Total number of physical files associated with a collection.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/complete.html}
#' 
#' @export
ddi_fileQnty <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "fileQnty")
    check_attribs(attribs)
  }

  build_leaf_node(
    "fileQnty",
    attribs = attribs,
    content = components$content
  )
}

#' Archive Where Study Orignally Stored
#' 
#' Archive from which the data collection was obtained; the originating archive. 
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/origArch.html}
#' 
#' @export
ddi_origArch <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "origArch")
    check_attribs(attribs)
  }

  build_leaf_node(
    "origArch",
    attribs = attribs,
    content = components$content
  )
}

#' Data Set Availability
#' 
#' Information on availability and storage of the collection. The "media" attribute may be used in combination with any of the subelements. See Location of Data Collection.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/respRate.html}
#' 
#' @export
ddi_useStmt <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "media", "callno", "label", "type")
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
  # Contact is within distStmt in citation, specPerm is within sampleFrame in dataColl in method

  build_branch_node(
    "useStmt",
    allowed_children = allowed_children,
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

#' Confidentiality Declaration
#' 
#' This element is used to determine if signing of a confidentiality declaration is needed to access a resource. The "required" attribute is used to 
#' aid machine processing of this element, and the default specification is "yes". The "formNo" attribute indicates the number or ID of the form that 
#' the user must fill out. The "URI" attribute may be used to provide a URN or URL for online access to a confidentiality declaration form.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/confDec.html}
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
#' Any restrictions on access to or use of the collection such as privacy certification or distribution restrictions should be indicated here. These can 
#' be restrictions applied by the author, producer, or disseminator of the data collection. If the data are restricted to only a certain class of user, specify which type.  
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

