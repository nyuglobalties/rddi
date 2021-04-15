#' Create a DDI citation
#'
#' Citation entities belong in study and DDI document description blocks.
#'
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/citation.html}
#' 
#' @export
# MAY CONTAIN ELMENTS BY SUBSTITUTIONS (IS THIS SOMETHING TO HANDLE?)
ddi_citation <- function(...) {
    allowed_children <- c(
        "titlStmt",
        "rspStmt", 
        "prodStmt", 
        "distStmt", 
        "serStmt",
        "verStmt",
        "biblCit",
        "holdings",
        "notes"
    )

    components <- dots_to_xml_components(...)
    attribs <- components$attribs    

    # rspStmt, prodStmt, and distStmt are only allowed once in citation according to DDI 2.5
    if(check_cardinality(components$content, "rspStmt") > 1) rddi_err("Only 0 or 1 rspStmt children are allowed in citation")
    if(check_cardinality(components$content, "prodStmt") > 1) rddi_err("Only 0 or 1 prodStmt children are allowed in citation")
    if(check_cardinality(components$content, "distStmt") > 1) rddi_err("Only 0 or 1 distStmt children are allowed in citation")
    
    if (!is.null(attribs)) {
        allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn"
                            , "MARCURI")
        check_attribs_in_set(names(attribs), allowed_attribs, field = "dataDscr")
        check_attribs(attribs)
    }

    build_branch_node(
        "citation",
        allowed_children = allowed_children,
        required_children = "titlStmt",
        components = components,
        attribs = attribs
    )
}

#' Title Statement
#' 
#' Title statement for the work at the appropriate level: marked-up document; marked-up document source; study; study description, other materials; other materials for the study
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/titlStmt.html}
#' 
#' @export
ddi_titlStmt <- function(...) {
    allowed_children <- c(
        "titl",
        "subTitl",
        "altTitl",
        "parTitl",
        "IDNo"
    )

    components <- dots_to_xml_components(...)
    attribs <- components$attribs

    if (!is.null(attribs)) {
        allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
        check_attribs_in_set(names(attribs), allowed_attribs, field = "titlStmt")
        check_attribs(attribs)
    }

    build_branch_node(
        "titlStmt",
        allowed_children = allowed_children,
        required_children = "titl",
        components = components,
        attribs = attribs
  )
}

#' Title Statement
#' 
#' Full authoritative title for the work at the appropriate level: marked-up document; marked-up document source; study; other material(s) related to study 
#' description; other material(s) related to study. The study title will in most cases be identical to the title for the marked-up document. A full title should 
#' indicate the geographic scope of the data collection as well as the time period covered. Title of data collection (codeBook/stdyDscr/citation/titlStmt/titl) maps
#'  to Dublin Core Title element. This element is required in the Study Description citation. Title statement for the work at the appropriate level: marked-up document;
#'  marked-up document source; study; study description, other materials; other materials for the study
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/titl.html}
#' 
#' @export
ddi_titl <-  function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "titl")
    check_attribs(attribs)
  }

  build_leaf_node(
    "titl",
    attribs = attribs,
    content = components$content
  )
}

#' Subtitle
#' 
#' A secondary title used to amplify or state certain limitations on the main title. It may repeat information already in the main title. 
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/subTitl.html}
#' 
#' @export
ddi_subTitl <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "subTitl")
    check_attribs(attribs)
  }

  build_leaf_node(
    "subTitl",
    attribs = attribs,
    content = components$content
  )
}

#' Alternative Title
#' 
#' A title by which the work is commonly referred, or an abbreviation of the title.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/altTitl.html}
#' 
#' @export
ddi_altTitl <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "altTitl")
    check_attribs(attribs)
  }

  build_leaf_node(
    "altTitl",
    attribs = attribs,
    content = components$content
  ) 
}

#' Parallel Title
#' 
#' Title translated into another language.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/parTitl.html}
#' 
#' @export
ddi_parTitl <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "parTitl")
    check_attribs(attribs)
  }

  build_leaf_node(
    "parTitl",
    attribs = attribs,
    content = components$content
  )
}

#' Identification Number
#' 
#' Unique string or number (producer's or archive's number). An "agency" attribute is supplied. Identification Number of data collection maps to Dublin Core Identifier element. 
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/IDNo.html}
#' 
#' @export
ddi_IDNo <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "agency", "level")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "IDNo")
    if(!is.null(attribs$level)) check_attribs_in_set(names(attribs), c("study", "file", "project"), field = "level (IDNo)")
    check_attribs(attribs)
  }

  build_leaf_node(
    "IDNo",
    attribs = attribs,
    content = components$content
  )   
}

#' Responsibility Statement
#' 
#' Responsibility for the creation of the work at the appropriate level: marked-up document; marked-up document source; study; study description, other material; other material for study. 
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/rspStmt.html}
#' 
#' @export
ddi_rspStmt <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "rspStmt")
    check_attribs(attribs)
  }

  allowed_children <- c(
    "AuthEnty",
    "othId"
  )

  build_branch_node(
    "rspStmt",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )   
}

#' Authoring Entity/Primary Investigator
#' 
#' The person, corporate body, or agency responsible for the work's substantive and intellectual content. Repeat the element for each author, and use "affiliation" attribute 
#' if available. Invert first and last name and use commas. Author of data collection (codeBook/stdyDscr/citation/rspStmt/AuthEnty) maps to Dublin Core Creator element. Inclusion 
#' of this element in codebook is recommended.
#' 
#' The "author" in the Document Description should be the individual(s) or organization(s) directly responsible for the intellectual content of the DDI version, as distinct from 
#' the person(s) or organization(s) responsible for the intellectual content of the earlier paper or electronic edition from which the DDI edition may have been derived.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/AuthEnty.html}
#' 
#' @export
ddi_AuthEnty <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "affiliation")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "AuthEnty")
    check_attribs(attribs)
  }

  build_leaf_node(
    "AuthEnty",
    attribs = attribs,
    content = components$content
  )   
}

#' Other Identifications/Acknowledgements
#' 
#' Statements of responsibility not recorded in the title and statement of responsibility areas. Indicate here the persons or bodies connected with the work, or significant
#' persons or bodies connected with previous editions and not already named in the description. For example, the name of the person who edited the marked-up documentation 
#' might be cited in codeBook/docDscr/rspStmt/othId, using the "role" and "affiliation" attributes. Other identifications/acknowledgments for data collection 
#' (codeBook/stdyDscr/citation/rspStmt/othId) maps to Dublin Core Contributor element.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/othId.html}
#' 
#' @export
ddi_othId <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "type", "role", "affiliation")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "othId")
    check_attribs(attribs)
  }

  build_leaf_node(
    "othId",
    attribs = attribs,
    content = components$content
  )   
}

#' Production Statement
#' 
#' Production statement for the work at the appropriate level: marked-up document; marked-up document source; study; study description, other material; 
#' other material for study.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/prodStmt.html}
#' 
#' @export
ddi_prodStmt <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "prodStmt")
    check_attribs(attribs)
  }

  allowed_children <- c(
    "producer",
    "copyright",
    "prodDate",
    "prodPlac",
    "software",
    "fundAg",
    "grantNo"
  )

  build_branch_node(
    "prodStmt",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
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
                        "abbr", "role", "affiliation")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "producer")
    check_attribs(attribs)
  }

  build_leaf_node(
    "producer",
    attribs = attribs,
    content = components$content
  )   
}

#' Copyright
#' 
#' Copyright statement for the work at the appropriate level. Copyright for data collection (codeBook/stdyDscr/citation/prodStmt/copyright) maps to Dublin Core Rights. 
#' Inclusion of this element is recommended.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/copyright.html}
#' 
#' @export
ddi_copyright <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "copyright")
    check_attribs(attribs)
  }

  build_leaf_node(
    "copyright",
    attribs = attribs,
    content = components$content
  )   
}

#' Date of Production
#' 
#' Date when the marked-up document/marked-up document source/data collection/other material(s) were produced (not distributed or archived). The ISO standard for 
#' dates (YYYY-MM-DD) is recommended for use with the date attribute. Production date for data collection (codeBook/stdyDscr/citation/prodStmt/prodDate) maps to 
#' Dublin Core Date element.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/prodDate.html}
#' 
#' @export
ddi_prodDate <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "date")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "prodDate")
    check_attribs(attribs)
  }

  build_leaf_node(
    "prodDate",
    attribs = attribs,
    content = components$content
  )   
}

#' Place of Production
#' 
#' Address of the archive or organization that produced the work.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/prodPlac.html}
#' 
#' @export
ddi_prodPlac <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "prodPlac")
    check_attribs(attribs)
  }

  build_leaf_node(
    "prodPlac",
    attribs = attribs,
    content = components$content
  )   
}

#' Software used in Production
#' 
#' Software used to produce the work. A "version" attribute permits specification of the software version number. The "date" attribute is provided to enable 
#' specification of the date (if any) for the software release. The ISO standard for dates (YYYY-MM-DD) is recommended for use with the date attribute.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/software.html}
#' 
#' @export
ddi_software <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "date", "version")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "software")
    check_attribs(attribs)
  }

  build_leaf_node(
    "software",
    attribs = attribs,
    content = components$content
  )   
}

#' Funding Agency/Sponsor
#' 
#' The source(s) of funds for production of the work. If different funding agencies sponsored different stages of the production process, use the "role" 
#' attribute to distinguish them.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/fundAg.html}
#' 
#' @export
ddi_fundAg <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "abbr", "role")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "fundAg")
    check_attribs(attribs)
  }

  build_leaf_node(
    "fundAg",
    attribs = attribs,
    content = components$content
  )   
}

#' Grant Number
#' 
#' The grant/contract number of the project that sponsored the effort. If more than one, indicate the appropriate agency using the "agency" attribute. If 
#' different funding agencies sponsored different stages of the production process, use the "role" attribute to distinguish the grant numbers. 
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/grantNo.html}
#' 
#' @export
ddi_grantNo <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "agency", "role")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "grantNo")
    check_attribs(attribs)
  }

  build_leaf_node(
    "grantNo",
    attribs = attribs,
    content = components$content
  )   
}

#' Series Statement
#' 
#' Series statement for the work at the appropriate level: marked-up document; marked-up document source; study; study description, other material; other material 
#' for study. The URI attribute is provided to point to a central Internet repository of series information. Repeat this field if the study is part of more than one 
#' series. Repetition of the internal content should be used to support multiple languages only.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/serStmt.html}
#' 
#' @export
ddi_serStmt <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "URI")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "serStmt")
    check_attribs(attribs)
  }

  allowed_children <- c(
    "serName",
    "serInfo"
  )

  build_branch_node(
    "serStmt",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )   
}

#' Series Name
#' 
#' The name of the series to which the work belongs.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/serName.html}
#' 
#' @export
ddi_serName <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "abbr")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "serName")
    check_attribs(attribs)
  }

  build_leaf_node(
    "serName",
    attribs = attribs,
    content = components$content
  )   
}

#' Series Information
#' 
#' Contains a history of the series and a summary of those features that apply to the series as a whole.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/serInfo.html}
#' 
#' @export
ddi_serInfo <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "serInfo")
    check_attribs(attribs)
  }

  build_leaf_node(
    "serInfo",
    attribs = attribs,
    content = components$content
  )   
}

#' Bibliographic Citation
#' 
#' Complete bibliographic reference containing all of the standard elements of a citation that can be used to cite the work. The "format" attribute is provided 
#' to enable specification of the particular citation style used, e.g., APA, MLA, Chicago, etc.
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/serInfo.html}
#' 
#' @export
ddi_biblCit <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "format")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "biblCit")
    check_attribs(attribs)
  }

  build_leaf_node(
    "biblCit",
    attribs = attribs,
    content = components$content
  )   
}

#' Holdings Information
#' 
#' Information concerning either the physical or electronic holdings of the cited work. Attributes include: location--The physical location where a copy is 
#' held; callno--The call number for a work at the location specified; and URI--A URN or URL for accessing the electronic copy of the cited work. 
#' 
#' @param ... Child nodes or attributes. To set a DDI ID, use `id_object` in any `ddi_` function to assign the identifier.
#'
#' @section DDI Codebook 2.5 Documentation
#' url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/holdings.html}
#' 
#' @export
ddi_holdings <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "location", "callno", "URI", "media")
    check_attribs_in_set(names(attribs), allowed_attribs, field = "holdings")
    check_attribs(attribs)
  }

  build_leaf_node(
    "holdings",
    attribs = attribs,
    content = components$content
  )   
}