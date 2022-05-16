#' citation, sourceCitation, fileCitation and their child nodes
#'
#' Citation entities for the study including general citations and source 
#' citations. Citation is a required element in the DDI-Codebook.
#' `fileCitation` provides a full bibliographic citation option for each data file described 
#' in `fileDscr`. The minimum element set includes: `titl`, `IDNo`, `authEnty`, `producer`, and 
#' `prodDate`. If a DOI is available for the data enter this in the `IDNo`. 
#' More information on these elements, especially their allowed attributes, can 
#' be found in the references.
#' 
#' \emph{Parent nodes}
#' 
#' `citation` is contained in the following elements: `docDscr`; `othRefs`; 
#' `otherMat`; `relMat`; `relPubl`; `relStdy`; and `stdyDscr`. `sourceCitation` 
#' is contained in the `sources` element. `fileCitation` is included in the 
#' `fileTxt` element.
#' 
#' \emph{citation, sourceCitation, and fileCitation specific child nodes}
#' 
#' `ddi_biblCit()` is the complete bibliographic reference containing all of the 
#' standard elements of a citation that can be used to cite the work. The "format" 
#' attribute is provided to enable specification of the particular citation style 
#' used, e.g., APA, MLA, Chicago, etc.
#' 
#' `ddi_holdings()` is information concerning either the physical or electronic 
#' holdings of the cited work. Attributes include: location--The physical location 
#' where a copy is held; callno--The call number for a work at the location 
#' specified; and URI--A URN or URL for accessing the electronic copy of the cited work. 
#' 
#' @param ... Child nodes or attributes. 
#'
#' @section Shared and complex child nodes: 
#' * [ddi_distStmt()]
#' * [ddi_notes()]
#' * [ddi_prodStmt()]
#' * [ddi_rspStmt()]
#' * [ddi_serStmt()]
#' * [ddi_titlStmt()]
#' * [ddi_verStmt()]
#' 
#' @return A ddi_node object..
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/citation.html}{citation documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/fileCitation.html}{fileCitation documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/sourceCitation.html}{sourceCitation documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/biblCit.html}{biblCit documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/holdings.html}{holdings documentation}
#' 
#' @examples 
#' ddi_citation()
#' 
#' ddi_sourceCitation()
#' 
#' ddi_fileCitation()
#' 
#' # An example using the ddi_biblCit() child:
#' 
#' ddi_citation(
#'    ddi_biblCit(format = "APA", "Full citation text")
#' )
#' 
#' # An example using the ddi_holdings() child:
#' 
#' ddi_citation(
#'    ddi_holdings(location = "ICPSR DDI Repository",
#'                 callno = "inap.",
#'                 URI = "http://www.icpsr.umich.edu/DDIrepository/",
#'                 "Marked-up Codebook for Current Population Survey, 1999: Annual Demographic File")
#' )
#' 
#' @export
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
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                          "MARCURI")
    attribs <- validate_attributes(attribs, allowed_attribs, "citation")
  }

  build_branch_node(
    "citation",
    allowed_children = allowed_children,
    components = components,
    attribs = attribs
  )
}

#' @rdname ddi_citation
#' @export
ddi_sourceCitation <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", 
                        "MARCURI")
    attribs <- validate_attributes(attribs, allowed_attribs, "sourceCitation")
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

  # rspStmt, prodStmt, and distStmt are only allowed once in citation according to DDI 2.5
  if(check_cardinality(components$content, "rspStmt") > 1) rddi_err("Only 0 or 1 rspStmt children are allowed in sourceCitation")
  if(check_cardinality(components$content, "prodStmt") > 1) rddi_err("Only 0 or 1 prodStmt children are allowed in sourceCitation")
  if(check_cardinality(components$content, "distStmt") > 1) rddi_err("Only 0 or 1 distStmt children are allowed in sourceCitation")
  
  build_branch_node(
    "sourceCitation",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_citation
#' @export
ddi_fileCitation <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", 
                        "MARCURI")
    attribs <- validate_attributes(attribs, allowed_attribs, "fileCitation")
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

  # rspStmt, prodStmt, and distStmt are only allowed once in citation according to DDI 2.5
  if(check_cardinality(components$content, "rspStmt") > 1) rddi_err("Only 0 or 1 rspStmt children are allowed in fileCitation")
  if(check_cardinality(components$content, "prodStmt") > 1) rddi_err("Only 0 or 1 prodStmt children are allowed in fileCitation")
  if(check_cardinality(components$content, "distStmt") > 1) rddi_err("Only 0 or 1 distStmt children are allowed in fileCitation")
  
  build_branch_node(
    "fileCitation",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' titlStmt and its child nodes
#' 
#' Title statement for the work at the appropriate level: marked-up document; 
#' marked-up document source; study; study description, other materials; other 
#' materials for the study. Both `titlStmt` and `titl` are required elements in the `citation`
#' branch of a DDI-Codebook. More information on these elements, especially 
#' their allowed attributes, can be found in the references.
#' 
#' \emph{Parent nodes}
#' 
#' `titlStmt` is contained in the following elements: `citation`; `docSrc`; 
#' `fileCitation`; and `sourceCitation`.
#' 
#' \emph{titlStmt specific child nodes}
#' 
#' `ddi_altTitl()` is the alternative title. A title by which the work is commonly referred, or an 
#' abbreviation of the title.
#' 
#' `ddi_IDNo()` is the identification number. This is a unique string or number (producer's or 
#' archive's number). Can be a DOI. An "agency" attribute is supplied. Identification Number 
#' of data collection maps to Dublin Core Identifier element. 
#' 
#' `ddi_parTitl()` is the parallel title. Title translated into another language.
#' 
#' `ddi_subTitl()` is the subtitle. A secondary title used to amplify or state certain limitations 
#' on the main title. It may repeat information already in the main title. 
#' 
#' @param ... Child nodes or attributes. 
#' 
#' @return A ddi_node object.
#' 
#' @section Shared and complex child nodes:
#' * [ddi_titl()]
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/titlStmt.html}{titlStmt documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/altTitl.html}{altTitl documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/IDNo.html}{IDNo documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/parTitl.html}{parTitl documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/subTitl.html}{subTitl documentation}
#' 
#' @examples 
#' ddi_titlStmt()
#' 
#' # Functions that need to be wrapped in ddi_titlStmt()
#' 
#' ddi_altTitl("Alternative Title of work")
#' 
#' ddi_IDNo(agency = "agency name", "ID number")
#' 
#' ddi_parTitl(lang = "fr", "French translation of the title")
#' 
#' ddi_subTitl("Subtitle of work")
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
    attribs <- validate_attributes(attribs, allowed_attribs, "titlStmt")
  }

  build_branch_node(
    "titlStmt",
    allowed_children = allowed_children,
    components = components,
    attribs = attribs
  )
}

#' @rdname ddi_titlStmt
#' @export
ddi_altTitl <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "altTitl")
  }
  
  build_leaf_node(
    "altTitl",
    attribs = attribs,
    content = components$content
  ) 
}

#' @rdname ddi_titlStmt
#' @export
ddi_IDNo <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                         "agency", "level")
    attribs <- validate_attributes(attribs, allowed_attribs, "IDNo")
    if(!is.null(attribs$level)) check_attribs_in_set(names(attribs), c("study", "file", "project"), field = "level (IDNo)")
  }
  
  build_leaf_node(
    "IDNo",
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_titlStmt
#' @export
ddi_parTitl <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "parTitl")
  }
  
  build_leaf_node(
    "parTitl",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_titlStmt
#' @export
ddi_subTitl <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "subTitl")
  }

  build_leaf_node(
    "subTitl",
    attribs = attribs,
    content = components$content
  )
}

#' rspStmt and its child nodes
#' 
#' Responsibility statement for the creation of the work at the appropriate level: marked-up document; marked-up document source; study; study description, other material; other 
#' material for study. More information on these elements, especially their allowed attributes, can 
#' be found in the references.
#' 
#' \emph{Parent nodes}
#' 
#' `rspStmt` is contained in the following elements: `citation`; `docSrc`; 
#' `fileCitation`; and `sourceCitation`.
#' 
#' \emph{rspStmt specific child nodes}
#' 
#' `ddi_AuthEnty()` is the person, corporate body, or agency responsible for the 
#' work's substantive and intellectual content. Repeat the element for each author, 
#' and use "affiliation" attribute if available. Invert first and last name and 
#' use commas. Author of data collection (codeBook/stdyDscr/citation/rspStmt/AuthEnty) 
#' maps to Dublin Core Creator element. Inclusion of this element in codebook is recommended.
#'
#' The "author" in the Document Description should be the individual(s) or 
#' organization(s) directly responsible for the intellectual content of the DDI 
#' version, as distinct from the person(s) or organization(s) responsible for the 
#' intellectual content of the earlier paper or electronic edition from which the 
#' DDI edition may have been derived.
#' 
#' `ddi_othId()` are the statements of responsibility not recorded in the title 
#' and statement of responsibility areas. Indicate here the persons or bodies 
#' connected with the work, or significant persons or bodies connected with 
#' previous editions and not already named in the description. For example, the 
#' name of the person who edited the marked-up documentation might be cited in 
#' codeBook/docDscr/rspStmt/othId, using the "role" and "affiliation" attributes. 
#' Other identifications/acknowledgments for data collection 
#' (codeBook/stdyDscr/citation/rspStmt/othId) maps to Dublin Core Contributor element.
#' 
#' @param ... Child nodes or attributes.
#'
#' @return A ddi_node object.
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/rspStmt.html}{rspStmt documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/AuthEnty.html}{AuthEnty documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/othId.html}{othId documentation}
#' 
#' @examples
#' ddi_rspStmt()
#' 
#' # Functions that need to be wrapped in ddi_rspStmt()
#' 
#' ddi_AuthEnty(affiliation = "Organization name",
#'              "LastName, FirstName")
#'
#' ddi_othId(role = "Data Manager",
#'           affiliation = "Organization name",
#'           "LastName, FirstName")
#' 
#' @export
ddi_rspStmt <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "rspStmt")
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

#' @rdname ddi_rspStmt
#' @export
ddi_AuthEnty <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "affiliation")
    attribs <- validate_attributes(attribs, allowed_attribs, "AuthEnty")
  }

  build_leaf_node(
    "AuthEnty",
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_rspStmt
#' @export
ddi_othId <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "type", "role", "affiliation")
    attribs <- validate_attributes(attribs, allowed_attribs, "othId")
  }

  build_leaf_node(
    "othId",
    attribs = attribs,
    content = components$content
  )   
}

#' prodStmt and its child nodes
#' 
#' Production statement for the work at the appropriate level: marked-up document; 
#' marked-up document source; study; study description, other material; 
#' other material for study. More information on these elements, especially 
#' their allowed attributes, can be found in the references.
#' 
#' \emph{Parent nodes}
#' 
#' `prodStmt` is contained in the following elements: `citation`; `docSrc`; 
#' `fileCitation`; and `sourceCitation`.
#'
#' \emph{prdStmt specific child nodes}
#' 
#' `ddi_copyright()` is the copyright statement for the work at the appropriate 
#' level. Copyright for data collection (codeBook/stdyDscr/citation/prodStmt/copyright) 
#' maps to Dublin Core Rights. Inclusion of this element is recommended.
#' 
#' `ddi_fundAg()` is the source(s) of funds for production of the work. If 
#' different funding agencies sponsored different stages of the production 
#' process, use the "role" attribute to distinguish them.
#' 
#' `ddi_grantNo()` is the grant/contract number of the project that sponsored 
#' the effort. If more than one, indicate the appropriate agency using the 
#' "agency" attribute. If different funding agencies sponsored different stages 
#' of the production process, use the "role" attribute to distinguish the grant 
#' numbers. 
#' 
#' `ddi_prodDate()` is the date when the marked-up document/marked-up document 
#' source/data collection/other material(s) were produced (not distributed or 
#' archived). The ISO standard for dates (YYYY-MM-DD) is recommended for use 
#' with the date attribute. Production date for data collection 
#' (codeBook/stdyDscr/citation/prodStmt/prodDate) maps to Dublin Core Date element.
#' 
#' `ddi_prodPlac()` is the address of the archive or organization that produced 
#' the work.
#' 
#' `ddi_software()` is the software used to produce the work. A "version" 
#' attribute permits specification of the software version number. The 
#' "date" attribute is provided to enable specification of the date (if any) 
#' for the software release. The ISO standard for dates (YYYY-MM-DD) is 
#' recommended for use with the date attribute.
#' 
#' @param ... Child nodes or attributes. 
#' 
#' @return A ddi_node object.
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/prodStmt.html}{prodStmt documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/copyright.html}{copyright documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/fundAg.html}{fundAg documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/grantNo.html}{grantNo documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/prodDate.html}{prodDate documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/prodPlac.html}{prodPlac documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/software.html}{software documentation}
#' 
#' @examples
#' ddi_prodStmt()
#' 
#' # Functions that need to be wrapped in ddi_prodStmt()
#' 
#' ddi_copyright("Copyright(c) ICPSR, 2000")
#' 
#' ddi_fundAg(abbr = "NSF", role = "infrastructure", "National Science Foundation")
#' 
#' ddi_grantNo(agency = "Bureau of Justice Statistics", "J-LEAA-018-77")
#' 
#' ddi_prodDate(date = "2022-01-01", "January 1, 2022")
#' 
#' ddi_prodPlac("Place of production")
#' 
#' ddi_software(version = "6.12", "SAS")
#' 
#' @export
ddi_prodStmt <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "prodStmt")
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

#' @rdname ddi_prodStmt
#' @export
ddi_copyright <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "copyright")
  }

  build_leaf_node(
    "copyright",
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_prodStmt
#' @export
ddi_fundAg <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                         "abbr", "role")
    attribs <- validate_attributes(attribs, allowed_attribs, "fundAg")
  }
  
  build_leaf_node(
    "fundAg",
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_prodStmt
#' @export
ddi_grantNo <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                         "agency", "role")
    attribs <- validate_attributes(attribs, allowed_attribs, "grantNo")
  }
  
  build_leaf_node(
    "grantNo",
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_prodStmt
#' @export
ddi_prodDate <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "date")
    attribs <- validate_attributes(attribs, allowed_attribs, "prodDate")
  }

  build_leaf_node(
    "prodDate",
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_prodStmt
#' @export
ddi_prodPlac <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "prodPlac")
  }

  build_leaf_node(
    "prodPlac",
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_prodStmt
#' @export
ddi_software <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "date", "version")
    attribs <- validate_attributes(attribs, allowed_attribs, "software")
  }

  build_leaf_node(
    "software",
    attribs = attribs,
    content = components$content
  )   
}


#' serStmt and its child nodes
#' 
#' Series statement for the work at the appropriate level: marked-up document; 
#' marked-up document source; study; study description, other material; other 
#' material for study. More information on these elements, especially their 
#' allowed attributes, can be found in the references.
#' 
#' \emph{Parent nodes}
#' 
#' `serStmt` is contained in the following elements: `citation`; `docSrc`; 
#' `fileCitation`; and `sourceCitation`.
#' 
#' \emph{serStmt specific child nodes}
#' 
#' `ddi_serInfo()` is the series information. This element contains a history of 
#' the series and a summary of those features that apply to the series as a whole.
#' 
#' `ddi_serName()` is the name of the series to which the work belongs.
#' 
#' @param ... Child nodes or attributes. 
#' 
#' @return A ddi_node object.
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/serStmt.html}{serStmt documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/serInfo.html}{serInfo documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/serName.html}{serName documentation}
#' 
#' @examples
#' ddi_serStmt()
#' 
#' # Functions that need to be wrapped in ddi_serStmt()
#' 
#' ddi_serInfo("Series abstract...")
#' 
#' ddi_serName(abbr="SN", "Series Name")
#' 
#' @export
ddi_serStmt <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "URI")
    attribs <- validate_attributes(attribs, allowed_attribs, "serStmt")
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

#' @rdname ddi_serStmt
#' @export
ddi_serInfo <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "serInfo")
  }
  
  build_leaf_node(
    "serInfo",
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_serStmt
#' @export
ddi_serName <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "abbr")
    attribs <- validate_attributes(attribs, allowed_attribs, "serName")
  }

  build_leaf_node(
    "serName",
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_citation
#' @export
ddi_biblCit <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "format")
    attribs <- validate_attributes(attribs, allowed_attribs, "biblCit")
  }

  build_leaf_node(
    "biblCit",
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_citation
#' @export
ddi_holdings <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "location", "callno", "URI", "media")
    attribs <- validate_attributes(attribs, allowed_attribs, "holdings")
  }

  build_leaf_node(
    "holdings",
    attribs = attribs,
    content = components$content
  )   
}

#' ddi_distStmt and its children
#'
#' Distribution statement for the work at the appropriate level: marked-up 
#' document; marked-up document source; study; study description, 
#' other material; other material for study. More information on these 
#' elements, especially their allowed attributes, can be found in the references.
#' 
#' \emph{Parent nodes}
#' 
#' `distStmt` is contained in the following elements: `citation`; `docSrc`; 
#' `fileCitation`; and `sourceCitation`.
#' 
#' \emph{distStmt specific child nodes}
#' 
#' `ddi_depDate()` is the date that the work was deposited with the archive that 
#' originally received it. The ISO standard for dates (YYYY-MM-DD) is recommended 
#' for use with the "date" attribute.
#' 
#' `ddi_depositr()` is the name of the person (or institution) who provided this 
#' work to the archive storing it. 
#' 
#' `ddi_distDate()` is the date that the work was made available for 
#' distribution/presentation. The ISO standard for dates (YYYY-MM-DD) is 
#' recommended for use with the "date" attribute. If using a text entry in the 
#' element content, the element may be repeated to support multiple language expressions.
#' 
#' `ddi_distrbtr()` is the organization designated by the author or producer to 
#' generate copies of the particular work including any necessary editions or 
#' revisions. Names and addresses may be specified and other archives may be 
#' co-distributors. A URI attribute is included to provide an URN or URL to the 
#' ordering service or download facility on a Web site. 
#'
#' @param ... Child nodes or attributes. 
#' 
#' @return A ddi_node object.
#'
#' @section Shared and complex child nodes:
#' * [ddi_contact()]
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/distStmt.html}{distStmt documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/depDate.html}{depDate documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/depositr.html}{depositr documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/distDate.html}{distDate documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/distrbtr.html}{distrbtr documentation}
#' 
#' @examples 
#' ddi_distStmt()
#' 
#' # Functions that need to be wrapped in ddi_distStmt()
#' 
#' ddi_depDate(date = "2022-01-01", "January 1, 2022")
#' 
#' ddi_depositr(abbr = "BJS", 
#'              affiliation = "U.S. Department of Justice",
#'              "Bureau of Justice Statistics")
#' 
#' ddi_distDate(date = "2022-01-01", "January 1, 2022")
#' 
#' ddi_distrbtr(abbr = "ICPSR",
#'              affiliation = "Institute for Social Research",
#'              URI = "http://www.icpsr.umich.edu",
#'              "Ann Arbor, MI: Inter-university Consortium for Political and Social Research")
#' 
#' @export
ddi_distStmt <- function(...) {
  allowed_children <- c(
    "distrbtr",
    "contact",
    "depositr",
    "depDate",
    "distDate"
  )

  components <- dots_to_xml_components(...)
  attribs <- components$attribs    
  
  if (!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "distStmt")
  }

  build_branch_node(
    "distStmt",
    allowed_children = allowed_children,
    components = components,
    attribs = attribs
  )
}

#' @rdname ddi_distStmt
#' @export
ddi_depDate <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "date")
    attribs <- validate_attributes(attribs, allowed_attribs, "depDate")
  }

  build_leaf_node(
    "depDate",
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_distStmt
#' @export
ddi_depositr <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "abbr", "affiliation")
    attribs <- validate_attributes(attribs, allowed_attribs, "depositr")
  }

  build_leaf_node(
    "depositr",
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_distStmt
#' @export
ddi_distDate <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "date")
    attribs <- validate_attributes(attribs, allowed_attribs, "distDate")
  }

  build_leaf_node(
    "distDate",
    attribs = attribs,
    content = components$content
  )   
}

#' @rdname ddi_distStmt
#' @export
ddi_distrbtr <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "abbr", "affiliation", "URI")
    attribs <- validate_attributes(attribs, allowed_attribs, "distrbtr")
  }

  build_leaf_node(
    "distrbtr",
    attribs = attribs,
    content = components$content
  )   
}