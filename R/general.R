#' labl node
#' 
#' A short description of the parent element. In the variable label, the length 
#' of this phrase may depend on the statistical analysis system 
#' used (e.g., some versions of SAS permit 40-character labels, while some 
#' versions of SPSS permit 120 characters), although the DDI itself imposes no 
#' restrictions on the number of characters allowed. More information on this 
#' element, especially its allowed attributes, can be found in the references. 
#' 
#' \emph{Parent nodes}
#' 
#' `labl` is contained in the following elements: `catgry`; `catgryGrp`; `nCube`; 
#' `nCubeGrp`; `otherMat`; `recGrp`; `sampleFrame`; `var`; and `varGrp`.
#'
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/labl.html}{labl documentation}
#' 
#' @examples 
#' ddi_labl(level = "variable", lang = "en", "short variable description")
#'  
#' @export
ddi_labl <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
 
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", 
                        "level", "vendor", "country", "sdatrefs")
    attribs <- validate_attributes(attribs, allowed_attribs, "labl")
  }

  build_leaf_node (
    "labl",
    attribs = attribs,
    content = components$content
  )
}

#' notes node
#' 
#' For clarifying information/annotation regarding the parent element. More 
#' information on this element, especially its allowed attributes, can be found 
#' in the references.
#' 
#' \emph{Parent nodes}
#' 
#' `notes` is contained in the following elements: `citation`; `dataAccs`; 
#' `dataDscr`; `docDscr`; `docSrc`; `fileCitation`; `fileDscr`' `fileStrc`;
#' `invalrng`; `method`; `nCube`; `nCubeGrp`; `otherMat`; `setAvail`; 
#' `sourceCitation`; `stdyDscr`; `stdyInfo`; `valrng`; `var`; `varGrp`; and 
#' `verStmt`.
#' 
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/notes.html}{notes documentation}
#'  
#' @examples 
#' ddi_notes(resp = "Jane Smith", "The source codebook was produced from original 
#'                                hardcopy materials using Optical Character Recognition (OCR).")
#' 
#' @export
ddi_notes <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", 
                        "type", "subject", "level", "resp", "sdatrefs", "parent", "sameNote")
    attribs <- validate_attributes(attribs, allowed_attribs, "notes")
  }

  build_leaf_node (
    "notes",
    attribs = attribs,
    content = components$content
  )
}

#' universe node
#'
#' The group of persons or other elements that are the object of research and 
#' to which any analytic results refer. Age, nationality, and residence 
#' commonly help to delineate a given universe, but any of a number of factors 
#' may be involved, such as sex, race, income, veteran status, criminal 
#' convictions, etc. The universe may consist of elements other than persons, 
#' such as housing units, court cases, deaths, countries, etc. In general, it 
#' should be possible to tell from the description of the universe whether a 
#' given individual or element (hypothetical or real) is a member of the 
#' population under study. More information on this element, especially its 
#' allowed attributes, can be found 
#' in the references.
#' 
#' \emph{Parent nodes}
#' 
#' `universe` is contained in the following elements: `nCube`; `nCubeGrp`; 
#' `sampleFrame`; `sumDscr`; `var`; and `varGrp`.
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/universe.html}{universe documentation}
#' 
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#' 
#' @examples
#' ddi_universe(clusion = "I", "Individuals 15-19 years of age.")
#' 
#' @export
ddi_universe <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", 
                      "level", "clusion") 
    attribs <- validate_attributes(attribs, allowed_attribs, "universe")
  }
  
  if(length(components$content) == 1 & is.character(components$content[[1]])) {
    build_leaf_node(
      "universe",
      attribs = attribs,
      content = components$content
    )
  } else {
    allowed_children = c("concept", "txt")
    build_branch_node(
      "universe",
      content = unwrap_content(components$content),
      attribs = components$attribs,
      allowed_children = allowed_children
    )
  }
}

#' concept node
#' 
#' The general subject to which the parent element may be seen as pertaining. 
#' This element serves the same purpose as the keywords and topic classification 
#' elements, but at the data description level. The "vocab" attribute is 
#' provided to indicate the controlled vocabulary, if any, used in the element, 
#' e.g., LCSH (Library of Congress Subject Headings), MeSH (Medical Subject 
#' Headings), etc. The "vocabURI" attribute specifies the location for the full 
#' controlled vocabulary. More information on this element, especially its 
#' allowed attributes, can be found in the references.
#' 
#' \emph{Parent nodes}
#' 
#' `concept` is contained in the following elements: `anlyUnit`; `anlysUnit`; 
#' `collMode`; `dataKind`; `geogCover`; `geogUnit`; `nCubeGrp`; `nation`; 
#' `resInstru`; `sampProc`; `srcOrig`; `timeMeth`; `universe`; `var`; and `varGrp`.
#' 
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/concept.html}{concept documentation}
#' 
#' @examples 
#' ddi_concept(vocab = "LCSH",
#'             vocabURI = "http://lcweb.loc.gov/catdir/cpso/lcco/lcco.html",
#'             source = "archive",
#'             "more experience")
#'  
#' @export
ddi_concept <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", 
                        "vocab", "vocabURI")
    attribs <- validate_attributes(attribs, allowed_attribs, "concept")
  }

  build_leaf_node(
    "concept",
    attribs = attribs,
    content = components$content
  )
}

#' verStmt and its child nodes
#' 
#' This is the version statement for the work at the appropriate level: 
#' marked-up document; marked-up document source; study; study description, 
#' other material; other material for study. More information on these elements, 
#' especially their allowed attributes, can be found in the references. 
#' 
#' \emph{Parent nodes}
#' 
#' `verStmt` is contained in the following elements: `citation`; `docSrc`; 
#' `fileCitation`; `fileTxt`; `nCube`; `sourceCitation`; and `var`.
#' 
#' \emph{verStmt specific child nodes}
#' 
#' * `ddi_verResp()` is the organization or person responsible for the version of the 
#' work.
#' 
#' * `ddi_version()` is also known as release or edition. If there have been 
#' substantive changes in the data/documentation since their creation, this 
#' statement should be used at the appropriate level. The ISO standard for 
#' dates (YYYY-MM-DD) is recommended for use with the "date" attribute. 
#' 
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#'
#' @section Shared and complex child nodes:
#' * [ddi_notes()] 
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/verStmt.html}{verStmt documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/verResp.html}{verResp documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/version.html}{version documentation}
#'  
#' @examples 
#' ddi_verStmt()
#' 
#' # Functions that need to be wrapped in ddi_verStmt()
#' 
#' ddi_verResp("Zentralarchiv fuer Empirische Sozialforschung")
#' 
#' ddi_version(type = "edition",
#'             date = "1999-01-25",
#'             "Second ICPSR Edition")
#' 
#' @export
ddi_verStmt <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn") 
    attribs <- validate_attributes(attribs, allowed_attribs, "verStmt")
  }

  allowed_children <- c(
    "version",
    "verResp",
    "notes"
  )

  build_branch_node(
    "verStmt",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_verStmt 
#' @export
ddi_verResp <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                         "affiliation") 
    attribs <- validate_attributes(attribs, allowed_attribs, "verResp")
  }
  
  build_leaf_node(
    "verResp",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_verStmt 
#' @export
ddi_version <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "date", "type") 
    attribs <- validate_attributes(attribs, allowed_attribs, "version")
  }

  build_leaf_node(
    "version",
    attribs = attribs,
    content = components$content
  )
}

#' txt node
#' 
#' Lengthier description of the parent element. More information on this 
#' element, especially its allowed attributes, can be found in the references.
#' 
#' \emph{Parent nodes}
#' 
#' `txt` is contained in the following elements: `anlyUnit`; `anlysUnit`; 
#' `catgry`; `catgryGrp`; `codingInstructions`; `collMode`; `dataKind`; 
#' `frameUnit`; `geogCover`; `geogUnit`; `nCube`; `nCubeGrp`; `nation`; 
#' `otherMat`; `resInstru`; `sampProc`; `sampleFrame`; `srcOrig`; `timeMeth`;
#' `universe`; `var`; and `varGrp`.
#'
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/txt.html}{txt documentation}
#'  
#' @examples 
#' ddi_txt("The following five variables refer to respondent attitudes toward 
#'          national environmental policies: air pollution, urban sprawl, noise 
#'          abatement, carbon dioxide emissions, and nuclear waste.")
#' 
#' @export
ddi_txt <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn", 
                        "level", "sdatrefs")
    attribs <- validate_attributes(attribs, allowed_attribs, "txt")
  }

  build_leaf_node(
    "txt",
    attribs = attribs,
    content = components$content 
  )
}


#' producer node
#' 
#' The producer is the person or organization with the financial or 
#' administrative responsibility for the physical processes whereby the 
#' document was brought into existence. Use the "role" attribute to distinguish 
#' different stages of involvement in the production process, such as original 
#' producer. Producer of data collection 
#' (codeBook/stdyDscr/citation/prodStmt/producer) maps to Dublin Core Publisher 
#' element. The "producer" in the Document Description should be the agency or 
#' person that prepared the marked-up document. More information on this 
#' element, especially its allowed attributes, can be found in the references.
#' 
#' \emph{Parent nodes}
#' 
#' `producer` is contained in the following elements: `prodStmt` and `standard`.
#' 
#' @param ... Child nodes or attributes. 
#' 
#' @return A ddi_node object.
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/producer.html}{producer documentation}
#' 
#' @examples 
#' ddi_producer(abbr = "MNPoll",
#'              affiliation = "Minneapolis Star Tribune Newspaper",
#'              role = "origianl producer",
#'              "Star Tribune Minnesota Poll")
#' 
#' @export
ddi_producer <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "abbr", "role", "affiliation")
    attribs <- validate_attributes(attribs, allowed_attribs, "producer")
  }

  build_leaf_node(
    "producer",
    attribs = attribs,
    content = components$content
  )   
}

#' contact node
#' 
#' Names and addresses of individuals responsible for the work. Individuals 
#' listed as contact persons will be used as resource persons regarding problems 
#' or questions raised by the user community. The URI attribute should be used 
#' to indicate a URN or URL for the homepage of the contact individual. The 
#' email attribute is used to indicate an email address for the contact 
#' individual. More information on this element, especially its allowed 
#' attributes, can be found in the references.
#' 
#' \emph{Parent nodes}
#' 
#' `contact` is contained in the following elements: `distStmt` and `useStmt`.
#' 
#' @param ... Child nodes or attributes. 
#' 
#' @return A ddi_node object.
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/contact.html}{contact documentation}
#' 
#' @examples 
#' ddi_contact(affiliation = "University of Wisconson",
#'             email = "jsmith@...",
#'             "Jane Smith")
#' 
#' @export
ddi_contact <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "affiliation", "URI", "email")
    attribs <- validate_attributes(attribs, allowed_attribs, "contact")
  }

  build_leaf_node(
    "contact",
    attribs = attribs,
    content = components$content
  )
}

#' dataSrc node
#' 
#' Used to list the book(s), article(s), serial(s), and/or machine-readable 
#' data file(s)--if any--that served as the source(s) of the data collection.
#' More information on this element, especially its allowed attributes, can be 
#' found in the references.
#' 
#' \emph{Parent nodes}
#' 
#' `dataSrc` is contained in the following elements: `sources` and `resource`. 
#' 
#' @param ... Child nodes or attributes. 
#' 
#' @return A ddi_node object.
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataSrc.html}{dataSrc documentation}
#' 
#' @examples 
#' ddi_dataSrc('"Voting Scores." CONGRESSIONAL QUARTERLY ALMANAC 33 (1977), 487-498.')
#' 
#' @export
ddi_dataSrc <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "dataSrc")
  }

  build_leaf_node(
    "dataSrc",
    attribs = attribs,
    content = components$content
  )
}

#' srcChar node
#' 
#' Assessment of characteristics and quality of source material. May not be 
#' relevant to survey data. This element may be repeated to support multiple 
#' language expressions of the content. More information on this element, 
#' especially its allowed attributes, can be found in the references.
#' 
#' \emph{Parent nodes}
#' 
#' `srcChar` is contained in the following elements: `sources` and `resource`. 
#' 
#' @param ... Child nodes or attributes. 
#' 
#' @return A ddi_node object.
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/srcChar.html}{srcChar documentation}
#' 
#' @examples 
#' ddi_srcChar("Assessment of source material(s).")
#'  
#' @export
ddi_srcChar <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "srcChar")
  }

  build_leaf_node(
    "srcChar",
    attribs = attribs,
    content = components$content
  )
}

#' srcDocu node
#' 
#' Level of documentation of the original sources. May not be relevant to 
#' survey data. This element may be repeated to support multiple language 
#' expressions of the content. More information on this element, especially its 
#' allowed attributes, can be found in the references.
#' 
#' \emph{Parent nodes}
#' 
#' `srcDocu` is contained in the following elements: `sources` and `resource`. 
#' 
#' @param ... Child nodes or attributes. 
#' 
#' @return A ddi_node object.
#'
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/srcDocu.html}{srcDocu documentation}
#' 
#' @examples 
#' ddi_srcDocu("Description of documentation of source material(s).")
#' 
#' @export
ddi_srcDocu <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "srcDocu")
  }

  build_leaf_node(
    "srcDocu",
    attribs = attribs,
    content = components$content
  )
}

#' srcOrig documentation
#' 
#' For historical materials, information about the origin(s) of the sources and 
#' the rules followed in establishing the sources should be specified. May not 
#' be relevant to survey data. This element may be repeated to support multiple 
#' language expressions of the content.More information on this element, 
#' especially its allowed attributes, can be found in the references.
#' 
#' \emph{Parent nodes}
#' 
#' `srcOrig` is contained in the following elements: `sources` and `resource`. 
#' 
#' @param ... Child nodes or attributes.
#' 
#' @return A ddi_node object.
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/srcOrig.html}{srcOrig documentation}
#' 
#' @examples 
#' ddi_srcOrig("Origin of source material(s).")
#' 
#' @export
ddi_srcOrig <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "srcOrig")
  }

  if(length(components$content) == 1 & is.character(components$content[[1]])) {
    build_leaf_node(
      "srcOrig",
      attribs = attribs,
      content = components$content
    )
  } else {
    allowed_children = c("concept", "txt")
    build_branch_node(
      "srcOrig",
      content = unwrap_content(components$content),
      attribs = components$attribs,
      allowed_children = allowed_children
    )
  }
}

#' useStmt and its children
#' 
#' Information on terms of use for the data collection. This element may be 
#' repeated only to support multiple language expressions of the content. 
#' More information on these elements, especially their allowed attributes, can 
#' be found in the references. 
#' 
#' \emph{Parent nodes}
#' 
#' `useStmt` is contained in the following elements: `dataAccs` and `sampleFrame`.
#' 
#' \emph{useStmt specific child nodes} 
#'  
#' * `ddi_citReq()` is the citation requirement. This is the text of requirement that 
#' a data collection should be cited properly in articles or other publications 
#' that are based on analysis of the data. 
#'  
#' * `ddi_conditions()` indicates any additional information that will assist the 
#' user in understanding the access and use conditions of the data collection.
#'  
#' * `ddi_confDec()` is the confidentiality declaration. This element is used to 
#' determine if signing of a confidentiality declaration is needed to access a 
#' resource. 
#'  
#' * `ddi_deposReq()` is the deposit requirement. This is information regarding 
#' user responsibility for informing archives of their use of data through 
#' providing citations to the published work or providing copies of the 
#' manuscripts. 
#'  
#' * `ddi_disclaimer()` is information regarding responsibility for uses of the 
#' data collection. This element may be repeated to support multiple language 
#' expressions of the content.
#'
#' * `ddi_restrctn()` are any restrictions on access to or use of the collection 
#' such as privacy certification or distribution restrictions should be 
#' indicated here. These can be restrictions applied by the author, producer, 
#' or disseminator of the data collection. If the data are restricted to only 
#' a certain class of user, specify which type. 
#'  
#' * `ddi_specPerm()` is used to determine if any special permissions are required 
#' to access a resource.
#' 
#' @param ... Child nodes or attributes. 
#' 
#' @return A ddi_node object.
#'
#' @section Shared and complex child nodes:
#' * [ddi_contact()]
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/useStmt.html}{useStmt documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/citReq.html}{citReq documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/conditions.html}{conditions documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/confDec.html}{confDec documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/deposReq.html}{deposReq documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/disclaimer.html}{disclaimer documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/restrctn.html}{restrctn documentation}
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/specPerm.html}{specPerm documentation}
#' 
#' @examples 
#' ddi_useStmt()
#' 
#' # Functions that need to be wrapped in ddi_useStmt()
#' 
#' ddi_citReq(lang = "en",
#'            "Publications based on ICPSR data collections should acknowledge 
#'            those sources by means of bibliographic citations. To ensure that 
#'            such source attributions are captured for social science 
#'            bibliographic utilities, citations must appear in footnotes or in 
#'            the reference section of publications.")
#' 
#' ddi_conditions(lang = "en",
#'                "The data are available without restriction. Potential users 
#'                of these datasets are advised, however, to contact the original 
#'                principal investigator Dr. J. Smith (Institute for Social Research, 
#'                The University of Michigan, Box 1248, Ann Arbor, MI 48106), 
#'                about their intended uses of the data. Dr. Smith would also 
#'                appreciate receiving copies of reports based on the datasets.")
#' 
#' ddi_confDec(formNo = "1",
#'             "To download this dataset, the user must sign a declaration of confidentiality.")
#' 
#' ddi_deposReq("To provide funding agencies with essential information about 
#'              use of archival resources and to facilitate the exchange of 
#'              information about ICPSR participants' research activities, users 
#'              of ICPSR data are requested to send to ICPSR bibliographic 
#'              citations for, or copies of, each completed manuscript or thesis 
#'              abstract. Please indicate in a cover letter which data were used.")
#' 
#' ddi_disclaimer("The original collector of the data, ICPSR, and the relevant 
#'                funding agency bear no responsibility for uses of this collection 
#'                or for interpretations or inferences based upon such uses.")
#' 
#' ddi_restrctn("ICPSR obtained these data from the World Bank under the terms of 
#'              a contract which states that the data are for the sole use of 
#'              ICPSR and may not be sold or provided to third parties outside 
#'              of ICPSR membership. Individuals at institutions that are not 
#'              members of the ICPSR may obtain these data directly from the 
#'              World Bank.")
#' 
#' ddi_specPerm(formNo = "4",
#'              "The user must apply for special permission to use this dataset 
#'              locally and must complete a confidentiality form.")
#' 
#' @export
ddi_useStmt <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
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
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "media", "callno", "label", "type")
    attribs <- validate_attributes(attribs, allowed_attribs, "useStmt")
  }

  build_branch_node(
    "useStmt",
    allowed_children = allowed_children,
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_useStmt
#' @export
ddi_citReq <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "citReq")
  }

  build_leaf_node(
    "citReq",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_useStmt 
#' @export
ddi_conditions <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "conditions")
  }

  build_leaf_node(
    "conditions",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_useStmt
#' @export
ddi_confDec <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                        "required", "formNo", "URI")
    attribs <- validate_attributes(attribs, allowed_attribs, "confDec")
  }

  build_leaf_node(
    "confDec",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_useStmt
#' @export
ddi_deposReq <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "deposReq")
  }

  build_leaf_node(
    "deposReq",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_useStmt
#' @export
ddi_disclaimer <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "disclaimer")
  }

  build_leaf_node(
    "disclaimer",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_useStmt
#' @export
ddi_restrctn <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "restrctn")
  }

  build_leaf_node(
    "restrctn",
    attribs = attribs,
    content = components$content
  )
}

#' @rdname ddi_useStmt
#' @export
ddi_specPerm <- function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs

  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn",
                          "formNo", "URI")
    attribs <- validate_attributes(attribs, allowed_attribs, "specPerm")
  }

  build_leaf_node(
    "specPerm",
    attribs = attribs,
    content = components$content
  )
}

#' titl node 
#' 
#' titl is the full authoritative title for the work at the appropriate level: 
#' marked-up document; marked-up document source; study; other material(s) 
#' related to study description; other material(s) related to study. The study 
#' title will in most cases be identical to the title for the marked-up document. 
#' 'A full title should indicate the geographic scope of the data collection as 
#' well as the time period covered. Title of data collection 
#' '(codeBook/stdyDscr/citation/titlStmt/titl) maps to Dublin Core Title element. 
#' This element is required in the Study Description citation. More information 
#' on this element, especially its allowed attributes, can be found in the 
#' references.
#' 
#' \emph{Parent nodes}
#' 
#' `titl` is contained in the following elements: `table` and `titlStmt`.
#' 
#' @param ... Child nodes or attributes. 
#' 
#' @return A ddi_node object.
#' 
#' @references \href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/titl.html}{titl documentation}
#' 
#' @examples 
#' ddi_titl("Census of Population, 1950 [United States]: Public Use Microdata Sample")
#' 
#' @export
ddi_titl <-  function(...) {
  components <- dots_to_xml_components(...)
  attribs <- components$attribs
  
  if(!is.null(attribs)) {
    allowed_attribs <- c("ID", "xml:lang", "source", "elementVersion", "elementVersionDate", "ddiLifecycleUrn", "ddiCodebookUrn")
    attribs <- validate_attributes(attribs, allowed_attribs, "titl")
  }
  
  build_leaf_node(
    "titl",
    attribs = attribs,
    content = components$content
  )
}