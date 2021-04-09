# Checks if attributes are in the set
check_attribs_in_set <- function(attributes, vals, field) {
    for(a in attributes) {
        if (!is.element(a, vals)) {
            rddi_err(c( "'{a}' is not an acceptable variable in {field} \n",
            "These are the accepted natures: [{glue_collapse(vals, ', ')}]"
        ))
        }
    }
	invisible(attributes)
}

check_attribs <- function(attribs) {
    if ("nature" %in% names(attribs)) check_attribs_in_set(attribs$nature, c("nominal", "ordinal", "interval", "ratio", "percent", "other"), field = "nature")
    if ("aggrMeth" %in% names(attribs)) check_attribs_in_set(attribs$aggrMeth, c("sum", "average", "count", "mode", "median", "maximum", "minimum", "percent", "other"), field = "aggrMeth")
    if ("representationType" %in% names(attribs)) check_attribs_in_set(attribs$representationType, c("text", "numeric", "code", "datetime", "other"), field = "representationType")
    if ("additivity" %in% names(attribs)) check_attribs_in_set(attribs$additivity, c("stock", "flow", "non-additive", "other"), field = "additivity")
    if ("responseDomainType" %in% names(attribs)) check_attribs_in_set(attribs$responseDomainType, c("text", "numeric", "code", "category", "datetime", "geographic", "multiple", "other"), field = "responseDomainType")
    if ("schema" %in% names(attribs)) check_attribs_in_set(attribs$schema, c("SAS", "SPSS", "IBM", "ISO", "XML-Data", "other"), field = "schema")
    if ("category" %in% names(attribs)) check_attribs_in_set(attribs$category, c("date", "time", "currency", "other"), field = "category")
    if ("source" %in% names(attribs)) check_attribs_in_set(attribs$source, c("producer", "archive"), field = "source")
    if ("intrvl" %in% names(attribs)) check_attribs_in_set(attribs$intrvl, c("contin", "discrete"), field = "intrvl")
    if ("wgt" %in% names(attribs)) check_attribs_in_set(attribs$wgt, c("wgt", "not-wgt"), field = "wgt")
    if ("wgtd" %in% names(attribs)) check_attribs_in_set(attribs$wgtd, c("wgtd", "not-wgtd"), field = "wgtd")
    if ("temporal" %in% names(attribs)) check_attribs_in_set(attribs$temporal, c("Y", "N"), field = "temporal")
    if ("geog" %in% names(attribs)) check_attribs_in_set(attribs$geog, c("Y", "N"), field = "geog")
    if ("missing" %in% names(attribs)) check_attribs_in_set(attribs$missing, c("Y", "N"), field = "missing")
    if ("excls" %in% names(attribs)) check_attribs_in_set(attribs$excls, c("true", "false"), field = "excls")
    if ("clusion" %in% names(attribs)) check_attribs_in_set(attribs$clusion, c("I", "E"), field = "clusion")
    if ("compl" %in% names(attribs)) check_attribs_in_set(attribs$compl, c("true", "false"), field = "compl")
    if ("event" %in% names(attribs)) check_attribs_in_set(attribs$event, c("notBefore", "notAfter"), field = "event")
    if ("UNITS" %in% names(attribs)) check_attribs_in_set(attribs$UNITS, c("INT", "REAL"), field = "UNITS")

    # check for xml:lang
    if ("xml:lang" %in% names(attribs)) check_xmlLanguage(attribs$`xml:lang`)   
 
    #check for character strings 
    chr_strings <- c("name", "elementVersion", "vendor", "cdml", "rectype", "geoVocab", "measUnit", "scale", "origin", "geoVocab", 
                "catQnty", "type", "subject", "levelnm", "missType", "country", "level", "resp", "seqNo", "date", "format", "URI", "mapformat",
                "vocab", "vocabURI", "formatname", "levelno", "affiliation", "syntax", "VALUE", "min", "minExclusive", "max", "maxExclusive",
                "StartPos", "EndPos", "width", "RecSegNo", "nCube")
    if(any(names(attribs) %in% chr_strings)) check_strings(attribs[names(attribs) %in% chr_strings])
    
    #check for NCName, the restriction on ID & IDREF. For loop for IDREFS
    ncnames <-c("ID", "wgt-var", "weight", "qstn", "files", "sdatrefs", "methrefs", "pubrefs", "access", "parent", "sameNote", "catgry", "catGrp", "var",
                "varRef", "fileid", "locMap")
    if(any(names(attribs) %in% ncnames)) check_ncname(attribs[names(attribs) %in% ncnames])
        
    # check for NMTOKEN 
    nmtoken <- c("otherAggrMeth", "otherAdditivity", "otherRepresentionType", "otherResponseDomainType", "otherType", "otherSchema", "otherCategory")
    if(any(names(attribs) %in% nmtoken)) check_nmtoken(attribs[names(attribs) %in% nmtoken])

    # anyURI has no pattern validation
    if(any(names(attribs) %in% c("ddiLifecycleUrn", "ddiCodebookUrn"))) check_anyURI(attribs[names(attribs) %in% c("ddiLifecycleUrn", "ddiCodebookUrn")])

    # check for xs:dateTime, xs:date, xs:gYearMonth, xs:gYear
    if("elementVersionDate" %in% names(attribs)) check_elementVersionDate(attribs$elementVersionDate)

    invisible(attribs)
}

check_strings <- function(attribs) {
    for(name in names(attribs)) {
        if(!is.character(attribs[[name]])) rddi_err("{name} must be a character string")
    }
}

check_ncname <- function(attribs) {
    reg_expr <- "^[A-Za-z_][-._A-Za-z0-9]*$"
    idrefs <- c("wgt-var", "weight", "qstn", "files", "sdatrefs", "methrefs", "pubrefs", "access", "catgry", "catGrp")
    for(name in names(attribs)) {
        if(any(name %in% idrefs)) {
            for(a in unlist(strsplit(attribs[[name]], " "))) {
                if(!grepl(reg_expr, a)) rddi_err("IDREF {a} in {name} must start with a character or underscore and can only include alphanumeric characters along with -._")
            }
        } else {
            if(!grepl(reg_expr, attribs[[name]])) rddi_err("{name} must start with a character or underscore and can only include alphanumeric characters along with -._")
        }
    }
}

check_nmtoken <- function(attribs) {
    reg_expr <- "^[^ ,]*$"
    for(name in names(attribs)) {
        if(!grepl(reg_expr, attribs)) rddi_err("'{name}' cannot contain a space or a comma")
    }
}

check_anyURI <- function(attribs) {

    reg_expr <- "^[^ #]+#{0,1}[^ #]*$"
    for (name in names(attribs)) {
        if(!grepl(reg_expr, attribs)) rddi_err("'{name}' cannot contain a space or more than one hashtag")
    }
}

check_elementVersionDate <- function(attribs) {
    gYear <- "^[0-9]{4}$"
    gYearMonth <- "^[0-9]{4}-[0-9]{2}"
    datetime <- "^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$"
    dateOnly <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"
    if(grepl(gYear, attribs) | grepl(gYearMonth, attribs) | grepl(datetime, attribs) | grepl(dateOnly, attribs)) {
        if(grepl(datetime, attribs)) {
            if(is.na(as.POSIXct(strptime(attribs, format = "%Y-%m-%d %H:%M:%S")))) rddi_err("elementVersionDate must be a real date/time")
        } else if(grepl(dateOnly, attribs)) {
            if(is.na(as.POSIXct(strptime(attribs, format = "%Y-%m-%d")))) rddi_err("elementVersionDate must be a real date")
        } else if(grepl(gYearMonth, attribs)) {
            if (as.numeric(substr(attribs, 6, 7)) < 1 | as.numeric(substr(attribs, 6,7)) > 12) rddi_err("elementVersionDate must have a month between 1 and 12")
        }
    }
    else rddi_err("elementVersionDate must be in YYYY, YYYY-MM, YYYY-MM-DD, or YYYY-MM-DD HH:MM:SS format")
}

check_xmlLanguage <- function(attribs) {
    reg_expr <- "^[a-zA-Z]{1,8}-?[a-zA-Z0-9]{0,8}$" 
    if(!grepl(reg_expr, attribs)) rddi_err("'xml:lang' does not match xs:language standard")
}