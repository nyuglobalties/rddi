# Checks if attributes are in the set
check_attribs_in_set <- function(attributes, vals, field) {
    if (!any(is.element(attributes, vals))) {
        rddi_err(c( "'{vals}' is not an acceptable variable in {field} \n",
        "These are the accepted natures: [{glue_collapse(attributes, ', ')}]"
      ))

    }
	invisible(attributes)
}

check_attribs <- function(attribs) {
    if(!is.na(attribs) & length(attribs) == 1) {
        check_attrib(attribs)
    } else if(!is.na(attribs) & length(attribs > 1)){
        lapply(attribs, check_attrib)
    }
}

check_attrib <- function(attribs) {
    chr_strings <- c("name", "elementVersion", "vendor", "cdml", "rectype", "geoVocab", "measUnit", "scale", "origin", "geoVocab", 
                "catQnty", "type", "subject", "levelnm", "missType", "country", "level", "resp", "seqNo", "date", "format", "URI", "mapformat",
                "vocab", "vocabURI", "formatname", "levelno", "affiliation", "syntax", "VALUE", "min", "minExclusive", "max", "maxExclusive",
                "StartPos", "EndPos", "width", "RecSegNo", "nCube")

    if ("nature" %in% names(attribs)) {
        check_attribs_in_set(attribs$nature, c("nominal", "ordinal", "interval", "ratio", "percent", "other"), field = "nature")
    }
    if ("aggrMeth" %in% names(attribs)){
        check_attribs_in_set(attribs$aggrMeth, c("sum", "average", "count", "mode", "median", "maximum", "minimum", "percent", "other"), field = "aggrMeth")
    }
    if ("representationType" %in% names(attribs)) {
        check_attribs_in_set(attribs$representationType, c("text", "numeric", "code", "datetime", "other"), field = "representationType")  
    }
    if ("additivity" %in% names(attribs)) {
        check_attribs_in_set(attribs$additivity, c("stock", "flow", "non-additive", "other"), field = "additivity")
    }
    if ("responseDomainType" %in% names(attribs)) {
        check_attribs_in_set(attribs$responseDomainType, c("text", "numeric", "code", "category", "datetime", "geographic", "multiple", "other"), field = "responseDomainType")
    }
    if ("schema" %in% names(attribs)) {
        check_attribs_in_set(attribs$schema, c("SAS", "SPSS", "IBM", "ISO", "XML-Data", "other"), field = "schema")
    }
    if ("category" %in% names(attribs)) {
        check_attribs_in_set(attribs$category, c("date", "time", "currency", "other"), field = "category")
    }
    if ("xml:lang" %in% names(attribs)) { # language check CAN I USE A COLON TO MAKE A NAMED VARIABLE IN A LIST?
        reg_expr <- "[a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*" # This is from the xml.xsd schema
        if(!regmatches(attribs, reg_expr)) rddi_err("'{names(attribs)}'' does not match xs:language standard")
    }
    if ("source" %in% names(attribs)) {
        check_attribs_in_set(attribs$source, c("producer", "archive"), field = "source")
    }
    if ("intrvl" %in% names(attribs)) {
        check_attribs_in_set(attribs$intrvl, c("contin", "discrete"), field = "intrvl")
    }
    if ("wgt" %in% names(attribs)) {
        check_attribs_in_set(attribs$wgt, c("wgt", "not-wgt"), field = "wgt")
    }
    if ("wgtd" %in% names(attribs)) {
        check_attribs_in_set(attribs$wgtd, c("wgtd", "not-wgtd"), field = "wgtd")
    }
    if ("temporal" %in% names(attribs)) {
        check_attribs_in_set(attribs$temporal, c("Y", "N"), field = "temporal")
    }
    if ("geog" %in% names(attribs)) {
        check_attribs_in_set(attribs$geog, c("Y", "N"), field = "geog")
    }
    if ("missing" %in% names(attribs)) {
        check_attribs_in_set(attribs$missing, c("Y", "N"), field = "missing")
    }
    if ("excls" %in% names(attribs)) {
        check_attribs_in_set(attribs$excls, c("true", "false"), field = "excls")
    }
    if ("clusion" %in% names(attribs)) {
        check_attribs_in_set(attribs$clusion, c("I", "E"), field = "clusion")
    }
    if ("compl" %in% names(attribs)) {
        check_attribs_in_set(attribs$compl, c("true", "false"), field = "compl")
    }
    if ("event" %in% names(attribs)) {
        check_attribs_in_set(attribs$event, c("notBefore", "notAfter"), field = "event")
    }
    if ("UNITS" %in% names(attribs)) {
        check_attribs_in_set(attribs$UNITS, c("INT", "REAL"), field = "UNITS")
    }

    #check for character strings 
    if(any(names(attribs) %in% chr_strings)) {
        sub_list <- attribs[names(attribs) %in% chr_strings]
        for(name in names(sub_list)) {
            if(!is.character(sub_list[[name]])) rddi_err("`{name}` must be a character string.")
        }
    }
    
    #check for NCName, the restriction on ID & IDREF. For loop for IDREFS
    if(any(names(attribs) %in% c("ID", "wgt-var", "weight", "qstn", "files", "sdatrefs", "methrefs", "pubrefs", "access", "parent", "sameNote", "catgry", "catGrp", "var",
                "varRef", "fileid", "locMap"))) {
        reg_expr <- "^[A-Za-z_][^:@$%\\/+,; ]*"
        for(a in attribs) { # IDREFS is a list of IDREF, this should go through them
            if(!regmatches(b, reg_expr)) rddi_err("'{names(attribs)}' cannot start with a number, dot, or minus side and cannot include :@$%&/+,; or a space ")
        }
    }    
    # check for NMTOKEN 
    if(any(names(attribs) %in% c("otherAggrMeth", "otherAdditivity", "otherRepresentionType", "otherResponseDomainType", "otherType", "otherSchema", "otherCategory"))) {
        reg_expr <- "[^ ,]*"
        if(!regmatches(attribs, reg_expr)) rddi_err("'{names(attribs)}' cannot contain a space or a comma")
    }
    # anyURI has no pattern validation
    if(any(names(attribs) %in% c("ddiLifecycleURN", "ddiCodebookURN"))) {
        reg_expr <- "[^ #]+#{0,1}[^ #]*" 
        if(!regmatches(attribs, reg_expr)) rddi_err("'{names(attribs)}' cannot contain a space or more than one hashtag")
    }
    # check for xs:dateTime, xs:date, xs:gYearMonth, xs:gYear
    if("elementVersionDate" %in% names(attribs)) {
        if(regmatches(attribs$elementVersionDate, "[0-9]{4}") | regmatches(attribs$elementVersionDate, "[0-9]{4}-[0-9]{2}") |
            regmatches(attribs$elementVersionDate, "[0-9]{4}-[0-9]{2}-[0-9]{2}") | regmatches(attribs$elementVersionDate, "[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}")) {
            if(regmatches(attribs$elementVersionDate, "[0-9]{4}-[0-9]{2}")) {
                if(!(as.numeric(substr(attribs$elementVersionDate, 6,7)) > 0 & as.numeric(substr(attribs$elementVersionDate, 6, 7)) <= 12)) rddi_err("elementVersionDate in YYYY-MM format must be an applicable month between 01 & 12")
            }
            else if(regmatches(attribs$elementVersionDate, "[0-9]{4}-[0-9]{2}-[0-9]{2}") | regmatches(a, "[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}")) {
                if (!is.na(as.POSIXct(attribs$elementVersionDate, tryFormats= c("%Y-%m-%d", "%Y-%m-%d %H:%M:%OS"), optional = FALSE))) rddi_err("elementVersionDate is not a valid date or datetime.")
            }
        }
        else rddi_err("elementVersionDate must be in YYYY, YYYY-MM, YYYY-MM-DD, or YYYY-MM-DD HH:MM:SS format")
    }

    invisible(attribs)
}