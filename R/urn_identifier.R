urn_identifier <- function(object, agency = getOption("rddi.agency_id"), version = "1.000") {
  urn_validate(glue("urn:ddi:{agency}:{object}:{version}"))
}

urn_validate <- function(ids) {
  pattern <- "[Uu][Rr][Nn]:[Dd][Dd][Ii]:[a-zA-Z0-9\\-]{1,63}(\\.[a-zA-Z0-9\\-]{1,63})*:[A-Za-z0-9\\*@$\\-_]+(\\.[A-Za-z0-9\\*@$\\-_]+)?:[0-9]+(\\.[0-9]+)*"

  if (!all(grepl(pattern, ids))) {
    no_match <- ids[!grepl(pattern, ids)]
    
    if (length(no_match) > 3) {
      no_match <- no_match[1:4]
      no_match[4] <- "..."
    }

    rddi_err("Invalid DDI URN: {glue_collapse(no_match, ', ')}")
  }

  ids
}

urn_object_component <- function(id) {
  tryCatch({
    id <- urn_validate(id)

    parts <- strsplit(id, "\\:")[[1]]
    parts[4]
  }, error = function(e) {
    id
  })
}
