.onLoad <- function(libname, pkgname) {
  op <- options()
  op.rddi <- list(
    rddi.id_generator = urn_identifier,
    rddi.agency_id = "com.agency.id"
  )

  toset <- !(names(op.rddi) %in% names(op))
  
  if (any(toset)) options(op.rddi[toset])

  invisible()
}
