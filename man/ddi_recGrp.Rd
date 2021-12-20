% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fileTxt.R
\name{ddi_recGrp}
\alias{ddi_recGrp}
\title{recGrp and its child nodes}
\usage{
ddi_recGrp(...)
}
\arguments{
\item{...}{Any parameters from the DDI Codebook 2.5 schema.}
}
\description{
Used to describe record groupings if the file is hierarchical or relational.
More information on the allowed attributes for fileStrc and its child nodes
can be found below and in the references.
}
\details{
#' @section Branch node children allowed:
\itemize{
\item \code{\link[=ddi_recDimnsn]{ddi_recDimnsn()}}
}
}
\section{General children allowed}{

\itemize{
\item \code{\link[=ddi_labl]{ddi_labl()}}
}
}

\references{
\href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/fileStrc.html}{fileStrc documentation}
}