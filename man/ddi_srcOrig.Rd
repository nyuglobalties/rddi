% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/general.R
\name{ddi_srcOrig}
\alias{ddi_srcOrig}
\title{srcOrig documentation}
\usage{
ddi_srcOrig(...)
}
\arguments{
\item{...}{Child nodes or attributes.}
}
\value{
A ddi_node object.
}
\description{
For historical materials, information about the origin(s) of the sources and
the rules followed in establishing the sources should be specified. May not
be relevant to survey data. This element may be repeated to support multiple
language expressions of the content.More information on this element,
especially its allowed attributes, can be found in the references.
}
\details{
\emph{Parent nodes}

\code{srcOrig} is contained in the following elements: \code{sources} and \code{resource}.
}
\examples{
ddi_srcOrig("Origin of source material(s).")

}
\references{
\href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/srcOrig.html}{srcOrig documentation}
}
