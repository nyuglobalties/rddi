% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/citation.R
\name{ddi_distStmt}
\alias{ddi_distStmt}
\alias{ddi_depDate}
\alias{ddi_depositr}
\alias{ddi_distDate}
\alias{ddi_distrbtr}
\title{ddi_distStmt and its children}
\usage{
ddi_distStmt(...)

ddi_depDate(...)

ddi_depositr(...)

ddi_distDate(...)

ddi_distrbtr(...)
}
\arguments{
\item{...}{Child nodes or attributes.}
}
\value{
A ddi_node object.
}
\description{
Distribution statement for the work at the appropriate level: marked-up
document; marked-up document source; study; study description,
other material; other material for study. More information on these
elements, especially their allowed attributes, can be found in the references.
}
\details{
\emph{Parent nodes}

\code{distStmt} is contained in the following elements: \code{citation}; \code{docSrc};
\code{fileCitation}; and \code{sourceCitation}.

\emph{distStmt specific child nodes}

\code{ddi_depDate()} is the date that the work was deposited with the archive that
originally received it. The ISO standard for dates (YYYY-MM-DD) is recommended
for use with the "date" attribute.

\code{ddi_depositr()} is the name of the person (or institution) who provided this
work to the archive storing it.

\code{ddi_distDate()} is the date that the work was made available for
distribution/presentation. The ISO standard for dates (YYYY-MM-DD) is
recommended for use with the "date" attribute. If using a text entry in the
element content, the element may be repeated to support multiple language expressions.

\code{ddi_distrbtr()} is the organization designated by the author or producer to
generate copies of the particular work including any necessary editions or
revisions. Names and addresses may be specified and other archives may be
co-distributors. A URI attribute is included to provide an URN or URL to the
ordering service or download facility on a Web site.
}
\section{Shared and complex child nodes}{

\itemize{
\item \code{\link[=ddi_contact]{ddi_contact()}}
}
}

\examples{
ddi_distStmt()

# Functions that need to be wrapped in ddi_distStmt()

ddi_depDate(date = "2022-01-01", "January 1, 2022")

ddi_depositr(abbr = "BJS", 
             affiliation = "U.S. Department of Justice",
             "Bureau of Justice Statistics")

ddi_distDate(date = "2022-01-01", "January 1, 2022")

ddi_distrbtr(abbr = "ICPSR",
             affiliation = "Institute for Social Research",
             URI = "http://www.icpsr.umich.edu",
             "Ann Arbor, MI: Inter-university Consortium for Political and Social Research")

}
\references{
\href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/distStmt.html}{distStmt documentation}

\href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/depDate.html}{depDate documentation}

\href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/depositr.html}{depositr documentation}

\href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/distDate.html}{distDate documentation}

\href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/distrbtr.html}{distrbtr documentation}
}
