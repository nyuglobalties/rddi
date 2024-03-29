% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/method.R
\name{ddi_dataColl}
\alias{ddi_dataColl}
\alias{ddi_actMin}
\alias{ddi_cleanOps}
\alias{ddi_collectorTraining}
\alias{ddi_collMode}
\alias{ddi_collSitu}
\alias{ddi_ConOps}
\alias{ddi_dataCollector}
\alias{ddi_deviat}
\alias{ddi_frequenc}
\alias{ddi_instrumentDevelopment}
\alias{ddi_resInstru}
\alias{ddi_sampProc}
\alias{ddi_timeMeth}
\alias{ddi_weight}
\title{dataColl and its children}
\usage{
ddi_dataColl(...)

ddi_actMin(...)

ddi_cleanOps(...)

ddi_collectorTraining(...)

ddi_collMode(...)

ddi_collSitu(...)

ddi_ConOps(...)

ddi_dataCollector(...)

ddi_deviat(...)

ddi_frequenc(...)

ddi_instrumentDevelopment(...)

ddi_resInstru(...)

ddi_sampProc(...)

ddi_timeMeth(...)

ddi_weight(...)
}
\arguments{
\item{...}{Child nodes or attributes.}
}
\value{
A ddi_node object.
}
\description{
Information about the data collection methodology employed in the codebook.
More information on these elements, especially their allowed attributes, can
be found in the references.
}
\details{
\emph{Parent nodes}

\code{dataColl} is contained in \code{method}.

\emph{dataColl specific child nodes}
\itemize{
\item \code{ddi_actMin()}is the summary of actions taken to minimize data loss.
Includes information on actions such as follow-up visits, supervisory
checks, historical matching, estimation, etc.
\item \code{ddi_cleanOps()} are the methods used to "clean" the data collection,
e.g., consistency checking, wild code checking, etc. The "agency" attribute
permits specification of the agency doing the data cleaning.
\item \code{ddi_collectorTraining()} describes the training provided to data
collectors including interviewer training, process testing, compliance with
standards etc. This is repeatable for language and to capture different
aspects of the training process. The type attribute allows specification of
the type of training being described.
\item \code{ddi_collMode()} is the method used to collect the data; instrumentation
characteristics.
\item \code{ddi_collSitu()} is the description of noteworthy aspects of the data
collection situation. Includes information on factors such as
cooperativeness of respondents, duration of interviews, number of
call-backs, etc.
\item \code{ddi_ConOps()} are control operations. These are methods to facilitate
data control performed by the primary investigator or by the data archive.
Specify any special programs used for such operations. The "agency"
attribute maybe used to refer to the agency that performed the control
operation.
\item \code{ddi_dataCollector()} is the entity (individual, agency, or institution)
responsible for administering the questionnaire or interview or compiling
the data. This refers to the entity collecting the data, not to the entity
producing the documentation.
\item \code{ddi_deviat()} are major deviations from the sample design. This is
information indicating correspondence as well as discrepancies between the
sampled units (obtained) and available statistics for the population (age,
sex-ratio, marital status, etc.) as a whole.
\item \code{ddi_frequenc()} is the frequency of data collection. It's for data
collected at more than one point in time.
\item \code{ddi_instrumentDevelopment()} describes any development work on the data
collection instrument.
\item \code{ddi_resInstru()} is the type of data collection instrument used.
\item \code{ddi_sampProc()} is the type of sample and sample design used to select
the survey respondents to represent the population. May include reference
to the target sample size and the sampling fraction.
\item \code{ddi_weight()} defines the weights used to produce accurate statistical
results within the sampling procedures. Describe here the criteria for
using weights in analysis of a collection. If a weighting formula or
coefficient was developed, provide this formula, define its elements, and
indicate how the formula is applied to data.
}
}
\section{Shared and complex child nodes}{

\itemize{
\item \code{\link[=ddi_sampleFrame]{ddi_sampleFrame()}}
\item \code{\link[=ddi_sources]{ddi_sources()}}
\item \code{\link[=ddi_targetSampleSize]{ddi_targetSampleSize()}}
}
}

\examples{
ddi_dataColl()

# Functions that need to be wrapped in ddi_dataColl()

ddi_actMin("To minimize the number of unresolved cases and reduce the 
           potential nonresponse bias, four follow-up contacts were made with 
           agencies that had not responded by various stages of the data 
           collection process.")
           
ddi_cleanOps("Checks for undocumented codes were performed, and data were 
             subsequently revised in consultation with the principal investigator.")
             
ddi_collectorTraining(type = "interviewer training",
                      "Describe research project, describe population and 
                      sample, suggest methods and language for approaching 
                      subjects, explain questions and key terms of survey instrument.")
                      
ddi_collMode("telephone interviews")

ddi_collSitu("There were 1,194 respondents who answered questions in face-to-face 
             interviews lasting approximately 75 minutes each.")
             
ddi_ConOps(agency = "ICPSR",
           "Ten percent of data entry forms were reentered to check for accuracy.")
           
ddi_dataCollector(abbr = "SRC",
                  affiliation = "University of Michigan",
                  role = "questionnaire administration",
                  "Survey Research Center")
                  
ddi_deviat("The suitability of Ohio as a research site reflected its similarity 
           to the United States as a whole. The evidence extended by Tuchfarber 
           (1988) shows that Ohio is representative of the United States in 
           several ways: percent urban and rural, percent of the population 
           that is African American, median age, per capita income, percent 
           living below the poverty level, and unemployment rate. Although 
           results generated from an Ohio sample are not empirically 
           generalizable to the United States, they may be suggestive of what 
           might be expected nationally.")
           
ddi_frequenc("monthly")

ddi_instrumentDevelopment(type = "pretesting",
                         "The questionnaire was pre-tested with split-panel 
                         tests, as well as an analysis of non-response rates 
                         for individual items, and response distributions.")
                         
ddi_resInstru("structured")

ddi_sampProc("National multistage area probability sample")

ddi_weight("The 1996 NES dataset includes two final person-level analysis weights 
           which incorporate sampling, nonresponse, and post-stratification 
           factors. One weight (variable #4) is for longitudinal micro-level 
           analysis using the 1996 NES Panel. The other weight (variable #3) 
           is for analysis of the 1996 NES combined sample (Panel component 
           cases plus Cross-section supplement cases). In addition, a Time 
           Series Weight (variable #5) which corrects for Panel attrition was 
           constructed. This weight should be used in analyses which compare 
           the 1996 NES to earlier unweighted National Election Study data 
           collections.")

}
\references{
\href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataColl.html}{dataColl documentation}

\href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/actMin.html}{actMin documentation}

\href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/cleanOps.html}{cleanOps documentation}

\href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/collectorTraining.html}{collectorTraining documentation}

\href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/collMode.html}{collMode documentation}

\href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/collSitu.html}{collSitu documentation}

\href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/ConOps.html}{ConOps documentation}

\href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/dataCollector.html}{dataCollector documentation}

\href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/deviat.html}{deviat documentation}

\href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/frequenc.html}{frequenc documentation}

\href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/instrumentDevelopment.html}{instrumentDevelopment documentation}

\href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/resInstru.html}{resInstru documentation}

\href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/sampProc.html}{sampProc documentation}

\href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/timeMeth.html}{timeMeth documentation}

\href{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/weight.html}{weight documentation}
}
