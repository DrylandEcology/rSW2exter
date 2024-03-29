% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_soils_NRCS_SDA.R
\name{fetch_soils_from_NRCS_SDA}
\alias{fetch_soils_from_NRCS_SDA}
\title{Download soil data from \var{NRCS} \var{SDA} web service}
\usage{
fetch_soils_from_NRCS_SDA(
  mukeys_unique,
  sql_template = NA,
  majcompflag = c("subset", "ignore"),
  only_soilcomp = TRUE,
  chunk_size = 1000L,
  progress_bar = FALSE
)
}
\arguments{
\item{mukeys_unique}{An integer vector with unique \var{mukey} values.}

\item{sql_template}{A character vector.
A valid \var{T-SQL} query with a \var{WHERE} clause so that the code can
inject chunks of \code{mukeys_unique} values,
i.e., \var{"mapunit.mukey IN (\%s)"}.
If \code{NA}, then the default query is loaded, see examples.}

\item{majcompflag}{A character string. \var{"subset"} keeps
the WHERE clause \var{component.majcompflag = 'Yes'} that is contained in
\code{sql_template}; \var{"ignore"} removes it from the query. Note that
the field \var{"majcompflag} exists only in the \var{SSURGO} version
of the \var{component} table, but not in the \var{STATSGO} version.}

\item{only_soilcomp}{A logical value. If \code{TRUE}, then query restricts
to soil components. If \code{FALSE}, then query includes
all components including "Miscellaneous areas" and \var{"NOTCOM"}
(not complete) components.}

\item{chunk_size}{An integer value. The size of chunks into which
\code{mukeys_unique} is broken up and looped over for processing.}

\item{progress_bar}{A logical value. Display a progress bar as the code
loops over the chunks?}
}
\value{
A \var{data.frame} according to the specifications of \code{sql} or
  \code{NULL} if the query returns empty.
}
\description{
Download soil data from \var{NRCS} \var{SDA} web service
}
\section{Notes}{
 A live internet connection is required to access \var{SDA}.

 This is a function with minimal functionality;
use \code{\link{extract_soils_NRCS_SDA}} for a user-friendly interface.
}

\examples{
\dontrun{
if (curl::has_internet()) {
  fetch_soils_from_NRCS_SDA(mukeys_unique = 67616)

  # As of 2022-March-15, mukey 2479921 contained one "NOTCOM" component
  fetch_soils_from_NRCS_SDA(mukeys_unique = 2479921)
  fetch_soils_from_NRCS_SDA(mukeys_unique = 2479921, only_soilcomp = FALSE)

  sql <- readLines(
    system.file("NRCS", "nrcs_sql_template.sql", package = "rSW2exter")
  )

  fetch_soils_from_NRCS_SDA(mukeys_unique = 67616, sql_template = sql)

  # This will return NULL because -1 is not an existing mukey value
  fetch_soils_from_NRCS_SDA(mukeys_unique = -1, sql_template = sql)
}
}

}
\seealso{
\code{\link[soilDB]{SDA_query}}
}
