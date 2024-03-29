% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/survey.R
\name{mdl_survey_options}
\alias{mdl_survey_options}
\title{Set survey options}
\usage{
mdl_survey_options(
  survey_idno,
  enum_survey_access_policy = NULL,
  data_remote_url = NULL,
  published = NULL,
  enum_collection = NULL,
  linked_collections = NULL,
  tags = NULL,
  aliases = NULL,
  link_study = NULL,
  link_indicator = NULL
)
}
\arguments{
\item{survey_idno}{Dataset IDNo.}

\item{enum_survey_access_policy}{Access policy. It is recommended to use mdl_enum_survey_access_policy.}

\item{data_remote_url}{Url to data in case access policiy was set to Data available from external repository ("remote")}

\item{published}{Set Dataset publish status. 0=draft, 1=published.}

\item{enum_collection}{Main collection of the dataset. It is recommended to use mdl_enum_collection.}

\item{linked_collections}{Array containing other secondary collections in which the dataset has to be shown.}

\item{tags}{Array of string tags.}

\item{aliases}{Array of strings with dataset aliases}

\item{link_study}{URL for study website}

\item{link_indicator}{URL to the indicators website}
}
\value{
API call response.
}
\description{
Can set various survey options with a PUT call.
If you want to set linked/secondary collections, it is recommended to use mdl_survey_attach_to_collections().
}
