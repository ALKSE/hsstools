% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/C_hss_recode_na.R
\name{C_hss_recode_na}
\alias{C_hss_recode_na}
\title{Recode values to NA}
\usage{
C_hss_recode_na(dat)
}
\arguments{
\item{dat}{The dataframe with HSS survey data for which you want to recode}
}
\value{
Returns a dataframe with recoded values
}
\description{
In the exported HSS datafile values are coded as 0  even when that particular
question was skipped for a respondent. In those cases NA values are expected.
This function recodes 0 values to NA where appropriate. Note that currently
the variables which are recoded are hard-coded into the function
}
