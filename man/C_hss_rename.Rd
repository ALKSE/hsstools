% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/C_hss_rename.R
\name{C_hss_rename}
\alias{C_hss_rename}
\title{Rename HSS Survey variables}
\usage{
C_hss_rename(dat, dict)
}
\arguments{
\item{dat}{the dataframe containing the HSS survey}

\item{xlsform}{File path to the XLS form for this particular HSS survey.}
}
\value{
Returns the dataframe with updated column names
}
\description{
This function is used to rename the variables in an HSS dataset from the 'Q01'
format to more human-readable variable names. This is done based on the XLS form
for that particular survey round. The XLS form needs to have 'r_name' columns
in both the question & response option sheets for this to work.
}
