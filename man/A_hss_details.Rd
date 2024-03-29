% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/A_hss_details.R
\name{A_hss_details}
\alias{A_hss_details}
\title{Produce a duration assessment for individual enumerators}
\usage{
A_hss_details(dat, audit, name, max)
}
\arguments{
\item{dat}{dataset that is prepared for the audit procedure}

\item{audit}{audit files (media export)}

\item{name}{the actual name of the enumerator in question}

\item{max}{a numerical value that is used to exclude lengthy question nodes}
}
\value{
two lists containing a detailed (duration) overview of clean surveys
}
\description{
This function generates two lists that provide clarity on: (1) The screen-time
duration + instance_ids of all audited surveys, and (2) the screen-time
duration + instance_ids of valid surveys.
}
