% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/T_hss_table_single.R
\name{T_hss_table_single}
\alias{T_hss_table_single}
\title{Create contingency table for single-response question}
\usage{
T_hss_table_single(df, var, group, percent = TRUE, digits = 1)
}
\arguments{
\item{df}{A dataframe containing the variable of interest and grouping variable.}

\item{var}{A character string with the variable name of interest.}

\item{group}{A character string with the grouping (or disaggregation) variable.}

\item{percent}{Set to TRUE to show percentages. Set to FALSE to show counts.}

\item{digits}{The number of (significant) digits to display. Trailing zeroes are
always removed. Note that 'digits' does not mean 'decimals', so digits = 3 will display as 'mm.d' not 'mm.ddd'}
}
\value{
A contingency table with the variable of interest and grouping variable.
}
\description{
Create contingency tables for HSS questions with a single response option and
a cross-variable. Tables can be created with either counts or percentages and the number
of significant digits for percentages can be set. Tables created with this function
come with a couple of additions according to HSS preferences. Most notably: sums are
added over columns, and N-values are added to column headers.
}
\seealso{
\code{T_hss_table_multi}
}
