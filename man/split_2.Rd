% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-split.R
\name{split_2}
\alias{split_2}
\title{Split select_multiple survey variables}
\usage{
split_2(data, dict)
}
\description{
Split select_multiple variables (1 2 7) into their individual components (1), (2), (7),
with additional mutations allowing for proper labeling of variables, inclusion of NAs and
no response variables, and prep-work for the reordering of split variables to match
the XLS-form standard.
}
