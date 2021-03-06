% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_get.R
\name{get_clone_targets}
\alias{get_clone_targets}
\title{Get clone targets}
\usage{
get_clone_targets(input_data_ls, Sex_1L_chr = "F", Target_1L_chr = "AUS_SNR_F")
}
\arguments{
\item{input_data_ls}{Input data (a list)}

\item{Sex_1L_chr}{Sex (a character vector of length one), Default: 'F'}

\item{Target_1L_chr}{Target (a character vector of length one), Default: 'AUS_SNR_F'}
}
\value{
Clone targets (a character vector)
}
\description{
get_clone_targets() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get clone targets. Function argument input_data_ls specifies the where to look for the required object. The function returns Clone targets (a character vector).
}
\keyword{internal}
