% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_update.R
\name{update_resc_use_tb_for_gndr_diffs}
\alias{update_resc_use_tb_for_gndr_diffs}
\title{Update resource use tibble for gender differences}
\usage{
update_resc_use_tb_for_gndr_diffs(
  resource_use_tb,
  fem_uids_chr = "AUS_SNR_F",
  male_uids_chr = "AUS_SNR_M",
  original_fem_data_dbl = c(0.2, 0.26666667),
  new_fem_data_dbl = c(0.398, 0.306),
  original_male_data_dbl = c(0.2, 0.21428571),
  new_male_data_dbl = c(0.181, 0.139)
)
}
\arguments{
\item{resource_use_tb}{Resource use (a tibble)}

\item{fem_uids_chr}{Female unique identifiers (a character vector), Default: 'AUS_SNR_F'}

\item{male_uids_chr}{Male unique identifiers (a character vector), Default: 'AUS_SNR_M'}

\item{original_fem_data_dbl}{Original female data (a double vector), Default: c(0.2, 0.26666667)}

\item{new_fem_data_dbl}{New female data (a double vector), Default: c(0.398, 0.306)}

\item{original_male_data_dbl}{Original male data (a double vector), Default: c(0.2, 0.21428571)}

\item{new_male_data_dbl}{New male data (a double vector), Default: c(0.181, 0.139)}
}
\value{
Resource use (a tibble)
}
\description{
update_resc_use_tb_for_gndr_diffs() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update resource use tibble for gender differences. The function returns Resource use (a tibble).
}
\keyword{internal}
