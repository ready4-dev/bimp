% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_cf_resc_tb}
\alias{make_cf_resc_tb}
\title{Make cf resource}
\usage{
make_cf_resc_tb(cf_data_ls, resources_tb, OOS_buffer_prop_dbl = 0.1)
}
\arguments{
\item{cf_data_ls}{Cf data (a list)}

\item{resources_tb}{Resources (a tibble)}

\item{OOS_buffer_prop_dbl}{Occasion of service buffer prop (a double vector), Default: 0.1}
}
\value{
Cf resource (a tibble)
}
\description{
make_cf_resc_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make cf resource tibble. The function returns Cf resource (a tibble).
}
\keyword{internal}
