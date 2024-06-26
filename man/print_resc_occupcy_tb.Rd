% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_print.R
\name{print_resc_occupcy_tb}
\alias{print_resc_occupcy_tb}
\title{Print resource occupancy tibble}
\usage{
print_resc_occupcy_tb(
  resc_occupcy_tb,
  resources_tb,
  caption_1L_chr =
    "Resource occupancy and proportion of demand met (predicted from current input data)",
  mkdn_tbl_ref_1L_chr = "tab:metneed",
  output_type_1L_chr = "HTML",
  use_rdocx_1L_lgl = T
)
}
\arguments{
\item{resc_occupcy_tb}{Resource occupancy (a tibble)}

\item{resources_tb}{Resources (a tibble)}

\item{caption_1L_chr}{Caption (a character vector of length one), Default: 'Resource occupancy and proportion of demand met (predicted from current input data)'}

\item{mkdn_tbl_ref_1L_chr}{Markdown table reference (a character vector of length one), Default: 'tab:metneed'}

\item{output_type_1L_chr}{Output type (a character vector of length one), Default: 'HTML'}

\item{use_rdocx_1L_lgl}{Use rdocx (a logical vector of length one), Default: T}
}
\value{
No return value, called for side effects.
}
\description{
print_resc_occupcy_tb() is a Print function that prints output to console. Specifically, this function implements an algorithm to print resource occupancy tibble. The function is called for its side effects and does not return a value.
}
\keyword{internal}
