% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_transform.R
\name{transform_to_clone_nat_dmd}
\alias{transform_to_clone_nat_dmd}
\title{Transform to clone nat demand}
\usage{
transform_to_clone_nat_dmd(
  input_data_ls,
  clone_ls = list(AUS_SNR_F = get_clone_targets(input_data_ls, Sex_1L_chr = "F",
    Target_1L_chr = "AUS_SNR_F"), AUS_SNR_M = get_clone_targets(input_data_ls, Sex_1L_chr
    = "M", Target_1L_chr = "AUS_SNR_M")),
  incld_intvs_cats_chr = NA_character_,
  incld_intvs_chr = NA_character_,
  intv_uid_var_nm_1L_chr = "Intervention_UID_chr",
  intv_cat_uid_var_nm_1L_chr = "Intervention_Cat_UID_chr"
)
}
\arguments{
\item{input_data_ls}{Input data (a list)}

\item{clone_ls}{Clone (a list), Default: list(AUS_SNR_F = get_clone_targets(input_data_ls, Sex_1L_chr = "F",
Target_1L_chr = "AUS_SNR_F"), AUS_SNR_M = get_clone_targets(input_data_ls,
Sex_1L_chr = "M", Target_1L_chr = "AUS_SNR_M"))}

\item{incld_intvs_cats_chr}{Included interventions cats (a character vector), Default: 'NA'}

\item{incld_intvs_chr}{Included interventions (a character vector), Default: 'NA'}

\item{intv_uid_var_nm_1L_chr}{Intervention unique identifier variable name (a character vector of length one), Default: 'Intervention_UID_chr'}

\item{intv_cat_uid_var_nm_1L_chr}{Intervention cat unique identifier variable name (a character vector of length one), Default: 'Intervention_Cat_UID_chr'}
}
\value{
Input data (a list)
}
\description{
transform_to_clone_nat_dmd() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform to clone nat demand. The function returns Input data (a list).
}
\keyword{internal}
