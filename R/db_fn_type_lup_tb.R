#' Function type lookup table
#' 
#' A lookup table to find descriptions for different types of functions used within the bimp package suite.
#' 
#' A tibble
#' 
#' \describe{
#'   \item{fn_type_nm_chr}{Function type name (a character vector)}
#'   \item{fn_type_desc_chr}{Function type description (a character vector)}
#'   \item{first_arg_desc_chr}{First argument description (a character vector)}
#'   \item{second_arg_desc_chr}{Second argument description (a character vector)}
#'   \item{is_generic_lgl}{Is generic (a logical vector)}
#'   \item{is_method_lgl}{Is method (a logical vector)}
#' }
"fn_type_lup_tb"
