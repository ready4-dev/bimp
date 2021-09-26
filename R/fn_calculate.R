#' Calculate expenditure
#' @description calculate_expenditure() is a Calculate function that performs a numeric calculation. Specifically, this function implements an algorithm to calculate expenditure. The function returns Expenditure (a double vector of length one).
#' @param resources_tb Resources (a tibble)
#' @param incld_areas_chr Included areas (a character vector), Default: 'NA'
#' @param area_var_nm_1L_chr Area variable name (a character vector of length one), Default: 'Recipient_STE_chr'
#' @param n_rescs__var_nm_1L_chr N resources  variable name (a character vector of length one), Default: 'Unit_Qty_dbl'
#' @param unit_cost_var_nm_1L_chr Unit cost variable name (a character vector of length one), Default: 'Unit_Cost_Dollars_dbl'
#' @return Expenditure (a double vector of length one)
#' @rdname calculate_expenditure
#' @export 
#' @importFrom dplyr filter pull
#' @importFrom rlang sym
calculate_expenditure <- function (resources_tb, incld_areas_chr = NA_character_, area_var_nm_1L_chr = "Recipient_STE_chr", 
    n_rescs__var_nm_1L_chr = "Unit_Qty_dbl", unit_cost_var_nm_1L_chr = "Unit_Cost_Dollars_dbl") 
{
    if (!is.na(incld_areas_chr[1])) {
        resources_tb <- resources_tb %>% dplyr::filter(!!rlang::sym(area_var_nm_1L_chr) %in% 
            incld_areas_chr)
    }
    expenditure_dbl <- resources_tb %>% dplyr::pull(!!rlang::sym(n_rescs__var_nm_1L_chr)) * 
        resources_tb %>% dplyr::pull(!!rlang::sym(unit_cost_var_nm_1L_chr))
    expenditure_1L_dbl <- sum(expenditure_dbl)
    return(expenditure_1L_dbl)
}
