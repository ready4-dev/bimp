#' Make expenditure summ
#' @description make_expenditure_summ_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make expenditure summ tibble. The function is called for its side effects and does not return a value.
#' @param input_data_ls Input data (a list)
#' @param incld_areas_ls Incld areas (a list)
#' @param area_var_nm_1L_chr Area var name (a character vector of length one), Default: 'Recipient_STE_chr'
#' @param n_ress_var_nm_1L_chr N ress var name (a character vector of length one), Default: 'Unit_Qty_dbl'
#' @param unit_cost_var_nm_1L_chr Unit cost var name (a character vector of length one), Default: 'Unit_Cost_Dollars_dbl'
#' @return NULL
#' @rdname make_expenditure_summ_tb
#' @export 
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
#' @keywords internal
make_expenditure_summ_tb <- function (input_data_ls, incld_areas_ls, area_var_nm_1L_chr = "Recipient_STE_chr", 
    n_ress_var_nm_1L_chr = "Unit_Qty_dbl", unit_cost_var_nm_1L_chr = "Unit_Cost_Dollars_dbl") 
{
    purrr::map_dfr(incld_areas_ls, ~{
        if (.x[1] == "ALL") {
            incld_areas_chr <- NA_character_
        }
        else {
            incld_areas_chr <- .x
        }
        tibble::tibble(State_chr = paste0(.x, collapse = "_"), 
            Expenditure_dbl = calculate_expenditure(input_data_ls$resources_tb, 
                incld_areas_chr = incld_areas_chr, area_var_nm_1L_chr = area_var_nm_1L_chr, 
                n_ress_var_nm_1L_chr = n_ress_var_nm_1L_chr, 
                unit_cost_var_nm_1L_chr = unit_cost_var_nm_1L_chr))
    })
}
