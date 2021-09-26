#' Bind resource tibbles
#' @description bind_resource_tbs() is a Bind function that binds two objects together to create a composite object. Specifically, this function implements an algorithm to bind resource tibbles. The function returns Joined (a tibble).
#' @param tfd_resc_use__tb Transformed resource use (a tibble)
#' @param resources_tb Resources (a tibble)
#' @param simple_outp_1L_lgl Simple output (a logical vector of length one), Default: T
#' @return Joined (a tibble)
#' @rdname bind_resource_tbs
#' @export 
#' @importFrom dplyr left_join select rename mutate
#' @keywords internal
bind_resource_tbs <- function (tfd_resc_use__tb, resources_tb, simple_outp_1L_lgl = T) 
{
    joined_tb <- tfd_resc_use__tb %>% dplyr::left_join(resources_tb %>% 
        dplyr::select(Resource_UID_chr, Role_Category_chr, Recipient_STE_chr, 
            Recipient_Sex_chr, Unit_Cost_Dollars_dbl, Unit_Qty_dbl)) %>% 
        dplyr::select(Role_Category_chr, Recipient_STE_chr, Recipient_Sex_chr, 
            OOS_serviced_demand_dbl, OOS_resource_occupancy_dbl, 
            Unit_Cost_Dollars_dbl, Unit_Qty_dbl) %>% dplyr::rename(Resource = Role_Category_chr, 
        Location = Recipient_STE_chr, Teams = Recipient_Sex_chr, 
        Service_Level_At_Current_WTE_dbl = OOS_serviced_demand_dbl)
    if (!simple_outp_1L_lgl) {
        joined_tb <- joined_tb %>% dplyr::rename(WTE_Required_For_Full_Provision_dbl = OOS_resource_occupancy_dbl) %>% 
            dplyr::mutate(Additional_Expenditure_dbl = Unit_Cost_Dollars_dbl * 
                (WTE_Required_For_Full_Provision_dbl - Unit_Qty_dbl))
    }
    joined_tb <- joined_tb %>% dplyr::select(-Unit_Cost_Dollars_dbl, 
        -Unit_Qty_dbl)
    return(joined_tb)
}
#' Bind tibbles in input list
#' @description bind_tbs_in_input_ls() is a Bind function that binds two objects together to create a composite object. Specifically, this function implements an algorithm to bind tibbles in input list. The function returns Combined (a list).
#' @param bc_ls Bc (a list)
#' @param cf_ls Counter-factual (a list)
#' @return Combined (a list)
#' @rdname bind_tbs_in_input_ls
#' @export 
#' @importFrom purrr map
#' @importFrom ready4fun add_lups
#' @importFrom stats setNames
#' @keywords internal
bind_tbs_in_input_ls <- function (bc_ls, cf_ls) 
{
    combined_ls <- 1:length(bc_ls) %>% purrr::map(~{
        if (nrow(cf_ls[[.x]]) == 0) {
            combined_tb <- bc_ls[[.x]]
        }
        else {
            combined_tb <- ready4fun::add_lups(bc_ls[[.x]], new_lup = cf_ls[[.x]], 
                key_var_nm_1L_chr = names(cf_ls[[.x]])[1], priority_lup_for_dupls_1L_chr = "new")
        }
        combined_tb
    }) %>% stats::setNames(names(bc_ls))
    return(combined_ls)
}
