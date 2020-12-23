#' Update intv recps per Occasion of service
#' @description update_intv_recps_per_OOS() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update intv recps per occasion of service. Function argument main_calcs_tb specifies the object to be updated. The function is called for its side effects and does not return a value.
#' @param main_calcs_tb Main calcs (a tibble)
#' @return NULL
#' @rdname update_intv_recps_per_OOS
#' @export 
#' @importFrom dplyr mutate case_when
#' @keywords internal
update_intv_recps_per_OOS <- function (main_calcs_tb) 
{
    main_calcs_tb <- main_calcs_tb %>% dplyr::mutate(Recipients_Per_Episode_dbl = dplyr::case_when(Group_Delivery_lgl ~ 
        Count_dbl, T ~ 1))
}
#' Update main calcs with met dmd
#' @description update_main_calcs_with_met_dmd() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update main calcs with met dmd. Function argument input_data_ls specifies the object to be updated. The function returns Input data (a list).
#' @param input_data_ls Input data (a list)
#' @return Input data (a list)
#' @rdname update_main_calcs_with_met_dmd
#' @export 
#' @importFrom dplyr left_join
#' @keywords internal
update_main_calcs_with_met_dmd <- function (input_data_ls) 
{
    input_data_ls$main_calcs_tb <- dplyr::left_join(input_data_ls$main_calcs_tb, 
        input_data_ls$resc_occupcy_tb, by = "Resource_UID_chr")
    return(input_data_ls)
}
#' Update resource occupancy
#' @description update_resc_occupcy_tb() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update resource occupancy tibble. Function argument resc_occupcy_tb specifies the object to be updated. The function returns Resource occupancy (a tibble).
#' @param resc_occupcy_tb Resource occupancy (a tibble)
#' @return Resource occupancy (a tibble)
#' @rdname update_resc_occupcy_tb
#' @export 
#' @importFrom dplyr select mutate
#' @keywords internal
update_resc_occupcy_tb <- function (resc_occupcy_tb) 
{
    resc_occupcy_tb <- resc_occupcy_tb %>% dplyr::select(Resource_UID_chr, 
        OOS_resource_occupancy_dbl, OOS_serviced_demand_dbl) %>% 
        dplyr::mutate(Resource_Use = paste0(round(OOS_resource_occupancy_dbl * 
            100, 2), " %")) %>% dplyr::mutate(Demand_Met = paste0(round(OOS_serviced_demand_dbl * 
        100, 2), " %")) %>% dplyr::select(-OOS_resource_occupancy_dbl, 
        -OOS_serviced_demand_dbl) %>% bind_resource_tbs(resources_tb = nat_data_ls$resources_tb)
    return(resc_occupcy_tb)
}
