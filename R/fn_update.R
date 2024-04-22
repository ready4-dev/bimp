#' Update intervention recipients per occasion of service
#' @description update_intv_rcps_per_OOS() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update intervention recipients per occasion of service. The function is called for its side effects and does not return a value.
#' @param main_calcs_tb Main calculations (a tibble)
#' @return No return value, called for side effects.
#' @rdname update_intv_rcps_per_OOS
#' @export 
#' @importFrom dplyr mutate case_when
#' @keywords internal
update_intv_rcps_per_OOS <- function (main_calcs_tb) 
{
    main_calcs_tb <- main_calcs_tb %>% dplyr::mutate(Recipients_Per_Episode_dbl = dplyr::case_when(Group_Delivery_lgl ~ 
        Count_dbl, T ~ 1))
}
#' Update main calculations with met demand
#' @description update_main_calcs_with_met_dmd() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update main calculations with met demand. The function returns Input data (a list).
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
#' Update resource occupancy tibble
#' @description update_resc_occupcy_tb() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update resource occupancy tibble. The function returns Resource occupancy (a tibble).
#' @param resc_occupcy_tb Resource occupancy (a tibble)
#' @param resources_tb Resources (a tibble)
#' @param simple_outp_1L_lgl Simple output (a logical vector of length one), Default: T
#' @return Resource occupancy (a tibble)
#' @rdname update_resc_occupcy_tb
#' @export 
#' @importFrom dplyr select
#' @keywords internal
update_resc_occupcy_tb <- function (resc_occupcy_tb, resources_tb, simple_outp_1L_lgl = T) 
{
    resc_occupcy_tb <- resc_occupcy_tb %>% dplyr::select(Resource_UID_chr, 
        OOS_resource_occupancy_dbl, OOS_serviced_demand_dbl) %>% 
        bind_resource_tbs(resources_tb = resources_tb, simple_outp_1L_lgl = simple_outp_1L_lgl)
    return(resc_occupcy_tb)
}
#' Update resource use tibble for gender differences
#' @description update_resc_use_tb_for_gndr_diffs() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update resource use tibble for gender differences. The function returns Resource use (a tibble).
#' @param resource_use_tb Resource use (a tibble)
#' @param fem_uids_chr Female unique identifiers (a character vector), Default: 'AUS_SNR_F'
#' @param male_uids_chr Male unique identifiers (a character vector), Default: 'AUS_SNR_M'
#' @param original_fem_data_dbl Original female data (a double vector), Default: c(0.2, 0.26666667)
#' @param new_fem_data_dbl New female data (a double vector), Default: c(0.398, 0.306)
#' @param original_male_data_dbl Original male data (a double vector), Default: c(0.2, 0.21428571)
#' @param new_male_data_dbl New male data (a double vector), Default: c(0.181, 0.139)
#' @return Resource use (a tibble)
#' @rdname update_resc_use_tb_for_gndr_diffs
#' @export 
#' @importFrom dplyr mutate case_when
#' @keywords internal
update_resc_use_tb_for_gndr_diffs <- function (resource_use_tb, fem_uids_chr = "AUS_SNR_F", male_uids_chr = "AUS_SNR_M", 
    original_fem_data_dbl = c(0.2, 0.26666667), new_fem_data_dbl = c(0.398, 
        0.306), original_male_data_dbl = c(0.2, 0.21428571), 
    new_male_data_dbl = c(0.181, 0.139)) 
{
    resource_use_tb <- resource_use_tb %>% dplyr::mutate(Proportion_Each_Timeframe_dbl = dplyr::case_when(Recipient_UID_chr %in% 
        fem_uids_chr & round(Proportion_Each_Timeframe_dbl, get_nbr_of_decimals(original_fem_data_dbl[1])) == 
        original_fem_data_dbl[1] ~ new_fem_data_dbl[1], Recipient_UID_chr %in% 
        fem_uids_chr & round(Proportion_Each_Timeframe_dbl, get_nbr_of_decimals(original_fem_data_dbl[2])) == 
        original_fem_data_dbl[2] ~ new_fem_data_dbl[2], Recipient_UID_chr %in% 
        male_uids_chr & round(Proportion_Each_Timeframe_dbl, 
        get_nbr_of_decimals(original_male_data_dbl[1])) == original_male_data_dbl[1] ~ 
        new_male_data_dbl[1], Recipient_UID_chr %in% male_uids_chr & 
        round(Proportion_Each_Timeframe_dbl, get_nbr_of_decimals(original_male_data_dbl[2])) == 
            original_male_data_dbl[2] ~ new_male_data_dbl[2], 
        T ~ Proportion_Each_Timeframe_dbl)) %>% dplyr::mutate(Proportion_Using_Service = paste0(round(Proportion_Each_Timeframe_dbl * 
        100, 2)))
    return(resource_use_tb)
}
