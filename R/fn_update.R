#' Update intv recps per OOS
#' @description update_intv_recps_per_OOS() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update intv recps per oos. Function argument main_calcs_tb specifies the object to be updated. The function is called for its side effects and does not return a value.
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
