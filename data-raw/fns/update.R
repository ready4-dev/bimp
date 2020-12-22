update_intv_recps_per_OOS <- function(main_calcs_tb){
  main_calcs_tb <- main_calcs_tb %>%
    dplyr::mutate(Recipients_Per_Episode_dbl = dplyr::case_when(Group_Delivery_lgl ~ Count_dbl,
                                                                T ~ 1))
}
update_main_calcs_with_met_dmd <- function(input_data_ls){
  input_data_ls$main_calcs_tb <- dplyr::left_join(input_data_ls$main_calcs_tb,
                                                  input_data_ls$resc_occupcy_tb,
                                                  by = "Resource_UID_chr")
  return(input_data_ls)
}
