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
update_resc_occupcy_tb <- function(resc_occupcy_tb,
                                   resources_tb,
                                   simple_outp_1L_lgl = T){
  resc_occupcy_tb <- resc_occupcy_tb  %>%
    dplyr::select(Resource_UID_chr, OOS_resource_occupancy_dbl,	OOS_serviced_demand_dbl) %>%
    bind_resource_tbs(resources_tb = resources_tb,
                      simple_outp_1L_lgl = simple_outp_1L_lgl)
  return(resc_occupcy_tb)
}
update_resc_use_tb_for_gndr_diffs <- function(resource_use_tb,
                                              fem_uids_chr = "AUS_SNR_F",
                                              male_uids_chr = "AUS_SNR_M",
                                              original_fem_data_dbl = c(0.2,0.26666667),
                                              new_fem_data_dbl = c(0.398,0.306),
                                              original_male_data_dbl = c(0.2,0.21428571),
                                              new_male_data_dbl = c(0.181,0.139)){

  resource_use_tb <- resource_use_tb %>%
    dplyr::mutate(Proportion_Each_Timeframe_dbl = dplyr::case_when(Recipient_UID_chr %in% fem_uids_chr & round(Proportion_Each_Timeframe_dbl,get_nbr_of_decimals(original_fem_data_dbl[1])) == original_fem_data_dbl[1] ~ new_fem_data_dbl[1],
                                                                   Recipient_UID_chr %in% fem_uids_chr & round(Proportion_Each_Timeframe_dbl,get_nbr_of_decimals(original_fem_data_dbl[2])) == original_fem_data_dbl[2] ~ new_fem_data_dbl[2],
                                                                   Recipient_UID_chr %in% male_uids_chr & round(Proportion_Each_Timeframe_dbl,get_nbr_of_decimals(original_male_data_dbl[1])) == original_male_data_dbl[1] ~ new_male_data_dbl[1],
                                                                   Recipient_UID_chr %in% male_uids_chr & round(Proportion_Each_Timeframe_dbl,get_nbr_of_decimals(original_male_data_dbl[2])) == original_male_data_dbl[2] ~ new_male_data_dbl[2],

                                                                   T ~ Proportion_Each_Timeframe_dbl)) %>%
    dplyr::mutate(Proportion_Using_Service = paste0(round(Proportion_Each_Timeframe_dbl *100,2)))
  return(resource_use_tb)
}
