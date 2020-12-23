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
update_resc_occupcy_tb <- function(resc_occupcy_tb){
  resc_occupcy_tb <- resc_occupcy_tb  %>%
    dplyr::select(Resource_UID_chr, OOS_resource_occupancy_dbl,	OOS_serviced_demand_dbl) %>%
    dplyr::mutate(Resource_Use = paste0(round(OOS_resource_occupancy_dbl * 100,2), " %")) %>%
    dplyr::mutate(Demand_Met = paste0(round(OOS_serviced_demand_dbl * 100,2), " %")) %>%
    dplyr::select(-OOS_resource_occupancy_dbl,	-OOS_serviced_demand_dbl) %>%
    bind_resource_tbs(resources_tb = nat_data_ls$resources_tb)
  return(resc_occupcy_tb)
}
