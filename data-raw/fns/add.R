add_eftv_wkly_hrs <- function(resources_tb){
  mean_anl_wkg_days_1L_dbl <- 365.25/ 7 *5
  resources_tb <- resources_tb %>%
    dplyr::mutate(eftv_wkly_hrs_dbl = (mean_anl_wkg_days_1L_dbl -(Annual_Leave_Days_dbl + Annual_Public_Holidays_Days_dbl))/(mean_anl_wkg_days_1L_dbl) * Weekly_Hours_Per_Unit_dbl * Unit_Qty_dbl)
  return(resources_tb)
}
add_main_calcs_tb <- function(input_data_ls,
                              OOS_buffer_prop_dbl = 0.1){
  input_data_ls$main_calcs_tb <- input_data_ls$resource_use_tb %>%
    dplyr::left_join(input_data_ls$recipients_tb, by = "Recipient_UID_chr") %>%
    dplyr::mutate(Indications_dbl = Proportion_Each_Timeframe_dbl * Count_dbl)  %>%
    dplyr::left_join(input_data_ls$interventions_tb, by = "Intervention_UID_chr") %>%
    dplyr::left_join(input_data_ls$resources_tb, by = "Resource_UID_chr") %>%
    update_intv_recps_per_OOS() %>%
    add_n_epsds_per_yr() %>%
    add_n_OOS_per_yr() %>%
    add_OOS_mins_per_yr(OOS_buffer_prop_dbl = OOS_buffer_prop_dbl) %>%
    dplyr::filter(!is.na(Resource_UID_chr))
  return(input_data_ls)
}
add_max_wkly_OOS_hrs <- function(resources_tb){
  # testit::assert(resources_tb$Unit_Qty_dbl==0|resources_tb$eftv_wkly_hrs_dbl>= resources_tb$Non_OOS_Weekly_Hours_Per_Unit_dbl)
  resources_tb <- resources_tb %>%
    dplyr::mutate(max_wkly_OOS_hrs = purrr::map2_dbl(eftv_wkly_hrs_dbl,Non_OOS_Weekly_Hours_Per_Unit_dbl,~max(.x - .y,0)))
  return(resources_tb)
}
add_meets_non_OOS_wkly_hrs_test <- function(resources_tb){
  resources_tb <- resources_tb %>%
    dplyr::mutate(meets_non_OOS_wkly_hrs_lgl = (eftv_wkly_hrs_dbl >= Non_OOS_Weekly_Hours_Per_Unit_dbl))
  return(resources_tb)

}
add_n_epsds_per_yr <- function(main_calcs_tb){
  main_calcs_tb <- main_calcs_tb %>%
    dplyr::mutate(n_epsds_per_yr = Indications_dbl / Recipients_Per_Episode_dbl * ifelse(Timeframe_In_Weeks_chr == "Yearly",
                                                                                         1,
                                                                                         ifelse(Timeframe_In_Weeks_chr == "Quarterly",
                                                                                                4,
                                                                                                ifelse(Timeframe_In_Weeks_chr == "Monthly",
                                                                                                       12,
                                                                                                       ifelse(Timeframe_In_Weeks_chr == "Fortnightly",
                                                                                                              365.25/7/2,
                                                                                                              ifelse(Timeframe_In_Weeks_chr == "Weekly",
                                                                                                                     365.25/7,
                                                                                                                     ifelse(Timeframe_In_Weeks_chr == "Daily",
                                                                                                                            365.25,
                                                                                                                            NA_real_)))))
                                                                                         ))
  return(main_calcs_tb)
}
add_n_OOS_per_yr <- function(main_calcs_tb){
  main_calcs_tb <- main_calcs_tb %>%
    dplyr::mutate(n_OOS_per_yr_dbl = N_OOS_Per_Episode_dbl * n_epsds_per_yr)
  return(main_calcs_tb)
}
add_OOS_mins_per_yr <- function(main_calcs_tb,
                                OOS_buffer_prop_dbl = 0.1){
  main_calcs_tb <- main_calcs_tb %>%
    dplyr::mutate(OOS_mins_per_yr_dbl = n_OOS_per_yr_dbl * Duration_OOS_Mins_dbl * (1+OOS_buffer_prop_dbl))
  return(main_calcs_tb)
}
add_resc_occupcy_tb <- function(input_data_ls){
  input_data_ls$resc_occupcy_tb <- input_data_ls[["main_calcs_tb"]] %>%
    dplyr::group_by(Resource_UID_chr) %>%
    dplyr::summarise(annual_mins_OOS_demand_dbl = sum(OOS_mins_per_yr_dbl),
                     annual_OOS_capacity_dbl = max_wkly_OOS_hrs[1]*60*365.25/7) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(OOS_resource_occupancy_dbl = annual_mins_OOS_demand_dbl/annual_OOS_capacity_dbl,
                  OOS_serviced_demand_dbl = purrr::map2_dbl(annual_OOS_capacity_dbl,annual_mins_OOS_demand_dbl, ~ min(1,.x/.y)))
  return(input_data_ls)
}
add_sfcs_to_var_nms <- function(var_nms_chr,
                                data_tb){
  purrr::map_chr(var_nms_chr,
                 ~ add_sfx_to_var_nm(.x,
                                     data_tb = data_tb))
}
add_sfx_to_var_nm <- function(var_nm_1L_chr,
                              data_tb){
  vec_xx <- data_tb %>% dplyr::pull(!!rlang::sym(var_nm_1L_chr))
  sfx <- ifelse(is.character(vec_xx),
                "_chr",
                ifelse(is.numeric(vec_xx),
                       "_dbl",
                       "_lgl"))
  new_nm_1L_chr <- paste0(var_nm_1L_chr,sfx)
  new_nm_1L_chr
}
