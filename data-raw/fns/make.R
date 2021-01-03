make_cf_resc_tb <- function(cf_data_ls,
                            resources_tb,
                            OOS_buffer_prop_dbl = 0.1){
  cf_data_curr_ls <- cf_data_ls %>%
    transform_inp_ls_for_analysis(OOS_buffer_prop_dbl = OOS_buffer_prop_dbl)
  cf_data_one_wte_ls <- cf_data_ls
  cf_data_one_wte_ls$resources_tb$Unit_Qty_dbl <- 1
  cf_data_one_wte_ls <- cf_data_one_wte_ls %>%
    transform_inp_ls_for_analysis(OOS_buffer_prop_dbl = OOS_buffer_prop_dbl)
  cf_resc_tb <- dplyr::inner_join(cf_data_curr_ls$resc_occupcy_tb %>%
                                    dplyr::select(Resource_UID_chr, OOS_serviced_demand_dbl),
                                  cf_data_one_wte_ls$resc_occupcy_tb %>%
                                    dplyr::select(Resource_UID_chr, OOS_resource_occupancy_dbl)) %>%
    bind_resource_tbs(resources_tb = resources_tb,
                      simple_outp_1L_lgl = F)
  return(cf_resc_tb)
}
make_expenditure_summ_tb <- function(input_data_ls,
                                     incld_areas_ls,
                                     area_var_nm_1L_chr = "Recipient_STE_chr",
                                     n_ress_var_nm_1L_chr = "Unit_Qty_dbl",
                                     unit_cost_var_nm_1L_chr = "Unit_Cost_Dollars_dbl"){
  purrr::map_dfr(incld_areas_ls,
                 ~{
                   if(.x[1]=="ALL"){
                     incld_areas_chr <- NA_character_
                   }else{
                     incld_areas_chr <- .x
                   }
                   tibble::tibble(State_chr = paste0(.x,collapse="_"),
                                  Expenditure_dbl = calculate_expenditure(input_data_ls$resources_tb,
                                                                          incld_areas_chr = incld_areas_chr,
                                                                          area_var_nm_1L_chr = area_var_nm_1L_chr,
                                                                          n_ress_var_nm_1L_chr = n_ress_var_nm_1L_chr,
                                                                          unit_cost_var_nm_1L_chr = unit_cost_var_nm_1L_chr))
                 })
}
make_resource_use_df <- function(input_data_ls){
  resource_use_df <- input_data_ls$resource_use_tb %>%
    dplyr::mutate(Intervention = purrr::map_chr(Intervention_UID_chr,~ready4fun::get_from_lup_obj(input_data_ls$interventions_tb,
                                                                                                  match_value_xx = .x,
                                                                                                  match_var_nm_1L_chr = "Intervention_UID_chr",
                                                                                                  target_var_nm_1L_chr = "Intervention_Name_chr",
                                                                                                  evaluate_lgl = F)[1])) %>%
    dplyr::mutate(Recipient = purrr::map_chr(Recipient_UID_chr,~ready4fun::get_from_lup_obj(input_data_ls$recipients_tb,
                                                                                            match_value_xx = .x,
                                                                                            match_var_nm_1L_chr = "Recipient_UID_chr",
                                                                                            target_var_nm_1L_chr = "Team_chr",
                                                                                            evaluate_lgl = F)[1])) %>%
    dplyr::mutate(Proportion_Using_Service = paste0(round(Proportion_Each_Timeframe_dbl *100,2)
                                                    #, " %"
    ) ) %>%
    dplyr::mutate(Resource = purrr::map_chr(Resource_UID_chr,~ready4fun::get_from_lup_obj(input_data_ls$resources_tb,
                                                                                          match_value_xx = .x,
                                                                                          match_var_nm_1L_chr = "Resource_UID_chr",
                                                                                          target_var_nm_1L_chr = "Role_Category_chr",
                                                                                          evaluate_lgl = F)[1]))
  return(resource_use_df)
}
