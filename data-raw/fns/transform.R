transform_inp_data_for_incld_intvs <- function(input_data_ls,
                                               incld_intvs_cats_chr = NA_character_,
                                               incld_intvs_chr = NA_character_,
                                               intv_uid_var_nm_1L_chr = "Intervention_UID_chr",
                                               intv_cat_uid_var_nm_1L_chr = "Intervention_Cat_UID_chr"){
  if(!is.na(incld_intvs_chr[1])){
    filter_1_intvs_tb <- input_data_ls$interventions_tb %>%
      dplyr::filter(!!rlang::sym(intv_uid_var_nm_1L_chr) %in% incld_intvs_chr)
  }else{
    filter_1_intvs_tb <- input_data_ls$interventions_tb
  }
  if(!is.na(incld_intvs_cats_chr[1])){
    if(!is.na(incld_intvs_chr[1])){
      incld_intvs_cats_chr <- c(incld_intvs_cats_chr,
                                filter_1_intvs_tb %>% dplyr::pull(!!rlang::sym(intv_uid_var_nm_1L_chr))) %>% unique()
    }
    filter_2_intvs_tb  <- input_data_ls$interventions_tb %>%
      dplyr::filter(!!rlang::sym(intv_cat_uid_var_nm_1L_chr) %in% incld_intvs_cats_chr)
    if(!is.na(incld_intvs_chr[1])){
      filtered_intvs_tb <- dplyr::bind_rows(filter_2_intvs_tb,
                                            filter_1_intvs_tb) %>% dplyr::distinct()
    }else{
      filtered_intvs_tb <- filter_2_intvs_tb
    }
  }else{
    filtered_intvs_tb <- filter_1_intvs_tb
  }
  input_data_ls$interventions_tb <- filtered_intvs_tb
  incld_intvs_chr <- input_data_ls$interventions_tb %>% dplyr::pull(!!rlang::sym(intv_uid_var_nm_1L_chr))
  input_data_ls <- input_data_ls %>%
    purrr::map(~ {
      if(intv_uid_var_nm_1L_chr %in% names(.x)){

        .x <- .x %>% dplyr::filter(!!rlang::sym(intv_uid_var_nm_1L_chr) %in% incld_intvs_chr)
      }
      .x
    })
  return(input_data_ls)
}
transform_inp_data_for_ress_calcs <- function(input_data_ls){
  input_data_ls$resources_tb <- input_data_ls$resources_tb %>%
    add_eftv_wkly_hrs() %>%
    add_meets_non_OOS_wkly_hrs_test() %>%
    add_max_wkly_OOS_hrs()
  return(input_data_ls)
}
transform_inp_ls_for_analysis <- function(input_data_ls,
                                          OOS_buffer_prop_dbl = 0.1){
  tfd_input_data_ls <- input_data_ls %>%
    transform_inp_data_for_ress_calcs() %>%
    add_main_calcs_tb(OOS_buffer_prop_dbl = OOS_buffer_prop_dbl) %>%
    add_resc_occupcy_tb() %>%
    update_main_calcs_with_met_dmd()
  return(tfd_input_data_ls)
}
transform_to_clone_nat_dmd <- function(input_data_ls,
                                       clone_ls = list(AUS_SNR_F = get_clone_targets(input_data_ls,
                                                                                     Sex_1L_chr = "F",
                                                                                     Target_1L_chr = "AUS_SNR_F"),
                                                       AUS_SNR_M = get_clone_targets(input_data_ls,
                                                                                     Sex_1L_chr = "M",
                                                                                     Target_1L_chr = "AUS_SNR_M")),
                                       incld_intvs_cats_chr = NA_character_,
                                       incld_intvs_chr = NA_character_,#c("PWM_K10_CHK","PWM_K10_CRD","PWM_EDU_1"),
                                       intv_uid_var_nm_1L_chr = "Intervention_UID_chr",
                                       intv_cat_uid_var_nm_1L_chr = "Intervention_Cat_UID_chr"){
  alt_inp_data_ls <- transform_inp_data_for_incld_intvs(input_data_ls,
                                                        incld_intvs_cats_chr = incld_intvs_cats_chr,
                                                        incld_intvs_chr = incld_intvs_chr,
                                                        intv_uid_var_nm_1L_chr = intv_uid_var_nm_1L_chr,
                                                        intv_cat_uid_var_nm_1L_chr = intv_cat_uid_var_nm_1L_chr)
  addl_res_tb <- purrr::map2_dfr(clone_ls,
                                 names(clone_ls),
                                 ~ {
                                   template_1L_chr <- .y
                                   template_tb <- alt_inp_data_ls$resource_use_tb %>%
                                     dplyr::filter(Recipient_UID_chr == .y) %>%
                                     dplyr::mutate(Discipline_UID_chr = ready4fun::get_from_lup_obj(alt_inp_data_ls$resources_tb,
                                                                                                    match_var_nm_1L_chr = "Resource_UID_chr",
                                                                                                    match_value_xx = Resource_UID_chr,
                                                                                                    target_var_nm_1L_chr = "Discipline_UID_chr",
                                                                                                    evaluate_lgl = F))
                                   recipients_chr <- .x
                                   new_tb <- purrr::map_dfr(recipients_chr,
                                                            ~ {
                                                              state_1L_chr <- ready4fun::get_from_lup_obj(alt_inp_data_ls$recipients_tb,
                                                                                                          match_var_nm_1L_chr = "Recipient_UID_chr",
                                                                                                          match_value_xx = .x,
                                                                                                          target_var_nm_1L_chr = "Location_UID_chr",
                                                                                                          evaluate_lgl = F)
                                                              template_tb %>%
                                                                dplyr::mutate(Recipient_UID_chr = .x) %>%
                                                                dplyr::mutate(Resource_UID_chr = ready4fun::get_from_lup_obj(alt_inp_data_ls$locations_tb,
                                                                                                                             match_var_nm_1L_chr = "Location_UID_chr",
                                                                                                                             match_value_xx = state_1L_chr,
                                                                                                                             target_var_nm_1L_chr = "STE_chr",
                                                                                                                             evaluate_lgl = F) %>%
                                                                                ready4fun::get_from_lup_obj(data_lookup_tb = alt_inp_data_ls$resources_tb %>% dplyr::filter(Discipline_UID_chr == template_tb$Discipline_UID_chr,
                                                                                                                                                                            Recipient_Sex_chr == ready4fun::get_from_lup_obj(alt_inp_data_ls$recipients_tb,
                                                                                                                                                                                                                             match_var_nm_1L_chr = "Recipient_UID_chr",
                                                                                                                                                                                                                             match_value_xx = template_1L_chr,
                                                                                                                                                                                                                             target_var_nm_1L_chr = "Sex_chr",
                                                                                                                                                                                                                             evaluate_lgl = F)),
                                                                                                            match_var_nm_1L_chr = "Recipient_STE_chr",
                                                                                                            match_value_xx = .,
                                                                                                            target_var_nm_1L_chr = "Resource_UID_chr",
                                                                                                            evaluate_lgl = F))

                                                            })
                                   new_tb
                                 })
  input_data_ls$resource_use_tb <- dplyr::bind_rows(input_data_ls$resource_use_tb,
                                                    addl_res_tb %>% dplyr::select(-Discipline_UID_chr)) %>%
    dplyr::distinct()
  return(input_data_ls)
}
