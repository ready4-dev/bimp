#' Transform input data for included interventions
#' @description transform_inp_data_for_incld_intvs() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform input data for included interventions. Function argument input_data_ls specifies the object to be updated. Argument incld_intvs_cats_chr provides the object to be updated. The function returns Input data (a list).
#' @param input_data_ls Input data (a list)
#' @param incld_intvs_cats_chr Included interventions cats (a character vector), Default: 'NA'
#' @param incld_intvs_chr Included interventions (a character vector), Default: 'NA'
#' @param intv_uid_var_nm_1L_chr Intervention unique identifier variable name (a character vector of length one), Default: 'Intervention_UID_chr'
#' @param intv_cat_uid_var_nm_1L_chr Intervention cat unique identifier variable name (a character vector of length one), Default: 'Intervention_Cat_UID_chr'
#' @return Input data (a list)
#' @rdname transform_inp_data_for_incld_intvs
#' @export 
#' @importFrom dplyr filter pull bind_rows distinct
#' @importFrom rlang sym
#' @importFrom purrr map
#' @keywords internal
transform_inp_data_for_incld_intvs <- function (input_data_ls, incld_intvs_cats_chr = NA_character_, 
    incld_intvs_chr = NA_character_, intv_uid_var_nm_1L_chr = "Intervention_UID_chr", 
    intv_cat_uid_var_nm_1L_chr = "Intervention_Cat_UID_chr") 
{
    if (!is.na(incld_intvs_chr[1])) {
        filter_1_intvs_tb <- input_data_ls$interventions_tb %>% 
            dplyr::filter(!!rlang::sym(intv_uid_var_nm_1L_chr) %in% 
                incld_intvs_chr)
    }
    else {
        filter_1_intvs_tb <- input_data_ls$interventions_tb
    }
    if (!is.na(incld_intvs_cats_chr[1])) {
        if (!is.na(incld_intvs_chr[1])) {
            incld_intvs_cats_chr <- c(incld_intvs_cats_chr, filter_1_intvs_tb %>% 
                dplyr::pull(!!rlang::sym(intv_uid_var_nm_1L_chr))) %>% 
                unique()
        }
        filter_2_intvs_tb <- input_data_ls$interventions_tb %>% 
            dplyr::filter(!!rlang::sym(intv_cat_uid_var_nm_1L_chr) %in% 
                incld_intvs_cats_chr)
        if (!is.na(incld_intvs_chr[1])) {
            filtered_intvs_tb <- dplyr::bind_rows(filter_2_intvs_tb, 
                filter_1_intvs_tb) %>% dplyr::distinct()
        }
        else {
            filtered_intvs_tb <- filter_2_intvs_tb
        }
    }
    else {
        filtered_intvs_tb <- filter_1_intvs_tb
    }
    input_data_ls$interventions_tb <- filtered_intvs_tb
    incld_intvs_chr <- input_data_ls$interventions_tb %>% dplyr::pull(!!rlang::sym(intv_uid_var_nm_1L_chr))
    input_data_ls <- input_data_ls %>% purrr::map(~{
        if (intv_uid_var_nm_1L_chr %in% names(.x)) {
            .x <- .x %>% dplyr::filter(!!rlang::sym(intv_uid_var_nm_1L_chr) %in% 
                incld_intvs_chr)
        }
        .x
    })
    return(input_data_ls)
}
#' Transform input data for resources  calculations
#' @description transform_inp_data_for_rescs__calcs() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform input data for resources  calculations. Function argument input_data_ls specifies the object to be updated. The function returns Input data (a list).
#' @param input_data_ls Input data (a list)
#' @return Input data (a list)
#' @rdname transform_inp_data_for_rescs__calcs
#' @export 

#' @keywords internal
transform_inp_data_for_rescs__calcs <- function (input_data_ls) 
{
    input_data_ls$resources_tb <- input_data_ls$resources_tb %>% 
        add_eftv_wkly_hrs() %>% add_meets_non_OOS_wkly_hrs_test() %>% 
        add_max_wkly_OOS_hrs()
    return(input_data_ls)
}
#' Transform input list for analysis
#' @description transform_inp_ls_for_analysis() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform input list for analysis. Function argument input_data_ls specifies the object to be updated. Argument OOS_buffer_prop_dbl provides the object to be updated. The function returns Transformed input data (a list).
#' @param input_data_ls Input data (a list)
#' @param OOS_buffer_prop_dbl Occasion of service buffer prop (a double vector), Default: 0.1
#' @return Transformed input data (a list)
#' @rdname transform_inp_ls_for_analysis
#' @export 
#' @importFrom dplyr filter
#' @keywords internal
transform_inp_ls_for_analysis <- function (input_data_ls, OOS_buffer_prop_dbl = 0.1) 
{
    input_data_ls$resource_use_tb <- input_data_ls$resource_use_tb %>% 
        dplyr::filter(Proportion_Each_Timeframe_dbl > 0)
    tfd_input_data_ls <- input_data_ls %>% transform_inp_data_for_rescs__calcs() %>% 
        add_main_calcs_tb(OOS_buffer_prop_dbl = OOS_buffer_prop_dbl) %>% 
        add_resc_occupcy_tb() %>% update_main_calcs_with_met_dmd()
    return(tfd_input_data_ls)
}
#' Transform resource occupancy tibble
#' @description transform_resc_occupcy_tb() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform resource occupancy tibble. Function argument resc_occupcy_tb specifies the object to be updated. Argument resources_tb provides the object to be updated. The function returns Resource occupancy (a tibble).
#' @param resc_occupcy_tb Resource occupancy (a tibble)
#' @param resources_tb Resources (a tibble)
#' @return Resource occupancy (a tibble)
#' @rdname transform_resc_occupcy_tb
#' @export 
#' @importFrom dplyr select mutate
#' @importFrom purrr map_chr
#' @importFrom stringr str_replace
#' @keywords internal
transform_resc_occupcy_tb <- function (resc_occupcy_tb, resources_tb) 
{
    resc_occupcy_tb <- resc_occupcy_tb %>% dplyr::select(Resource_UID_chr, 
        OOS_resource_occupancy_dbl, OOS_serviced_demand_dbl) %>% 
        dplyr::mutate(Resource_Use = paste0(round(OOS_resource_occupancy_dbl * 
            100, 2), " %") %>% purrr::map_chr(~stringr::str_replace(.x, 
            "Inf %", ""))) %>% dplyr::mutate(Demand_Met = paste0(round(OOS_serviced_demand_dbl * 
        100, 2), " %")) %>% dplyr::select(-OOS_resource_occupancy_dbl, 
        -OOS_serviced_demand_dbl) %>% bind_resource_tbs(resources_tb = resources_tb)
    return(resc_occupcy_tb)
}
#' Transform to clone nat demand
#' @description transform_to_clone_nat_dmd() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform to clone nat demand. Function argument input_data_ls specifies the object to be updated. Argument clone_ls provides the object to be updated. The function returns Input data (a list).
#' @param input_data_ls Input data (a list)
#' @param clone_ls Clone (a list), Default: list(AUS_SNR_F = get_clone_targets(input_data_ls, Sex_1L_chr = "F", 
#'    Target_1L_chr = "AUS_SNR_F"), AUS_SNR_M = get_clone_targets(input_data_ls, 
#'    Sex_1L_chr = "M", Target_1L_chr = "AUS_SNR_M"))
#' @param incld_intvs_cats_chr Included interventions cats (a character vector), Default: 'NA'
#' @param incld_intvs_chr Included interventions (a character vector), Default: 'NA'
#' @param intv_uid_var_nm_1L_chr Intervention unique identifier variable name (a character vector of length one), Default: 'Intervention_UID_chr'
#' @param intv_cat_uid_var_nm_1L_chr Intervention cat unique identifier variable name (a character vector of length one), Default: 'Intervention_Cat_UID_chr'
#' @return Input data (a list)
#' @rdname transform_to_clone_nat_dmd
#' @export 
#' @importFrom purrr map2_dfr map_dfr
#' @importFrom dplyr filter mutate bind_rows select distinct
#' @importFrom ready4fun get_from_lup_obj
#' @keywords internal
transform_to_clone_nat_dmd <- function (input_data_ls, clone_ls = list(AUS_SNR_F = get_clone_targets(input_data_ls, 
    Sex_1L_chr = "F", Target_1L_chr = "AUS_SNR_F"), AUS_SNR_M = get_clone_targets(input_data_ls, 
    Sex_1L_chr = "M", Target_1L_chr = "AUS_SNR_M")), incld_intvs_cats_chr = NA_character_, 
    incld_intvs_chr = NA_character_, intv_uid_var_nm_1L_chr = "Intervention_UID_chr", 
    intv_cat_uid_var_nm_1L_chr = "Intervention_Cat_UID_chr") 
{
    alt_inp_data_ls <- transform_inp_data_for_incld_intvs(input_data_ls, 
        incld_intvs_cats_chr = incld_intvs_cats_chr, incld_intvs_chr = incld_intvs_chr, 
        intv_uid_var_nm_1L_chr = intv_uid_var_nm_1L_chr, intv_cat_uid_var_nm_1L_chr = intv_cat_uid_var_nm_1L_chr)
    addl_resc_tb <- purrr::map2_dfr(clone_ls, names(clone_ls), 
        ~{
            template_1L_chr <- .y
            template_tb <- alt_inp_data_ls$resource_use_tb %>% 
                dplyr::filter(Recipient_UID_chr == .y) %>% dplyr::mutate(Discipline_UID_chr = ready4fun::get_from_lup_obj(alt_inp_data_ls$resources_tb, 
                match_var_nm_1L_chr = "Resource_UID_chr", match_value_xx = Resource_UID_chr, 
                target_var_nm_1L_chr = "Discipline_UID_chr", 
                evaluate_lgl = F))
            recipients_chr <- .x
            new_tb <- purrr::map_dfr(recipients_chr, ~{
                state_1L_chr <- ready4fun::get_from_lup_obj(alt_inp_data_ls$recipients_tb, 
                  match_var_nm_1L_chr = "Recipient_UID_chr", 
                  match_value_xx = .x, target_var_nm_1L_chr = "Location_UID_chr", 
                  evaluate_lgl = F)
                template_tb %>% dplyr::mutate(Recipient_UID_chr = .x) %>% 
                  dplyr::mutate(Resource_UID_chr = ready4fun::get_from_lup_obj(alt_inp_data_ls$locations_tb, 
                    match_var_nm_1L_chr = "Location_UID_chr", 
                    match_value_xx = state_1L_chr, target_var_nm_1L_chr = "STE_chr", 
                    evaluate_lgl = F) %>% ready4fun::get_from_lup_obj(data_lookup_tb = alt_inp_data_ls$resources_tb %>% 
                    dplyr::filter(Discipline_UID_chr == template_tb$Discipline_UID_chr, 
                      Recipient_Sex_chr == ready4fun::get_from_lup_obj(alt_inp_data_ls$recipients_tb, 
                        match_var_nm_1L_chr = "Recipient_UID_chr", 
                        match_value_xx = template_1L_chr, target_var_nm_1L_chr = "Sex_chr", 
                        evaluate_lgl = F)), match_var_nm_1L_chr = "Recipient_STE_chr", 
                    match_value_xx = ., target_var_nm_1L_chr = "Resource_UID_chr", 
                    evaluate_lgl = F))
            })
            new_tb
        })
    input_data_ls$resource_use_tb <- dplyr::bind_rows(input_data_ls$resource_use_tb, 
        addl_resc_tb %>% dplyr::select(-Discipline_UID_chr)) %>% 
        dplyr::distinct()
    return(input_data_ls)
}
