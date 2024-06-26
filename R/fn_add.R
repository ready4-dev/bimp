#' Add effective weekly hours
#' @description add_eftv_wkly_hrs() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add effective weekly hours. The function returns Resources (a tibble).
#' @param resources_tb Resources (a tibble)
#' @return Resources (a tibble)
#' @rdname add_eftv_wkly_hrs
#' @export 
#' @importFrom dplyr mutate
#' @keywords internal
add_eftv_wkly_hrs <- function (resources_tb) 
{
    mean_anl_wkg_days_1L_dbl <- 365.25/7 * 5
    resources_tb <- resources_tb %>% dplyr::mutate(eftv_wkly_hrs_dbl = (mean_anl_wkg_days_1L_dbl - 
        (Annual_Leave_Days_dbl + Annual_Public_Holidays_Days_dbl))/(mean_anl_wkg_days_1L_dbl) * 
        Weekly_Hours_Per_Unit_dbl * Unit_Qty_dbl)
    return(resources_tb)
}
#' Add main calculations tibble
#' @description add_main_calcs_tb() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add main calculations tibble. The function returns Input data (a list).
#' @param input_data_ls Input data (a list)
#' @param OOS_buffer_prop_dbl Occasion of service buffer prop (a double vector), Default: 0.1
#' @return Input data (a list)
#' @rdname add_main_calcs_tb
#' @export 
#' @importFrom dplyr left_join mutate filter
#' @keywords internal
add_main_calcs_tb <- function (input_data_ls, OOS_buffer_prop_dbl = 0.1) 
{
    input_data_ls$main_calcs_tb <- input_data_ls$resource_use_tb %>% 
        dplyr::left_join(input_data_ls$recipients_tb, by = "Recipient_UID_chr") %>% 
        dplyr::mutate(Indications_dbl = Proportion_Each_Timeframe_dbl * 
            Count_dbl) %>% dplyr::left_join(input_data_ls$interventions_tb, 
        by = "Intervention_UID_chr") %>% dplyr::left_join(input_data_ls$resources_tb, 
        by = "Resource_UID_chr") %>% update_intv_rcps_per_OOS() %>% 
        add_n_epsds_per_yr() %>% add_n_OOS_per_yr() %>% add_OOS_mins_per_yr(OOS_buffer_prop_dbl = OOS_buffer_prop_dbl) %>% 
        dplyr::filter(!is.na(Resource_UID_chr))
    return(input_data_ls)
}
#' Add maximum weekly occasion of service hours
#' @description add_max_wkly_OOS_hrs() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add maximum weekly occasion of service hours. The function returns Resources (a tibble).
#' @param resources_tb Resources (a tibble)
#' @return Resources (a tibble)
#' @rdname add_max_wkly_OOS_hrs
#' @export 
#' @importFrom dplyr mutate
#' @importFrom purrr map2_dbl
#' @keywords internal
add_max_wkly_OOS_hrs <- function (resources_tb) 
{
    resources_tb <- resources_tb %>% dplyr::mutate(max_wkly_OOS_hrs = purrr::map2_dbl(eftv_wkly_hrs_dbl, 
        Non_OOS_Weekly_Hours_Per_Unit_dbl, ~max(.x - .y, 0)))
    return(resources_tb)
}
#' Add meets non occasion of service weekly hours test
#' @description add_meets_non_OOS_wkly_hrs_test() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add meets non occasion of service weekly hours test. The function returns Resources (a tibble).
#' @param resources_tb Resources (a tibble)
#' @return Resources (a tibble)
#' @rdname add_meets_non_OOS_wkly_hrs_test
#' @export 
#' @importFrom dplyr mutate
#' @keywords internal
add_meets_non_OOS_wkly_hrs_test <- function (resources_tb) 
{
    resources_tb <- resources_tb %>% dplyr::mutate(meets_non_OOS_wkly_hrs_lgl = (eftv_wkly_hrs_dbl >= 
        Non_OOS_Weekly_Hours_Per_Unit_dbl))
    return(resources_tb)
}
#' Add n episodes per year
#' @description add_n_epsds_per_yr() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add n episodes per year. The function returns Main calculations (a tibble).
#' @param main_calcs_tb Main calculations (a tibble)
#' @return Main calculations (a tibble)
#' @rdname add_n_epsds_per_yr
#' @export 
#' @importFrom dplyr mutate
#' @keywords internal
add_n_epsds_per_yr <- function (main_calcs_tb) 
{
    main_calcs_tb <- main_calcs_tb %>% dplyr::mutate(n_epsds_per_yr = Indications_dbl/Recipients_Per_Episode_dbl * 
        ifelse(Timeframe_In_Weeks_chr == "Yearly", 1, ifelse(Timeframe_In_Weeks_chr == 
            "Quarterly", 4, ifelse(Timeframe_In_Weeks_chr == 
            "Monthly", 12, ifelse(Timeframe_In_Weeks_chr == "Fortnightly", 
            365.25/7/2, ifelse(Timeframe_In_Weeks_chr == "Weekly", 
                365.25/7, ifelse(Timeframe_In_Weeks_chr == "Daily", 
                  365.25, NA_real_)))))))
    return(main_calcs_tb)
}
#' Add n occasion of service per year
#' @description add_n_OOS_per_yr() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add n occasion of service per year. The function returns Main calculations (a tibble).
#' @param main_calcs_tb Main calculations (a tibble)
#' @return Main calculations (a tibble)
#' @rdname add_n_OOS_per_yr
#' @export 
#' @importFrom dplyr mutate
#' @keywords internal
add_n_OOS_per_yr <- function (main_calcs_tb) 
{
    main_calcs_tb <- main_calcs_tb %>% dplyr::mutate(n_OOS_per_yr_dbl = N_OOS_Per_Episode_dbl * 
        n_epsds_per_yr)
    return(main_calcs_tb)
}
#' Add occasion of service minimums per year
#' @description add_OOS_mins_per_yr() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add occasion of service minimums per year. The function returns Main calculations (a tibble).
#' @param main_calcs_tb Main calculations (a tibble)
#' @param OOS_buffer_prop_dbl Occasion of service buffer prop (a double vector), Default: 0.1
#' @return Main calculations (a tibble)
#' @rdname add_OOS_mins_per_yr
#' @export 
#' @importFrom dplyr mutate
#' @keywords internal
add_OOS_mins_per_yr <- function (main_calcs_tb, OOS_buffer_prop_dbl = 0.1) 
{
    main_calcs_tb <- main_calcs_tb %>% dplyr::mutate(OOS_mins_per_yr_dbl = n_OOS_per_yr_dbl * 
        Duration_OOS_Mins_dbl * (1 + OOS_buffer_prop_dbl))
    return(main_calcs_tb)
}
#' Add resource occupancy tibble
#' @description add_resc_occupcy_tb() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add resource occupancy tibble. The function returns Input data (a list).
#' @param input_data_ls Input data (a list)
#' @return Input data (a list)
#' @rdname add_resc_occupcy_tb
#' @export 
#' @importFrom dplyr group_by summarise ungroup mutate
#' @importFrom purrr map2_dbl
#' @keywords internal
add_resc_occupcy_tb <- function (input_data_ls) 
{
    input_data_ls$resc_occupcy_tb <- input_data_ls[["main_calcs_tb"]] %>% 
        dplyr::group_by(Resource_UID_chr) %>% dplyr::summarise(annual_mins_OOS_demand_dbl = sum(OOS_mins_per_yr_dbl), 
        annual_OOS_capacity_dbl = max_wkly_OOS_hrs[1] * 60 * 
            365.25/7) %>% dplyr::ungroup() %>% dplyr::mutate(OOS_resource_occupancy_dbl = annual_mins_OOS_demand_dbl/annual_OOS_capacity_dbl, 
        OOS_serviced_demand_dbl = purrr::map2_dbl(annual_OOS_capacity_dbl, 
            annual_mins_OOS_demand_dbl, ~min(1, .x/.y)))
    return(input_data_ls)
}
#' Add suffices to variable names
#' @description add_sfcs_to_var_nms() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add suffices to variable names. The function is called for its side effects and does not return a value.
#' @param var_nms_chr Variable names (a character vector)
#' @param data_tb Data (a tibble)
#' @return No return value, called for side effects.
#' @rdname add_sfcs_to_var_nms
#' @export 
#' @importFrom purrr map_chr
#' @keywords internal
add_sfcs_to_var_nms <- function (var_nms_chr, data_tb) 
{
    purrr::map_chr(var_nms_chr, ~add_sfx_to_var_nm(.x, data_tb = data_tb))
}
#' Add suffix to variable name
#' @description add_sfx_to_var_nm() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add suffix to variable name. The function is called for its side effects and does not return a value.
#' @param var_nm_1L_chr Variable name (a character vector of length one)
#' @param data_tb Data (a tibble)
#' @return No return value, called for side effects.
#' @rdname add_sfx_to_var_nm
#' @export 
#' @importFrom dplyr pull
#' @importFrom rlang sym
#' @keywords internal
add_sfx_to_var_nm <- function (var_nm_1L_chr, data_tb) 
{
    vec_xx <- data_tb %>% dplyr::pull(!!rlang::sym(var_nm_1L_chr))
    sfx <- ifelse(is.character(vec_xx), "_chr", ifelse(is.numeric(vec_xx), 
        "_dbl", "_lgl"))
    new_nm_1L_chr <- paste0(var_nm_1L_chr, sfx)
    new_nm_1L_chr
}
