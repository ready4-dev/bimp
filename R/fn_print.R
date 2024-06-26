#' Print expenditure summary tibble
#' @description print_expdr_smry_tb() is a Print function that prints output to console. Specifically, this function implements an algorithm to print expenditure summary tibble. The function is called for its side effects and does not return a value.
#' @param expdr_smry_tb Expenditure summary (a tibble)
#' @param caption_1L_chr Caption (a character vector of length one), Default: 'Predicted Expenditure'
#' @param mkdn_tbl_ref_1L_chr Markdown table reference (a character vector of length one), Default: 'tab:exptst'
#' @param output_type_1L_chr Output type (a character vector of length one), Default: 'HTML'
#' @param use_rdocx_1L_lgl Use rdocx (a logical vector of length one), Default: T
#' @return No return value, called for side effects.
#' @rdname print_expdr_smry_tb
#' @export 
#' @importFrom dplyr mutate select
#' @importFrom ready4show print_table
#' @keywords internal
print_expdr_smry_tb <- function (expdr_smry_tb, caption_1L_chr = "Predicted Expenditure", 
    mkdn_tbl_ref_1L_chr = "tab:exptst", output_type_1L_chr = "HTML", 
    use_rdocx_1L_lgl = T) 
{
    expdr_smry_tb %>% dplyr::mutate(Annual_Cost = format(Expenditure_dbl, 
        big.mark = " ")) %>% dplyr::select(-Expenditure_dbl) %>% 
        remove_col_nms_obj_sfcs(complete_cases_1L_lgl = T) %>% 
        ready4show::print_table(output_type_1L_chr = output_type_1L_chr, 
            use_rdocx_1L_lgl = use_rdocx_1L_lgl, caption_1L_chr = "Predicted Expenditure", 
            mkdn_tbl_ref_1L_chr = "tab:exptst", footnotes_chr = "", 
            big_mark_1L_chr = " ")
}
#' Print interventions dataframe
#' @description print_intvs_df() is a Print function that prints output to console. Specifically, this function implements an algorithm to print interventions dataframe. The function is called for its side effects and does not return a value.
#' @param intvs_df Interventions (a data.frame)
#' @param caption_1L_chr Caption (a character vector of length one), Default: 'Included mental health interventions'
#' @param mkdn_tbl_ref_1L_chr Markdown table reference (a character vector of length one), Default: 'tab:intvs'
#' @param output_type_1L_chr Output type (a character vector of length one), Default: 'HTML'
#' @param use_rdocx_1L_lgl Use rdocx (a logical vector of length one), Default: T
#' @return No return value, called for side effects.
#' @rdname print_intvs_df
#' @export 
#' @importFrom dplyr select rename mutate case_when
#' @importFrom ready4show print_table
#' @keywords internal
print_intvs_df <- function (intvs_df, caption_1L_chr = "Included mental health interventions", 
    mkdn_tbl_ref_1L_chr = "tab:intvs", output_type_1L_chr = "HTML", 
    use_rdocx_1L_lgl = T) 
{
    intvs_df <- intvs_df %>% dplyr::select(c(names(intvs_df)[1:10], 
        names(intvs_df)[12], names(intvs_df)[11], names(intvs_df)[13:14]))
    intvs_df %>% dplyr::select(-Intervention_UID_chr, -Domain_UID_chr, 
        -Intervention_Cat_UID_chr, -Discipline_UID_chr, -Role_Cat_UID_chr, 
        -Location_chr) %>% dplyr::rename(Quantity = N_OOS_Per_Episode_dbl, 
        `Duration (mins)` = Duration_OOS_Mins_dbl, Timeframe = Timeframe_In_Weeks_chr, 
        Activity = Intervention_Name_chr, Format = Intervention_Format_chr) %>% 
        dplyr::select(-Domain_chr, -Intervention_Category_chr) %>% 
        dplyr::mutate(Format = dplyr::case_when(Format == "F2F" ~ 
            "Face to face", Format == "Provider only" ~ "Desk", 
            T ~ Format)) %>% remove_col_nms_obj_sfcs(complete_cases_1L_lgl = T) %>% 
        ready4show::print_table(output_type_1L_chr = output_type_1L_chr, 
            use_rdocx_1L_lgl = use_rdocx_1L_lgl, caption_1L_chr = caption_1L_chr, 
            mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr, footnotes_chr = "")
}
#' Print recipients tibble
#' @description print_rcps_tb() is a Print function that prints output to console. Specifically, this function implements an algorithm to print recipients tibble. The function is called for its side effects and does not return a value.
#' @param recipients_tb Recipients (a tibble)
#' @param caption_1L_chr Caption (a character vector of length one), Default: 'Team groups'
#' @param mkdn_tbl_ref_1L_chr Markdown table reference (a character vector of length one), Default: 'tab:pgps'
#' @param output_type_1L_chr Output type (a character vector of length one), Default: 'HTML'
#' @param use_rdocx_1L_lgl Use rdocx (a logical vector of length one), Default: T
#' @return No return value, called for side effects.
#' @rdname print_rcps_tb
#' @export 
#' @importFrom dplyr select
#' @importFrom ready4show print_table
#' @keywords internal
print_rcps_tb <- function (recipients_tb, caption_1L_chr = "Team groups", mkdn_tbl_ref_1L_chr = "tab:pgps", 
    output_type_1L_chr = "HTML", use_rdocx_1L_lgl = T) 
{
    recipients_tb %>% dplyr::select(-Recipient_UID_chr, -Recipient_Type_chr, 
        -Location_UID_chr, -Age_Band_chr, -Note_chr) %>% remove_col_nms_obj_sfcs(complete_cases_1L_lgl = T) %>% 
        ready4show::print_table(output_type_1L_chr = output_type_1L_chr, 
            use_rdocx_1L_lgl = use_rdocx_1L_lgl, caption_1L_chr = caption_1L_chr, 
            mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr, footnotes_chr = "")
}
#' Print resource occupancy tibble
#' @description print_resc_occupcy_tb() is a Print function that prints output to console. Specifically, this function implements an algorithm to print resource occupancy tibble. The function is called for its side effects and does not return a value.
#' @param resc_occupcy_tb Resource occupancy (a tibble)
#' @param resources_tb Resources (a tibble)
#' @param caption_1L_chr Caption (a character vector of length one), Default: 'Resource occupancy and proportion of demand met (predicted from current input data)'
#' @param mkdn_tbl_ref_1L_chr Markdown table reference (a character vector of length one), Default: 'tab:metneed'
#' @param output_type_1L_chr Output type (a character vector of length one), Default: 'HTML'
#' @param use_rdocx_1L_lgl Use rdocx (a logical vector of length one), Default: T
#' @return No return value, called for side effects.
#' @rdname print_resc_occupcy_tb
#' @export 
#' @importFrom ready4show print_table
#' @keywords internal
print_resc_occupcy_tb <- function (resc_occupcy_tb, resources_tb, caption_1L_chr = "Resource occupancy and proportion of demand met (predicted from current input data)", 
    mkdn_tbl_ref_1L_chr = "tab:metneed", output_type_1L_chr = "HTML", 
    use_rdocx_1L_lgl = T) 
{
    resc_occupcy_tb %>% update_resc_occupcy_tb(resources_tb = resources_tb) %>% 
        format_bound_resc_tb() %>% remove_col_nms_obj_sfcs(complete_cases_1L_lgl = T) %>% 
        ready4show::print_table(output_type_1L_chr = output_type_1L_chr, 
            use_rdocx_1L_lgl = use_rdocx_1L_lgl, caption_1L_chr = "Resource occupancy and proportion of demand met (predicted from current input data)", 
            mkdn_tbl_ref_1L_chr = "tab:metneed")
}
#' Print resource use dataframe
#' @description print_resource_use_df() is a Print function that prints output to console. Specifically, this function implements an algorithm to print resource use dataframe. The function is called for its side effects and does not return a value.
#' @param resource_use_df Resource use (a data.frame)
#' @param caption_1L_chr Caption (a character vector of length one), Default: 'Resource use'
#' @param mkdn_tbl_ref_1L_chr Markdown table reference (a character vector of length one), Default: 'tab:resuse'
#' @param output_type_1L_chr Output type (a character vector of length one), Default: 'HTML'
#' @param use_rdocx_1L_lgl Use rdocx (a logical vector of length one), Default: T
#' @return No return value, called for side effects.
#' @rdname print_resource_use_df
#' @export 
#' @importFrom dplyr select rename
#' @importFrom ready4show print_table
#' @keywords internal
print_resource_use_df <- function (resource_use_df, caption_1L_chr = "Resource use", mkdn_tbl_ref_1L_chr = "tab:resuse", 
    output_type_1L_chr = "HTML", use_rdocx_1L_lgl = T) 
{
    resource_use_df %>% dplyr::select(-Intervention_UID_chr, 
        -Recipient_UID_chr, -Proportion_Each_Timeframe_dbl, -Resource_UID_chr) %>% 
        dplyr::rename(Activity = Intervention, `Team Group` = Recipient, 
            `Usage (percent)` = Proportion_Using_Service) %>% 
        remove_col_nms_obj_sfcs(complete_cases_1L_lgl = T) %>% 
        ready4show::print_table(output_type_1L_chr = output_type_1L_chr, 
            use_rdocx_1L_lgl = use_rdocx_1L_lgl, caption_1L_chr = "Resource use", 
            mkdn_tbl_ref_1L_chr = "tab:resuse", footnotes_chr = "")
}
#' Print resources dataframe
#' @description print_resources_df() is a Print function that prints output to console. Specifically, this function implements an algorithm to print resources dataframe. The function is called for its side effects and does not return a value.
#' @param resources_df Resources (a data.frame)
#' @param caption_1L_chr Caption (a character vector of length one), Default: 'Resource types'
#' @param mkdn_tbl_ref_1L_chr Markdown table reference (a character vector of length one), Default: 'tab:restypes'
#' @param output_type_1L_chr Output type (a character vector of length one), Default: 'HTML'
#' @param use_rdocx_1L_lgl Use rdocx (a logical vector of length one), Default: T
#' @return No return value, called for side effects.
#' @rdname print_resources_df
#' @export 
#' @importFrom dplyr select mutate rename
#' @importFrom ready4show print_table
#' @keywords internal
print_resources_df <- function (resources_df, caption_1L_chr = "Resource types", mkdn_tbl_ref_1L_chr = "tab:restypes", 
    output_type_1L_chr = "HTML", use_rdocx_1L_lgl = T) 
{
    resources_df %>% dplyr::select(Discipline_chr, Role_Category_chr, 
        Recipient_STE_chr, Recipient_Sex_chr, Unit_Measure_chr, 
        Unit_Qty_dbl, Non_OOS_Weekly_Hours_Per_Unit_dbl, Unit_Cost_Dollars_dbl) %>% 
        dplyr::mutate(Unit_Cost_Dollars_dbl = format(Unit_Cost_Dollars_dbl, 
            big.mark = " ")) %>% dplyr::select(-Discipline_chr) %>% 
        dplyr::rename(Role = Role_Category_chr, Teams = Recipient_Sex_chr, 
            Location = Recipient_STE_chr, Unit = Unit_Measure_chr, 
            Quantity = Unit_Qty_dbl, `Weekly Non-Service Hours` = Non_OOS_Weekly_Hours_Per_Unit_dbl) %>% 
        remove_col_nms_obj_sfcs(complete_cases_1L_lgl = T) %>% 
        ready4show::print_table(output_type_1L_chr = output_type_1L_chr, 
            use_rdocx_1L_lgl = use_rdocx_1L_lgl, caption_1L_chr = caption_1L_chr, 
            mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr, big_mark_1L_chr = " ", 
            footnotes_chr = "")
}
