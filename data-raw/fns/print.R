print_expdr_summ_tb <- function(expdr_summ_tb,
                                caption_1L_chr = "Predicted Expenditure",
                                mkdn_tbl_ref_1L_chr = "tab:exptst",
                                output_type_1L_chr = "HTML",
                                use_rdocx_1L_lgl = T){
  expdr_summ_tb %>%
    dplyr::mutate(Annual_Cost = format(Expenditure_dbl, big.mark = " ")) %>%
    dplyr::select(-Expenditure_dbl) %>%
    remove_col_nms_obj_sfcs(complete_cases_1L_lgl = T) %>%
    ready4show::print_table(output_type_1L_chr = output_type_1L_chr,
                            use_rdocx_1L_lgl = use_rdocx_1L_lgl,
                            caption_1L_chr = "Predicted Expenditure",
                            mkdn_tbl_ref_1L_chr = "tab:exptst",
                            footnotes_chr = "", big_mark_1L_chr = " ")
}
print_intvs_df <- function(intvs_df,
                           caption_1L_chr = "Included mental health interventions",
                           mkdn_tbl_ref_1L_chr = "tab:intvs",
                           output_type_1L_chr = "HTML",
                           use_rdocx_1L_lgl = T){

  intvs_df %>%
    dplyr::select(-Intervention_UID_chr,-Domain_UID_chr, - Intervention_Cat_UID_chr, -Discipline_UID_chr, -Role_Cat_UID_chr, -Location_chr) %>%
    dplyr::rename(Quantity = N_OOS_Per_Episode_dbl,
                  `Duration (mins)` = Duration_OOS_Mins_dbl,
                  Timeframe = Timeframe_In_Weeks_chr) %>%
    dplyr::select(-Domain_chr, - Intervention_Category_chr) %>%
    dplyr::mutate(Intervention_Format_chr = dplyr::case_when(Intervention_Format_chr == "F2F" ~ "Face to face", Intervention_Format_chr == "Provider only" ~ "No client interaction",T ~ Intervention_Format_chr)) %>%
    remove_col_nms_obj_sfcs(complete_cases_1L_lgl = T) %>%
    ready4show::print_table(output_type_1L_chr = output_type_1L_chr,
                            use_rdocx_1L_lgl = use_rdocx_1L_lgl,
                            caption_1L_chr = caption_1L_chr,
                            mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr,
                            footnotes_chr = "")
}
print_recpts_tb <- function(recipients_tb,
                            caption_1L_chr = "Player groups of interest",
                            mkdn_tbl_ref_1L_chr = "tab:pgps",
                            output_type_1L_chr = "HTML",
                            use_rdocx_1L_lgl = F){
  recipients_tb %>%
    dplyr::select(-Recipient_UID_chr,-Recipient_Type_chr, -Location_UID_chr,- Age_Band_chr, - Note_chr) %>%
    remove_col_nms_obj_sfcs(complete_cases_1L_lgl = T) %>%
    ready4show::print_table(output_type_1L_chr = output_type_1L_chr,
                            use_rdocx_1L_lgl = use_rdocx_1L_lgl,
                            caption_1L_chr = caption_1L_chr,
                            mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr,
                            footnotes_chr = "")
}
print_resc_occupcy_tb <- function(resc_occupcy_tb,
                                  resources_tb,
                                  caption_1L_chr = "Resource occupancy and proportion of demand met (predicted from current input data)",
                                  mkdn_tbl_ref_1L_chr = "tab:metneed",
                                  output_type_1L_chr = "HTML",
                                  use_rdocx_1L_lgl = T){
  resc_occupcy_tb  %>%
    update_resc_occupcy_tb(resources_tb = resources_tb) %>%
    format_bound_resc_tb() %>%
    remove_col_nms_obj_sfcs(complete_cases_1L_lgl = T) %>%
    ready4show::print_table(output_type_1L_chr = output_type_1L_chr,
                            use_rdocx_1L_lgl = use_rdocx_1L_lgl,
                            caption_1L_chr = "Resource occupancy and proportion of demand met (predicted from current input data)",
                            mkdn_tbl_ref_1L_chr = "tab:metneed"
    )
}
print_resources_df <- function(resources_df,
                               caption_1L_chr = "Resource types",
                               mkdn_tbl_ref_1L_chr = "tab:restypes",
                               output_type_1L_chr = "HTML",
                               use_rdocx_1L_lgl = T){
  resources_df %>%
    dplyr::select(Discipline_chr,Role_Category_chr, Recipient_STE_chr,Recipient_Sex_chr, Unit_Measure_chr, Unit_Qty_dbl, Non_OOS_Weekly_Hours_Per_Unit_dbl, Unit_Cost_Dollars_dbl) %>%
    dplyr::mutate(Unit_Cost_Dollars_dbl = format(Unit_Cost_Dollars_dbl, big.mark = " ")) %>%
    dplyr::select(-Discipline_chr) %>%
    dplyr::rename(Role = Role_Category_chr,
                  Teams = Recipient_Sex_chr,
                  Location = Recipient_STE_chr,
                  Unit = Unit_Measure_chr,
                  Quantity = Unit_Qty_dbl,
                  `Weekly Non-Service Hours` = Non_OOS_Weekly_Hours_Per_Unit_dbl) %>%
    remove_col_nms_obj_sfcs(complete_cases_1L_lgl = T) %>%
    ready4show::print_table(output_type_1L_chr = output_type_1L_chr,
                            use_rdocx_1L_lgl = use_rdocx_1L_lgl,
                            caption_1L_chr = caption_1L_chr,
                            mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr,
                            big_mark_1L_chr = " ",
                            footnotes_chr = "")
}
print_resource_use_df <- function(resource_use_df,
                                  caption_1L_chr = "Resource use",
                                  mkdn_tbl_ref_1L_chr = "tab:resuse",
                                  output_type_1L_chr = "HTML",
                                  use_rdocx_1L_lgl = T){
  resource_use_df %>%
    dplyr::select(-Intervention_UID_chr,	-Recipient_UID_chr,	-Proportion_Each_Timeframe_dbl,	-Resource_UID_chr)  %>%
    dplyr::rename(`Player Group` = Recipient,`Usage (percent)` = Proportion_Using_Service) %>%
    remove_col_nms_obj_sfcs(complete_cases_1L_lgl = T) %>%
    ready4show::print_table(output_type_1L_chr = output_type_1L_chr,
                            use_rdocx_1L_lgl = use_rdocx_1L_lgl,
                            caption_1L_chr = "Resource use",
                            mkdn_tbl_ref_1L_chr = "tab:resuse",
                            footnotes_chr = "")
}

