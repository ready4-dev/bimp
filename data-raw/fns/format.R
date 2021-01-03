format_bound_resc_tb <- function(bound_resc_tb,
                                 simple_outp_1L_lgl = T){
  
  if(!simple_outp_1L_lgl){
    bound_resc_tb <- bound_resc_tb %>%
      dplyr::mutate(`Service Level At Current WTE (percent)` = paste0( round(Service_Level_At_Current_WTE_dbl *100,1)#,"%"
      ))
    bound_resc_tb <- bound_resc_tb %>%
      dplyr::mutate(WTE_Required_For_Full_Provision_dbl = round(WTE_Required_For_Full_Provision_dbl,2),
                    Budget_Impact_Of_Full_Provision_chr = format(round(Additional_Expenditure_dbl,0), big.mark = " ")) %>%
      dplyr::select(Resource, Location, Teams,`Service Level At Current WTE (percent)`,WTE_Required_For_Full_Provision_dbl,Budget_Impact_Of_Full_Provision_chr)
  }else{
    bound_resc_tb <- bound_resc_tb %>%
      dplyr::mutate(`Resource Use (percent)` = paste0(round(OOS_resource_occupancy_dbl * 100,2)),
                    `Service Level (percent)` = paste0(round(Service_Level_At_Current_WTE_dbl * 100,2))) %>%
      dplyr::select(-OOS_resource_occupancy_dbl,-Service_Level_At_Current_WTE_dbl)
    
  }
  return(bound_resc_tb)
}