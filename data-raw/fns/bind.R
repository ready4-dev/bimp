bind_resource_tbs <- function(res_use_tb,
                              resources_tb){
  joined_tb <- res_use_tb %>%
    dplyr::left_join(nat_data_ls$resources_tb %>% dplyr::select(Resource_UID_chr,Role_Category_chr,Recipient_STE_chr,Recipient_Sex_chr)) %>%
    dplyr::select(Role_Category_chr,Recipient_STE_chr,Recipient_Sex_chr, Resource_Use, Demand_Met) %>%
    dplyr::rename(Resource = Role_Category_chr,
                  Location = Recipient_STE_chr,
                  Teams = Recipient_Sex_chr)
  return(joined_tb)
}
