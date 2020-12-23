#' Bind resource tibbles
#' @description bind_resource_tbs() is a Bind function that binds two instances of a data-frame type class together. Specifically, this function implements an algorithm to bind resource tibbles. The function returns Joined (a tibble).
#' @param res_use_tb Res use (a tibble)
#' @param resources_tb Resources (a tibble)
#' @return Joined (a tibble)
#' @rdname bind_resource_tbs
#' @export 
#' @importFrom dplyr left_join select rename
#' @keywords internal
bind_resource_tbs <- function (res_use_tb, resources_tb) 
{
    joined_tb <- res_use_tb %>% dplyr::left_join(nat_data_ls$resources_tb %>% 
        dplyr::select(Resource_UID_chr, Role_Category_chr, Recipient_STE_chr, 
            Recipient_Sex_chr)) %>% dplyr::select(Role_Category_chr, 
        Recipient_STE_chr, Recipient_Sex_chr, Resource_Use, Demand_Met) %>% 
        dplyr::rename(Resource = Role_Category_chr, Location = Recipient_STE_chr, 
            Teams = Recipient_Sex_chr)
    return(joined_tb)
}
