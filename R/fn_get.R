#' Get clone targets
#' @description get_clone_targets() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get clone targets. Function argument input_data_ls specifies the where to look for the required object. The function returns Clone targets (a character vector).
#' @param input_data_ls Input data (a list)
#' @param Sex_1L_chr Sex (a character vector of length one), Default: 'F'
#' @param Target_1L_chr Target (a character vector of length one), Default: 'AUS_SNR_F'
#' @return Clone targets (a character vector)
#' @rdname get_clone_targets
#' @export 
#' @importFrom dplyr filter pull
#' @keywords internal
get_clone_targets <- function (input_data_ls, Sex_1L_chr = "F", Target_1L_chr = "AUS_SNR_F") 
{
    clone_targets_chr <- input_data_ls$recipients_tb %>% dplyr::filter(Sex_chr == 
        Sex_1L_chr) %>% dplyr::filter(Recipient_UID_chr != Target_1L_chr) %>% 
        dplyr::pull(Recipient_UID_chr)
    return(clone_targets_chr)
}
