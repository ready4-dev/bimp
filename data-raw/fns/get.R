get_clone_targets <- function(input_data_ls,
                              Sex_1L_chr = "F",
                              Target_1L_chr = "AUS_SNR_F"){
  clone_targets_chr <- input_data_ls$recipients_tb %>%
    dplyr::filter(Sex_chr == Sex_1L_chr) %>%
    dplyr::filter(Recipient_UID_chr != Target_1L_chr) %>%
    dplyr::pull(Recipient_UID_chr)
  return(clone_targets_chr)
}
