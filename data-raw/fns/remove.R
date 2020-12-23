remove_col_nms_obj_sfcs <- function(data_tb,
                                    complete_cases_1L_lgl = F,
                                    remove_underscore_1L_lgl = T){
  if(complete_cases_1L_lgl){
    data_tb <- data_tb %>%
      na.omit()
  }
  data_tb <- data_tb %>%
    dplyr::rename_all(.funs = stringr::str_replace, pattern = "_chr", replacement = "") %>%
    dplyr::rename_all(.funs = stringr::str_replace, pattern = "_dbl", replacement = "") %>%
    dplyr::rename_all(.funs = stringr::str_replace, pattern = "_lgl", replacement = "")
  if(remove_underscore_1L_lgl){
    data_tb <- data_tb %>%
      dplyr::rename_all(.funs = stringr::str_replace_all, pattern = "_", replacement = " ")
  }
  return(data_tb)
}
