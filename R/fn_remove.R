#' Remove column names object suffices
#' @description remove_col_nms_obj_sfcs() is a Remove function that edits an object, removing a specified element or elements. Specifically, this function implements an algorithm to remove column names object suffices. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param complete_cases_1L_lgl Complete cases (a logical vector of length one), Default: F
#' @param remove_underscore_1L_lgl Remove underscore (a logical vector of length one), Default: T
#' @return Data (a tibble)
#' @rdname remove_col_nms_obj_sfcs
#' @export 
#' @importFrom dplyr rename_all
#' @importFrom stringr str_replace str_replace_all
#' @keywords internal
remove_col_nms_obj_sfcs <- function (data_tb, complete_cases_1L_lgl = F, remove_underscore_1L_lgl = T) 
{
    if (complete_cases_1L_lgl) {
        data_tb <- data_tb %>% na.omit()
    }
    data_tb <- data_tb %>% dplyr::rename_all(.funs = stringr::str_replace, 
        pattern = "_chr", replacement = "") %>% dplyr::rename_all(.funs = stringr::str_replace, 
        pattern = "_dbl", replacement = "") %>% dplyr::rename_all(.funs = stringr::str_replace, 
        pattern = "_lgl", replacement = "")
    if (remove_underscore_1L_lgl) {
        data_tb <- data_tb %>% dplyr::rename_all(.funs = stringr::str_replace_all, 
            pattern = "_", replacement = " ")
    }
    return(data_tb)
}
