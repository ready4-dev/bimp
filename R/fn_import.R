#' Import input data
#' @description import_input_data() is an Import function that reads a data object in its native format and converts it to an R object. Specifically, this function implements an algorithm to import input data. The function returns Input data (a list).
#' @param path_to_data_1L_chr Path to data (a character vector of length one)
#' @return Input data (a list)
#' @rdname import_input_data
#' @export 
#' @importFrom purrr map
#' @importFrom readxl read_xlsx
#' @importFrom dplyr rename_with
#' @importFrom stats setNames
#' @keywords internal
import_input_data <- function (path_to_data_1L_chr) 
{
    data_items_chr <- c("Interventions", "Locations", "Recipients", 
        "Resources", "Resource_Use")
    input_data_ls <- purrr::map(data_items_chr, ~{
        data_tb <- readxl::read_xlsx(path_to_data_1L_chr, sheet = .x)
        dplyr::rename_with(data_tb, .fn = add_sfcs_to_var_nms, 
            data_tb = data_tb)
    }) %>% stats::setNames(paste0(data_items_chr %>% tolower(), 
        "_tb"))
    return(input_data_ls)
}
