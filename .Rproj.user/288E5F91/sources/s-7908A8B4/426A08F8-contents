get_all_depcys_of_fns <- function(pkg_depcy_ls,
                                  fns_chr){
  arg_ls <- list(new_dbl = pkg_depcy_ls$Nomfun %>%
                   dplyr::filter(label %in% fns_chr) %>%
                   dplyr::pull(id) %>% as.numeric(),
                 solo_dbl = numeric(0),
                 upper_tb = data.frame(from = numeric(0),to=numeric(0)))
  while(!identical(arg_ls$new_dbl,numeric(0))){
    arg_ls <- make_depnt_fns_ls(arg_ls, pkg_depcy_ls = pkg_depcy_ls)
  }
  fn_idcs_dbl <- c(arg_ls$upper_tb$to,arg_ls$solo_dbl) %>% unique() %>% sort()
  fns_to_keep_chr <- pkg_depcy_ls$Nomfun %>% dplyr::filter(id %in% fn_idcs_dbl) %>% dplyr::pull(2)
  return(fns_to_keep_chr)
}
get_arg_obj_type <- function(argument_nm_1L_chr,
                                    object_type_lup = NULL){
  if(is.null(object_type_lup))
    utils::data("object_type_lup", package="ready4fun",envir = environment())
  nchar_int <- nchar(object_type_lup$short_name_chr)
  match_chr <- object_type_lup$long_name_chr[endsWith(argument_nm_1L_chr,
                                                      paste0(ifelse(nchar(argument_nm_1L_chr)==nchar_int,"","_"),
                                                             object_type_lup$short_name_chr))]
  if(!identical(match_chr,character(0))){
    arg_obj_type_1L_chr <- dplyr::filter(object_type_lup,
                                         long_name_chr %in% match_chr) %>%
      dplyr::mutate(nchar_int = nchar(short_name_chr)) %>%
      dplyr::filter(nchar_int == max(nchar_int)) %>%
      dplyr::pull(long_name_chr)
  }else{
    arg_obj_type_1L_chr <- character(0)
  }
  return(arg_obj_type_1L_chr)
}
get_dev_pkg_nm <- function(path_to_pkg_rt_1L_chr = "."){
  dev_pkg_nm_1L_chr <- readLines(paste0(path_to_pkg_rt_1L_chr,"/DESCRIPTION"))[1] %>% stringr::str_sub(start=10)
  return(dev_pkg_nm_1L_chr)
}
get_fn_args <- function(fn){
  fn_args_chr <- as.list(args(fn)) %>%
    names() %>%
    purrr::discard({.==""})
  return(fn_args_chr)
}
get_fn_nms_in_file <- function(path_1L_chr){
  source(path_1L_chr, local=T)
  local_chr <- ls()
  local_chr <- local_chr[local_chr %>% purrr::map_lgl(~is.function(eval(parse(text=.x))))]
  return(local_chr)
}
get_from_lup_obj <- function(data_lookup_tb,
                             match_value_xx,
                             match_var_nm_1L_chr,
                             target_var_nm_1L_chr,
                             evaluate_lgl = TRUE){
  return_object_ref <- data_lookup_tb %>%
    dplyr::filter(!!rlang::sym(match_var_nm_1L_chr)==match_value_xx) %>%
    dplyr::select(!!target_var_nm_1L_chr) %>%
    dplyr::pull()
  if(evaluate_lgl){
    if(stringr::str_detect(return_object_ref,"::")){
      colon_positions <- stringr::str_locate(return_object_ref,
                                             "::")
      namespace_ref <- stringr::str_sub(return_object_ref,
                                        start=1,
                                        end=colon_positions[1,"start"]-1)
      object_ref <- stringr::str_sub(return_object_ref,
                                     start=colon_positions[1,"end"]+1)

      if(sum(stringr::str_detect(search(),paste0("package:",
                                                 namespace_ref))) == 0){
        namespace_ref_sym <- rlang::sym(namespace_ref)
        attachNamespace(namespace_ref)
        return_object_xx <- get(x = object_ref,
                                envir = as.environment(paste0("package:",
                                                              namespace_ref)))
        detach(paste0("package:",
                      namespace_ref),
               character.only = TRUE)
      }else{
        return_object_xx <- get(x = object_ref,
                                envir = as.environment(paste0("package:",
                                                              namespace_ref)))
      }
    }else{
      return_object_xx <- get(x = return_object_ref)
    }
  }else{
    return_object_xx <- return_object_ref
  }
  return(return_object_xx)
}
get_new_fn_types <- function(abbreviations_lup, # NOTE: Needs to be updated to read S4 generics and methods
                             fn_type_lup_tb,
                             fn_nms_ls = make_fn_nms(),
                             undmtd_fns_dir_chr = make_undmtd_fns_dir_chr()){
  new_fn_types_chr <- purrr::map2(fn_nms_ls[c(1,3)],
                                  undmtd_fns_dir_chr[c(1,3)],
                                  ~stringr::str_remove(.x,paste0(.y,"/")) %>% stringr::str_sub(end=-3)) %>%
    purrr::flatten_chr() %>%
    c(get_fn_nms_in_file(paste0(undmtd_fns_dir_chr[2],"/generics.R"))) %>%
    unique() %>%
    sort() %>%
    make_fn_title(abbreviations_lup = abbreviations_lup,
                  is_generic_lgl = T) %>%
    tools::toTitleCase() %>%
    setdiff(fn_type_lup_tb$fn_type_nm_chr)
  return(new_fn_types_chr)
}
get_outp_obj_type <- function(fns_chr){
  outp_obj_type_chr <- purrr::map_chr(fns_chr,
                                          ~ {
                                            return_obj_chr <- get_return_obj_nm(eval(parse(text=.x))) %>%
                                              make_arg_desc()
                                            ifelse(return_obj_chr  == "NO MATCH","NULL", return_obj_chr)
                                          })
  return(outp_obj_type_chr)
}
get_r4_obj_slots <- function(fn_name_1L_chr,
                                     package_1L_chr = ""){
  slots_ls <- className(fn_name_1L_chr,update_ns(package_1L_chr)) %>% methods::getSlots()
  slots_chr <- purrr::map_chr(slots_ls, ~ .x)
  return(slots_chr)
}
get_return_obj_nm <- function(fn){
  fn_chr <- deparse(fn)
  last_line_1L_chr <- fn_chr[length(fn_chr)-1] %>%
    trimws()
  if(startsWith(last_line_1L_chr,"return(")){
    return_1L_chr <- stringr::str_replace(last_line_1L_chr,"return","") %>%
      stringr::str_sub(start=2,end=-2)
  }else{
    return_1L_chr <- NA_character_
  }
  return(return_1L_chr)
}
