library(ready4)
library(ready4use)
#ready4fun::write_fn_type_dirs()
pkg_desc_ls <- ready4fun::make_pkg_desc_ls(pkg_title_1L_chr = "Undertake health economic budget impact analysis." %>% tools::toTitleCase(),
                                           pkg_desc_1L_chr = "Tools for undertaking budget impact analyses for mental health projects.
  This highly preliminary development version of the bimp package has been made available as part of the process of testing and documenting the package.
  If you have any questions, please contact the authors (matthew.hamilton1@monash.edu).",
                                           authors_prsn = c(utils::person(given = "Matthew",family = "Hamilton",email = "matthew.hamilton1@monash.edu", role = c("aut", "cre"),comment = c(ORCID = "0000-0001-7407-9194")),
                                                            utils::person("Orygen", role = c("cph", "fnd"))
                                           ),
                                           urls_chr = c("https://ready4-dev.github.io/bimp/",
                                                        "https://github.com/ready4-dev/bimp",
                                                        "https://www.ready4-dev.com/"))
x <- pkg_desc_ls %>%
  ready4fun::make_manifest(addl_pkgs_ls = ready4fun::make_addl_pkgs_ls(suggests_chr = "rmarkdown"),
                           build_ignore_ls = ready4fun::make_build_ignore_ls(file_nms_chr = c("initial_setup.R")),
                           check_type_1L_chr = "ready4",
                           copyright_holders_chr = "Orygen",
                           custom_dmt_ls = ready4fun::make_custom_dmt_ls(user_manual_fns_chr = c("calculate_expenditure",
                                                                                                 "import_input_data")),
                           dev_pkgs_chr = c("ready4","ready4use", "ready4show"),
                           lifecycle_stage_1L_chr = "experimental",
                           path_to_pkg_logo_1L_chr = "../../../../../Documentation/Images/bimp-logo/default.png",
                           piggyback_to_1L_chr = "ready4-dev/ready4",

                           ready4_type_1L_chr = "modelling",
                           zenodo_badge_1L_chr = "[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5889462.svg)](https://doi.org/10.5281/zenodo.5889462)"
  )
x <- author(x)
#usethis::use_dev_package("specific", remote ="orygen/specific")
ready4::write_extra_pkgs_to_actions(consent_1L_chr = "Y")
write_to_edit_workflow("pkgdown.yaml", consent_1L_chr = "Y") # In other packages, run for "test-coverage.yaml" as well.
readLines("_pkgdown.yml") %>%
  stringr::str_replace_all("  - text: Model", "  - text: Framework & Model") %>%
  writeLines(con = "_pkgdown.yml")
devtools::build_vignettes()
# manifest_r3$subsequent_ls$abbreviations_lup <- manifest_r3$subsequent_ls$abbreviations_lup %>%
#   dplyr::filter(short_name_chr != "efc") %>%
#   ready4fun::update_abbr_lup(short_name_chr = "re",
#                              long_name_chr = "result")
# manifest_r3 <- ready4fun::write_new_abbrs(manifest_r3,
#                                            long_name_chr = c("calculation","counter-factual","demand","effective",
#                                                              "episode", "expenditure", "female", "gender", "hour",
#                                                              "included", "intervention", "occupancy", "occasion of service",
#                                                              "recipient", "resource", "weekly"),
#                                            no_plural_chr = c("effective", "included","occupancy", "weekly"))
# manifest_r3 <- ready4fun::update_msng_abbrs(manifest_r3,
#                                              are_words_chr = c("non"),
#                                              tf_to_singular_chr = c(calc = "calcs",
#                                                                     epsd = "epsds",
#                                                                     hr = "hrs",
#                                                                     intv = "intvs",
#                                                                     rcp = "rcps",
#                                                                     resc = "rescs"))
# manifest_r3 <- ready4fun::write_new_fn_types(manifest_r3,
#                                                      fn_type_desc_chr = c("Binds two objects together to create a composite object.",
#                                                                           "Modifies the format of an output."),
#                                       is_generic_lgl = T,
#                                       publish_dv_1L_lgl = T)
