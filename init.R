
# 1. Github credits for loading gls
#----------------------------------------------
original_gitcreds_get <- gitcreds::gitcreds_get

# Override it
assignInNamespace(
  "gitcreds_get",
  function(...) {
    creds <- original_gitcreds_get(...)
    if (is.na(creds$username)) {
      creds$username <- "zander-prinsloo"
    }
    creds
  },
  ns = "gitcreds")



# 2. aux list prep
#----------------------------------------------



#' Read aux list
#' 
#' This function reads all `.qs` files from the `_aux`
#' folder of the api data folder and outputs them as a list.
#' It is intended as input for the lineup estimation process.
#'
#' @param path 
#'
#' @return named list of aux data
#' @export
read_aux_list <- function(path) {
  
  qs_files <- list.files(path       = fs::path(path, 
                                               "_aux"), 
                         pattern    = "\\.qs$", 
                         full.names = TRUE)
  dl_aux <- lapply(qs_files, 
                   qread)
  names(dl_aux) <- tools::file_path_sans_ext(basename(qs_files))
  
  dl_aux
}







#' Find the relative distance from survey years to reference year to interpolate
#'
#' @param ref_year
#' @param svy_year
#'
#' @return
#' @examples
relative_distance <- \(ref_year, svy_year) {

  ls <- length(svy_year)
  ry <- unique(ref_year)

  stopifnot(exprs = {
    length(ry) == 1
    ls %in% c(1, 2)
  })

  if (ls == 1)
    return(1)

  
  dist <-  abs(svy_year - ref_year)
  1 - dist/sum(dist)
}


prep_df_refy_for_lineups <- function(df_refy) {
  
  # variables to check for duplicates
  vars <- c("survey_acronym", 
            "survey_coverage",
            "comparable_spell",
            "ppp",
            "cpi",
            "distribution_type",
            "estimation_type", 
            "country_code", 
            "reporting_year", 
            "reporting_level", 
            "welfare_type", 
            "income_group_code",
            "lineup_approach",
            "wb_region_code",
            "is_interpolated")
  
  # Define grouping variables
  group_vars <- c("country_code", 
                  "reporting_year", 
                  "welfare_type", 
                  "reporting_level")
  
  # Columns to check for uniqueness (exclude the grouping vars)
  check_vars <- setdiff(vars, group_vars)
  
  df_refy[, 
          (check_vars) := lapply(.SD, function(col) {
            # For each group, if column is not unique, set NA
            if (length(unique(col)) == 1L) {
              col
              } else {
                rep(NA, .N)
                }
            }), 
          by            = group_vars, 
          .SDcols       = check_vars]
  
  df_refy |> 
    fselect(vars) |> 
    funique()
  
  
}




