
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


#' Get full list of countries and years for lineup estimation
#' 
#' The reason this approach is used, is to identify specific countries
#' like SSD where there isn't data for all years in [lineup_years]
#'
#' @param lineup_years 
#'
#' @return
#' @export
get_full_list <- function(lineup_years, 
                          only_country = NULL) {
  
  ctry <- pipr::get_stats(fill_gaps = T)
  ctry <-
    ctry |>
    fsubset(year <= max(lineup_years)) |> 
    fselect(country_code,
            year) |>
    funique() |>
    qDT()
  
  # Group and create the list
  full_list <- ctry[,
                    .(year = list(sort(unique(year)))),
                    by = country_code][,
                                       lapply(.SD,
                                              as.list),
                                       .SDcols = c("country_code", "year")]
  
  
  # Convert rows into list of named lists
  full_list <- lapply(1:nrow(full_list),
                      \(i) {
                        list(
                          country_code = full_list$country_code[[i]],
                          year         = full_list$year[[i]])})
  
  if (!is.null(only_country)) {
    cn <- which(unlist(full_list |>
                         lapply(\(x){return(x$country_code)})) == only_country)
    full_list <- full_list[cn]
  }
  
  full_list
  
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


#' Prep refy table to be used in the api
#' 
#' This function checks for uniqueness among needed columns by country-year, 
#' and makes those that are not unique NA. Then, it selects only the necessary columns
#' and makes the df unique by country-year
#'
#' @param df_refy 
#'
#' @return data frame
#' @export
prep_df_refy_for_lineups <- function(df_refy) {
  
  vars <- colnames(df_refy)
  
  # Define grouping variables
  group_vars <- c("country_code", 
                  "reporting_year", 
                  "welfare_type", 
                  "reporting_level")
  
  # Columns to check for uniqueness (exclude the grouping vars)
  check_vars <- setdiff(vars, group_vars)
  df_refy$survey_comparability <- NA
  df_refy$comparable_spell     <- NA
  df_refy$survey_mean_lcu      <- NA
  df_refy$survey_mean_ppp      <- NA
  df_refy$survey_median_lcu    <- NA
  df_refy$survey_median_ppp    <- NA

  # Change to character
  #---------------------------------------------------------------
  # per-group count of distinct (non-NA) values for each check_var
  nu_counts <- df_refy[, 
                       lapply(.SD, uniqueN, na.rm = TRUE),
                       by      = group_vars,
                       .SDcols = check_vars]
  
  # columns that are "non-unique of length 1" in at least one group (i.e., >1 distinct value)
  cols_to_char <- check_vars[sapply(nu_counts[, 
                                              ..check_vars], 
                                    function(v) any(v > 1L))]
  
  # turn those entire columns into character
  if (length(cols_to_char)) {
    df_refy[, 
            (cols_to_char) := lapply(.SD, 
                                     as.character), 
            .SDcols         = cols_to_char]
  }  
  
  # Add // where necessary for interpolations
  #---------------------------------------------------------------
  # only apply the '//' logic to columns that sometimes need slashes
  cols_slashed <- setdiff(cols_to_char, "estimation_type")
  
  if (length(cols_slashed) > 0L) {
    df_refy[
      ,
      (cols_slashed) := {
        # computed once per group; visible inside lapply
        has_interp <- any(estimation_type == "interpolation", na.rm = TRUE)
        
        lapply(.SD, function(col) {
          vals <- unique(na.omit(col))  # distinct non-NA in first-seen order
          
          if (length(vals) > 1L) {
            # normal collapse: x//y//...
            rep(paste(vals, collapse = "//"), .N)
          } else if (has_interp && length(vals) == 1L) {
            # force interpolation format even when identical: x//x
            rep(paste(vals[1L], vals[1L], sep = "//"), .N)
          } else {
            # leave as-is (including all-NA)
            col
          }
        })
      },
      by = group_vars,
      .SDcols = cols_slashed
    ]
  } 
  
  df_refy |> 
    fselect(vars) |> 
    funique()
  
  
}




