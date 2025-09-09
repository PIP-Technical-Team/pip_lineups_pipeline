
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
  
  # qs_files <- list.files(path       = fs::path(path, 
  #                                              "_aux"), 
  #                        pattern    = "\\.qs$", 
  #                        full.names = TRUE)
  # dl_aux <- lapply(qs_files, 
  #                  qread)
  # names(dl_aux) <- tools::file_path_sans_ext(basename(qs_files))
  # 
  # dl_aux
  
  aux_path <- fs::path(path, "_aux")
  
  # Read all .qs files
  qs_files <- list.files(
    path       = aux_path,
    pattern    = "\\.qs$",
    full.names = TRUE
  )
  
  qs_names <- tools::file_path_sans_ext(basename(qs_files))
  dl_aux <- lapply(qs_files, qs::qread)
  names(dl_aux) <- qs_names
  
  # Read .fst files that are not already loaded via .qs
  fst_files <- list.files(
    path       = aux_path,
    pattern    = "\\.fst$",
    full.names = TRUE
  )
  
  fst_names <- tools::file_path_sans_ext(basename(fst_files))
  
  # Filter .fst files whose base names are not in qs_names
  keep_idx <- !(fst_names %in% qs_names)
  fst_files_to_read <- fst_files[keep_idx]
  fst_names_to_add  <- fst_names[keep_idx]
  
  # Read remaining .fst files and add to dl_aux
  if (length(fst_files_to_read) > 0) {
    fst_data <- lapply(fst_files_to_read, fst::read_fst, as.data.table = TRUE)
    names(fst_data) <- fst_names_to_add
    dl_aux <- c(dl_aux, fst_data)
  }
  
  dl_aux
  
}





# 
# 
# add_aux_data_attr <- function(df,
#                               dl_aux,
#                               df_refy,
#                               py = 2021) {
#   
#   code <- attr(x = df,
#                which = "country_code")
#   year <- attr(x = df,
#                which = "reporting_year")
#   reporting_level <- attr(x     = df,
#                           which = "reporting_level_rows")$reporting_level |>
#     funique()
#   print(paste0("country:", code, " year: ", year, "rep_level: ", reporting_level)
#   aux_data_list <- aux_data(cde             = code,
#                             yr              = year,
#                             reporting_level = reporting_level,
#                             dl_aux          = dl_aux,
#                             df_refy         = df_refy,
#                             py              = py)
#   
#   attr(df,
#        "aux_data") <- aux_data_list
#   
#   df
# }











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
                          only_country = NULL, 
                          df_refy, 
                          rm_country   = NULL) {
  
  #ctry <- pipr::get_stats(fill_gaps = T)
  ctry <-
    df_refy |>
    fsubset(reporting_year <= max(lineup_years)) |> 
    fselect(country_code,
            year = reporting_year) |>
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
  
  if (!is.null(rm_country)) {
    cn <- which(unlist(full_list |>
                         lapply(\(x){return(x$country_code)})) == rm_country)
    print(cn)
    full_list <- full_list[-cn]
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
  
  # turn those entire columns into NA
  if (length(cols_to_char)) {
    df_refy[, 
            (cols_to_char) := NA, 
            .SDcols         = cols_to_char]
  }  
  
  # # Add // where necessary for interpolations
  # #---------------------------------------------------------------
  # # only apply the '//' logic to columns that sometimes need slashes
  # cols_slashed <- setdiff(cols_to_char, "estimation_type")
  # 
  # if (length(cols_slashed) > 0L) {
  #   df_refy[
  #     ,
  #     (cols_slashed) := {
  #       # computed once per group; visible inside lapply
  #       has_interp <- any(estimation_type == "interpolation", na.rm = TRUE)
  #       
  #       lapply(.SD, function(col) {
  #         vals <- unique(na.omit(col))  # distinct non-NA in first-seen order
  #         
  #         if (length(vals) > 1L) {
  #           # normal collapse: x//y//...
  #           rep(paste(vals, collapse = "//"), .N)
  #         } else if (has_interp && length(vals) == 1L) {
  #           # force interpolation format even when identical: x//x
  #           rep(paste(vals[1L], vals[1L], sep = "//"), .N)
  #         } else {
  #           # leave as-is (including all-NA)
  #           col
  #         }
  #       })
  #     },
  #     by = group_vars,
  #     .SDcols = cols_slashed
  #   ]
  # } 
  # 
  df_refy |> 
    fselect(vars) |> 
    funique()
  
  
}


























#' Identify unique variables in data frame
#' @param x data frame.
#'
#' @return character vector of unique variable names
#' @examples
#' \dontrun{
#'  df <- data.frame(a = 1, b = rnorm(5), c = 4)
#'  uniq_vars(df)
#' }
#'
#' @export
uniq_vars <- function(x) {
  
  x <- check_data_table(x)
  N_vars   <- x[, lapply(.SD, uniqueN)]
  uni_vars <- names(N_vars)[N_vars == 1]
  
  return(uni_vars)
  
}

#' Turn data to data.table if it is not already
#' @noRd
check_data_table <- function(x) {
  if (!is.data.table(x)) {
    x <- qDT(x)
  }
  x
}
#' convert variables with unique values along the data set to attributes and then
#' remove those unique variables
#'
#' @param x data frame.
#'
#' @return list of single-value variables from dataframe `x`
#' @examples
#' \dontrun{
#'  df <- data.frame(a = 1, b = rnorm(5), c = 4)
#'  uniq_vars_to_list(df)
#' }
#' @export
uniq_vars_to_list <- function(x) {
  
  x <- check_data_table(x)
  uni_vars <- uniq_vars(x)
  
  y <- x[, lapply(.SD, unique),
         .SDcols = uni_vars]
  
  as.list(y)
}

#' Return a named list with unique values of variables
#'
#' @param x A data.table
#' @param vars variable to be turn to attributes.
#' @param nm variables for naming attributes
#'
#' @return a named list with unique values
#'
vars_to_list <- function(x, vars, nm = NULL) {
  var1 <- lapply(x[, ..vars], unique)
  if(!is.null(nm)) {
    var2 <- lapply(x[, ..nm], unique)
    if(!all(mapply(\(x, y) length(x) == length(y), var1, var2))) {
      cli::cli_abort("The unique values in {.arg num_var} and {.arg name_var} column are not equal")
    }
    var1 <- Map(stats::setNames, var1, var2)
  }
  var1
}

#' convert variables with unique values along the data set to attributes and then
#' remove those unique variables
#'
#' @param x a data.frame
#' @param exclude_vars variables to be excluded from turning to attributes (default NULL)
#'
#' @return data.frame with multiple-value variables only and single-value
#'   variables as attributes
#' @export
#' @examples
#' dt <- data.table(a = 1, b = 1:10, c = 5)
#' out <- uniq_vars_to_attr(dt)
#' out[]
#' attr(out, "a")
#' attr(out, "c")
#'
#' # Exclude `a` from being added as attribute
#' out <- uniq_vars_to_attr(dt, "a")
#' out[]
#'
#' # var `a` is not included as part of the attributes
#' attr(out, "a")
#'
#' # Var `c` is
#' attr(out, "c")
uniq_vars_to_attr <- function(x, exclude_vars = NULL) {
  nm <- names(x) |>
    copy() # make sure names are not modified by reference
  # Doing everything on copy of x since we want to preserve x in it's original form
  x1 <- copy(x)
  
  # Drop exclude_vars columns
  if(!is.null(exclude_vars)) {
    # Make sure that the column names in exclude_vars is a part of data
    if( !all(exclude_vars %in% nm) ) {
      ev <- exclude_vars[!exclude_vars %in% nm]
      cli::cli_abort("{.var {ev}} {?is/are} not {?a/} column name{?s} in data.
                     Choose one of {.var {nm}}")
    }
    
    #Dropping columns from x1
    x1[, (exclude_vars) := NULL]
  }
  uvl <- uniq_vars_to_list(x1)
  
  uni_vars <- names(uvl)
  mul_vars <- setdiff(nm, uni_vars)
  x <- change_vars_to_attr(x, uvl)
  x <- x[, ..mul_vars]
  
  return(x)
}

change_vars_to_attr <- function(df, uvl) {
  for (i in seq_along(uvl)) {
    var   <- names(uvl)[i]
    value <- uvl[[i]]
    
    # make sure that attributes are set correctly for data.table.
    if (inherits(df, "data.table")) {
      setattr(df, var, value)
    } else {
      attr(df, var) <- value
    }
    
  }
  df
}


#' Get path to pipdata original files
#'
#' pipdata comes bundled with a number of internal datasets originally created
#' in CSV format and then converted to proper R format. They are placed in  its
#' `inst/extdata` directory. This function make them easy to access. This
#' function is based (mainly copied) from `readr_example` in the `readr` package
#'
#' @param file Name of file. If `NULL`, the internal files will be listed.
#' @export
#' @examples
#' pipdata_int()
#' pipdata_int("pip_pc_var_type.csv")
pipdata_int <- function(file = NULL) {
  if (is.null(file)) {
    dir(system.file("extdata", package = "pipdata"))
  } else {
    system.file("extdata", file, package = "pipdata", mustWork = TRUE)
  }
}


#' get ordered level of data_level variables
#'
#' @param dt cleaned dataframe
#' @param x data_level variable name
#'
#' @return integer
#' @noRd
get_ordered_level <- function(dt, x) {
  x_level <- unique(dt[[x]])
  d1 <- c("national")
  d2 <- c("rural", "urban")
  
  if (identical(x_level, d1)) {
    1
  } else if (identical(x_level, d2)) {
    2
  } else {
    3
  }
}

#' Make vars as attributes
#'
#' @param df A data.frame
#' @param vars variables to changed to attributes
#'
#' @return A data.frame with vars variables as attributes
#' @export
#'
#' @examples
#' \dontrun{
#' dt <- data.table(a = c(1, 2), b = 1:10, c = 5)
#' out <- vars_to_attr(dt, "a")
#' }
vars_to_attr <- function(df, vars) {
  df <- check_data_table(df)
  uvl <- vars_to_list(df, vars)
  df <- change_vars_to_attr(df, uvl)
  df[, !..vars]
}


#' Create a named vector of attributes
#'
#' @param df A data.frame
#' @param num_var Column name with numerical values
#' @param name_var Column name with name values
#'
#' @return Data.table with named attributes
#' @export
#'
#' @examples
#' \dontrun{
#'  dt <- data.table(a = c(1, 2), b = 1:10, c = c("a", "b"))
#'  out <- num_vars_to_attr(dt, "a", "c")
#' }
num_vars_to_attr <- function(df, num_var, name_var) {
  dt <- check_data_table(df)
  
  if(length(num_var) != length(name_var)) {
    cli::cli_abort("{.arg num_var} and {.arg name_var} should be of same length.
                   You have passed {length(num_var)} variable{?s} in {.arg num_var}
                   whereas {.arg name_var} consists of {length(name_var)} variable{?s}.")
  }
  uvl <- vars_to_list(dt, num_var, name_var)
  dt <- change_vars_to_attr(dt, uvl)
  c_col <- c(num_var, name_var)
  dt[, !..c_col]
}



