





#' Write lineup data to file
#'
#' This function saves a data object to disk in either `.qs` or `.fst` format.
#'
#' @param x The object to be saved (usually a data frame).
#' @param path A character string indicating the directory where the file should be saved.
#' @param obj_name The base name of the output file (without extension).
#' @param ext File extension. Either `"qs"` (default) or `"fst"`.
#' @param nthreads Number of threads to use when writing `.qs` files.
#'
#' @return Invisibly returns `TRUE` if the file is written successfully.
#'
#' @examples
#' \dontrun{
#' write_lineup_files(x = df, path = "output/", obj_name = "lineup_ZAF_2020", ext = "qs", nthreads = 4)
#' }
#'
#' @export
write_lineup_files <- function(x, path, obj_name, ext = "qs", nthreads = 4) {
  
  # path
  full_path <- fs::path(path, 
                        obj_name, 
                        ext = ext)
  # save object 'x'
  if (ext == "qs") {
    qs::qsave(x        = x, 
              file     = full_path,
              preset   = "fast", 
              nthreads = nthreads)
  } else if (ext == "fst") {
    fst::write_fst(x        = x, 
                   path     = full_path, 
                   compress = 50)
  }
  invisible(TRUE)
  
}










#' Estimate full lineup distributions for multiple country-year combinations
#' 
#' Output the distribution, incl country, rep. level, and year
#'
#' This function is a wrapper around [get_lineup_distribution()], applied to a list of country-year
#' combinations. It estimates lineup distributions for each specified country and reference year.
#'
#' @param df_refy A data frame containing reference year metadata, typically the output from
#'        [get_refy_mult_factor()] or a similar function that prepares lineup inputs.
#' @param full_list A list of lists, where each element is a list with two components:
#'   - `country_code`: A character scalar (e.g., `"ZAF"`),
#'   - `year`: A numeric vector of reference years (e.g., `c(2010, 2015)`).
#'
#'        This structure is consistent with the input used in [transform_input()].
#'
#' @param gls A global list containing shared configuration and metadata, including `vintage_dir`,
#'        used to specify the version of cached microdata for [pipload::pip_load_cache()].
#' @param dl_aux A list of auxiliary datasets (e.g., CPI, PPP, GDP) that can be passed along
#'        for post-estimation enhancement (currently not used in this function).
#'
#' @return A nested list of lineup distribution data frames. Each top-level element corresponds
#'         to a country in `full_list`, and each sub-element corresponds to a reference year.
#'
#' @details
#' This function:
#' \itemize{
#'   \item Iterates over each country in `full_list`
#'   \item For each country, iterates over the specified years
#'   \item Applies [get_lineup_distribution()] to compute the aligned welfare distribution
#' }
#'
#' This function does not write any files or add auxiliary attributes. For writing results to disk, use [write_refy_dist()] or [write_multiple_refy_dist()].
#'
#' @seealso [get_lineup_distribution()]
#'
#' @export
get_full_lineup_distribution <-
  function(df_refy,
           full_list,
           path,
           gls,
           dl_aux) {
    
    # ppp year
    py <- strsplit(gls$vintage_dir, "_")[[1]][2]
    
    df <- 
      lapply(cli::cli_progress_along(full_list,
                                     total = length(full_list)),
           FUN = \(i) {
             
             x <- full_list[[i]]
             
             d <- 
               lapply(x$year,
                    FUN = \(year         = x$year,
                            country_code = x$country_code){
                      
                      get_lineup_distribution(df_refy    = df_refy,
                                              cntry_code = country_code,
                                              ref_year   = year,
                                              gls        = gls)
                    }
             ) |> 
               collapse::rowbind()
           }) |> 
      collapse::rowbind()
    
    df
  }












#' Estimate lineup distribution for a given country and reference year
#' 
#' Without attributes, including cols for reporting year, reporting level, and country code
#'
#' This function prepares and estimates a lineup distribution for a specific country and reference year
#' by combining reference year metadata with microdata. It joins welfare and weight vectors,
#' adjusts population weights relative to the reference year, and computes the aligned welfare values.
#'
#' @param df_refy A data frame containing metadata for reference year alignment.
#'                Typically the output of [get_refy_mult_factor()] or similar preparation functions.
#' @param cntry_code A character scalar specifying the ISO3 country code (e.g., `"ZAF"` for South Africa).
#' @param ref_year An integer specifying the reference year (e.g., `2020`).
#' @param gls A global list containing loaded configuration such as `gls$vintage_dir`,
#'            used to locate the correct microdata versions via [pipload::pip_load_cache()].
#'
#' @return A data frame with columns:
#'   - `country_code`
#'   - `reporting_year`
#'   - `reporting_level`
#'   - `welfare` (aligned to reference year)
#'   - `weight` (adjusted to reference year population)
#'
#' @details
#' This function performs the following steps:
#' \itemize{
#'   \item Filters the reference year metadata for the given country and year.
#'   \item Loads survey microdata corresponding to the cache IDs in the metadata.
#'   \item Joins survey welfare and weight vectors with the metadata.
#'   \item Adjusts weights based on population scaling and relative distance.
#'   \item Computes reference-year-aligned welfare values using the multiplication factor.
#' }
#'
#' @export
get_lineup_distribution <- function(df_refy, cntry_code, ref_year, gls) {
  
  # ensure no factors
  lapply(df_refy,
         FUN = function(x) {
           if (is.factor(x)) {
             as.character(x)
           } else {
             x
           }
         }) |>
    qDT()
  
  # Filter df_refy
  df_refy <-
    df_refy |>
    fsubset(country_code == cntry_code &
              reporting_year == ref_year) |>
    fselect("country_code",
            "reporting_level",
            "welfare_type",
            "survey_year",
            "reporting_year",
            "relative_distance",
            "reporting_pop",
            "cache_id",
            "mult_factor")
  
  # Load surveys
  cache_id <- df_refy$cache_id |>
    funique()
  gv(df_refy,
     "cache_id") <- NULL
  df_svy <- collapse::rowbind(lapply(as.list(cache_id),
                                     FUN = function(x){
                                       pipload::pip_load_cache(cache_id = x,
                                                               version  = gls$vintage_dir, 
                                                               verbose  = FALSE) |>
                                         fselect(country_code,
                                                 surveyid_year,
                                                 survey_acronym,
                                                 survey_year,
                                                 welfare_ppp,
                                                 weight,
                                                 reporting_level,
                                                 welfare_type,
                                                 imputation_id)
                                     }))
  
  # Join welfare & weights vectors from surveys to df_refy
  df <-
    df_refy |>
    joyn(y          = df_svy,
         by         = c("country_code",
                        "survey_year",
                        "reporting_level",
                        "welfare_type"),
         keep       = "left",
         match_type = "1:m",
         verbose    = FALSE,
         sort       = FALSE,
         reportvar  = FALSE) |>
    # Group by survey year
    fgroup_by(survey_year,
              reporting_level) |>
    # number of imputations per survey year (if micro data then n_imp = 1)
    fmutate(n_imp      = data.table::uniqueN(imputation_id),
            # population at survey (decimal) year found by summing survey weights
            svy_pop    = fsum(weight),
            # weights scaled by ratio of ref year and svy year pop
            #         to make relative to reference year
            # weights adj by rel dist to get weighted average of population at ref year
            weight = weight * (reporting_pop / svy_pop) * # "adjust to WDI population" --> Andres, your comment
              relative_distance#,
            # ref year weights divided by number of imputations
            #      this should sum to population amount
            #weight_refy_adj = weight / n_imp
    ) |>
    fungroup() |>
    fmutate(welfare = welfare_ppp * mult_factor)
  
  if (any(diff(df$survey_year) < 0) &
      !any(diff(df_svy$survey_year) < 0)) {
    data.table::setorder(df,
                         survey_year)
  }
  
  # temp
  setkey(df, NULL)
  

  df |> 
    fselect(country_code, 
            reporting_year, 
            reporting_level, 
            welfare, 
            weight)
  
}
