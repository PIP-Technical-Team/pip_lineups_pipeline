
#' Estimate reference year distribution for given country, year
#'
#' @param df_refy data frame: output from [get_refy_mult_factor]
#' @param cntry_code country code
#' @param ref_year reference year for lineup
#' @param gls global list
#'
#' @return data frame:
#' @export
get_refy_distributions <- function(df_refy, cntry_code, ref_year, gls, py = 2021) {

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
            "survey_id",
            "cache_id",
            "survey_acronym",
            "distribution_type",
            "is_interpolated",
            "lineup_approach",
            "mult_factor")


  # reduce df_refy
  df_refy <-
    df_refy |>
    vars_to_attr(vars = c(
      "survey_id",
      "survey_acronym",
      "distribution_type",
      "is_interpolated",
      "lineup_approach"))
  


  # Load surveys
  cache_id <- df_refy$cache_id |>
    funique()
  gv(df_refy,
     "cache_id") <- NULL
  df_svy <- collapse::rowbind(lapply(as.list(cache_id),
                                     FUN = function(x){
                                       pipload::pip_load_cache(cache_id = x,
                                                               version  = gls$vintage_dir) |>
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

  # bottom censoring
  if (py == 2021) {
    bc <- 0.28
  } else if (py == 2017) {
    bc <- .25
  } else if (py == 2011) {
    bc <- .22
  } else {
    bc <- 0
  }
  # Bottom censoring
  df[welfare <= bc, 
    welfare := bc
  ]

  # temp
  setkey(df, NULL)

  # Add dist stats to attributes
  dist_stats <- get_dist_stats(df = df)
  attr(df,
       "dist_stats") <- dist_stats$dist_stats
  attr(df,
       "dt_dist_stats") <- dist_stats$dt_dist
  
  # keep attributes of df_refy
  attributes(df) <- c(attributes(df),
                      attributes(df_refy)[-which(names(attributes(df_refy)) %in%
                                                   c("dim",
                                                     "row.names",
                                                     "names",
                                                     "class",
                                                     ".internal.selfref",
                                                     names(attributes(df))))])


  # Add columns to attributes
  df <- vars_to_attr(df, "n_imp")
  df <- df |>
    vars_to_attr(var = c("country_code",
                         "survey_acronym",
                         "survey_year",
                         "reporting_year", 
                         "welfare_type"))

  # rm cols
  gv(df,
     c("svy_pop",
       "relative_distance",
       "reporting_pop",
       "surveyid_year",
       "mult_factor",
       "welfare_ppp",
       "imputation_id")) <- NULL

  df

}




get_refy_quantiles <- function(df, nobs = 2e4) {
  
  setorder(df, 
           reporting_level, 
           welfare)
  
  df_attr <- attributes(df)
  
  # get reporting levels
  rls <- df[, reporting_level] |> 
    funique()
  probs <- seq(1, nobs, 1)/nobs - 5/(nobs*10)
  
  qx <- lapply(rls, \(rl) {
    x    <- df[reporting_level == rl]
    xpop <- fsum(df$weight)
    
    Qx <- fquantile(x$welfare, 
                    w     = x$weight, 
                    probs =  probs, 
                    names = FALSE)
    
    data.table(welfare         = Qx, 
               weight          = xpop/nobs, 
               reporting_level = rl) |> 
      fselect(df_attr$names)
  }) |> 
    rowbind()
  
  # Add attr back
  attributes(qx) <- df_attr
  
  qx
}











#' Distribution statistics of lineup distribution
#'
#' @param df data frame: output of [get_refy_distributions]
#'
#' @return list with:
#' - dist_stats: original list of distributional stats
#' - dt_stats: a data.table with flattened statistics
#' @export
get_dist_stats <- function(df) {

  # Ensure necessary columns exist
  if (!all(c("reporting_level", "welfare", "weight", "country_code", "reporting_year") %in% names(df))) {
    stop("df must contain columns: reporting_level, welfare, weight, country_code, reporting_year")
  }

  # Extract grouping levels
  levels <- funique(df$reporting_level)

  # === Compute distributional stats ===
  min    <- fmin(df$welfare,
                 g = df$reporting_level) |>
    as.list()
  max    <- fmax(df$welfare,
                 g = df$reporting_level) |>
    as.list()
  mean   <- fmean(df$welfare,
                  w = df$weight,
                  g = df$reporting_level) |>
    as.list()
  median <- fmedian(df$welfare,
                    w = df$weight,
                    g = df$reporting_level) |>
    as.list()

  # Gini
  gini <- sapply(levels, \(x) {
    wbpip::md_compute_gini(
      welfare = df$welfare[df$reporting_level == x],
      weight  = df$weight[df$reporting_level == x]
    )
  }) |>
    as.list()

  # MLD (requires mean)
  mld <- sapply(levels, \(x) {
    wbpip::md_compute_mld(
      welfare = df$welfare[df$reporting_level == x],
      weight  = df$weight[df$reporting_level == x],
      mean    = mean[[x]]
    )
  }) |>
    as.list()

  # Polarization
  pol <- sapply(levels, \(x) {
    wbpip::md_compute_polarization(
      welfare = df$welfare[df$reporting_level == x],
      weight  = df$weight[df$reporting_level == x],
      gini    = gini[[x]],
      mean    = mean[[x]],
      median  = median[[x]]
    )}) |>
    as.list()
  
  # Deciles
  deciles <- lapply(levels, \(x) {
    d <- wbpip:::md_compute_quantiles_share(
      welfare = df$welfare[df$reporting_level == x],
      weight  = df$weight[df$reporting_level == x])
    names(d) <- paste0("decile", 1:10)
    qDT(list2DF(as.list(d)))
  })
  names(deciles) <- levels
  deciles_dt     <- rowbind(deciles)
  deciles_dt     <- data.table(reporting_level = levels,
                               deciles_dt)

  # === Output: original list ===
  dist_stats <- list(
    min          = min,
    max          = max,
    mean         = mean,
    median       = median,
    gini         = gini,
    mld          = mld,
    polarization = pol,
    deciles      = deciles)

  # === Output: data.table version ===
  country_code   <- unique(df$country_code)
  reporting_year <- unique(df$reporting_year)

  dt_dist <- data.table(
    country_code    = country_code,
    reporting_year  = reporting_year,
    reporting_level = names(mean),
    min             = unlist(min),
    max             = unlist(max),
    mean            = unlist(mean),
    median          = unlist(median),
    gini            = unlist(gini),
    mld             = unlist(mld),
    polarization    = unlist(pol)) |>
    joyn::left_join(y         = deciles_dt,
                    by        = "reporting_level",
                    reportvar = FALSE,
                    verbose   = FALSE)

  # === Return both ===
  list(
    dist_stats = dist_stats,
    dt_dist   = dt_dist)

}












#' Add auxiliary data as attribute to estimated lineup data
#'
#' @param df data frame of estimated lineups - output of [get_refy_distributions]
#' @inheritParams aux_data
#'
#' @return data frame with added attributes
#' @export
add_aux_data_attr <- function(df,
                              dl_aux,
                              df_refy,
                              py = 2021) {

  code <- attr(x = df,
               which = "country_code")
  year <- attr(x = df,
               which = "reporting_year")
  reporting_level <- attr(x     = df,
                          which = "reporting_level_rows")$reporting_level |>
    funique()

  aux_data_list <- aux_data(cde             = code,
                            yr              = year,
                            reporting_level = reporting_level,
                            dl_aux          = dl_aux,
                            df_refy         = df_refy,
                            py              = py)

  attr(df,
       "aux_data") <- aux_data_list

  df
}




#' Prepare auxiliary data for attributes to be extracted for country-reference-year
#'
#' @param cde character: country code
#' @param yr numeric: reference year
#' @param reporting_level character: national, rural, urban
#' @param dl_aux list: auxiliary data
#' @param df_refy data frame: reference year data
#' @param py numeric: PPP year. Default is 2017
#'
#' @return list
#' @export
aux_data <- function(cde,
                     yr,
                     reporting_level,
                     dl_aux,
                     df_refy,
                     py = 2021) {

  if (length(yr) > 1) cli::cli_alert_warning("reporting year non-unique")
  if (length(cde) > 1) cli::cli_alert_warning("country code non-unique")
  if (length(reporting_level) > 1) cli::cli_alert_warning("reporting level non-unique")

  stopifnot(py %in% c(2011, 2017, 2021))
  output <- list()
  yr     <- as.character(yr)

  # PCE
  if (yr %in% names(dl_aux$pce)) {
    output[["pce"]] <-
      dl_aux$pce[country_code == cde,
                 ..yr]  |>
      unlist() |>
      unname() |>
      funique()
  } else {
    output[["pce"]] <- NA_real_
  }
  

  # POP
  tempvs  <- c("data_level",
               yr)
  temp    <- dl_aux$pop[country_code == cde,
                        ..tempvs]
  result <- setNames(as.list(temp[[yr]]),
                     temp$data_level)
  output[["pop"]] <-
    result

  # GDP
  output[["gdp"]] <-
    dl_aux$gdp[country_code == cde &
               data_level   %in% reporting_level,
               ..yr] |>
    funique() |>
    as.numeric()

  # PPP
  tempvs   <- c("data_level", paste(py))
  temp    <- dl_aux$ppp[country_code == cde,
                        ..tempvs]
  result <- setNames(as.list(temp[[tempvs[2]]]),
                     temp$data_level)
  output[["ppp"]] <-
    result

  # CPI
  if (yr %in% names(dl_aux$pce)) {
    tempvs  <- c("data_level",
                 yr)
    temp    <- dl_aux$cpi[country_code == cde,
                          ..tempvs]
    result <- setNames(as.list(temp[[yr]]),
                       temp$data_level)
    output[["cpi"]] <-
      result
  } else {
    output[["cpi"]] <- NA_real_
  }

  # return
  output

}



#' Get multiplication factor and add to refy data frame
#'
#' Multiplication factor
#'
#' @param df_refy data frame: reference year (refy) table with estimation type, monotonicity, national accounts growth rates, means, etc. already calculated for each country ref-year
#'
#' @return data frame: refy table with a mult_factor column
#' @export
get_refy_mult_factor <- function(df_refy) {

  df_refy |>
    fmutate(
      lineup_approach = fcase(
        estimation_type == "extrapolation" ,
        "extrapolation",
        estimation_type == "interpolation" & monotonic == TRUE & same_direction == TRUE, "interpolation_same",
        estimation_type == "interpolation" & !(monotonic == TRUE & same_direction == TRUE),
        "interpolation_diverge",
        default = "survey"
      ),
      mult_factor = fcase(
        lineup_approach == "extrapolation" | lineup_approach == "interpolation_diverge",
        nac / nac_sy,
        lineup_approach == "interpolation_same",
        predicted_mean_ppp / svy_mean,
        default = 1
      )
    )

}
