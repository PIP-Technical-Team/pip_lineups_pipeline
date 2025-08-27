devtools::install_github("PIP-Technical-Team/pipdata@lineups")
# load packages
library(fastverse)
library(pipdata)
library(qs)
library(joyn)

# Set key arguments
version       <- "20250401_2021_01_02_PROD"
version2      <- "20250601_2021_01_02_PROD"
ppp           <- "2021"
version_path  <- fs::path("E:/PIP/pipapi_test_folder/", 
                          version)
version_path2 <- fs::path("E:/PIP/pipapi_test_folder/", 
                          version2)

# load key objects
source(fs::path("init.R")) # git creds to run create globals function
df_refy <- fst::read.fst(path = fs::path(version_path, 
                                         "estimations",
                                         "prod_ref_estimation.fst")) |> 
  get_refy_mult_factor()

version      <- "20250401_2021_01_02_PROD"
gls <- pipfun::pip_create_globals(vintage = version)

# Get all .qs files
dl_aux <- read_aux_list(path = version_path)
max_lineup_year <- 
  dl_aux$metaregion |> 
  fsubset(region_code == "WLD") |> 
  fselect(lineup_year) |> 
  reg_elem() # use this lineup year

lineup_years <- 1981:max_lineup_year

full_list <-
  get_full_list(lineup_years)



# execute load functions
#-------------------------------------------
pipdata:::write_multiple_refy_dist(df_refy     = df_refy,
                                   cntry_refy  = full_list, 
                                   path        = fs::path(version_path2, 
                                                          "lineup_data"),
                                   gls         = gls,
                                   dl_aux      = dl_aux)

# Create dist stats data table
#-------------------------------------------
full_dt_dist_stats <- 
  load_full_dt_dist_stats(full_list, 
                          path = fs::path(version_path, 
                                          "lineup_data"))


# Create refy table unique per country year for pipeline
#-------------------------------------------------------
setDT(df_refy)
df_refy <- prep_df_refy_for_lineups(df_refy)
fst::write.fst(df_refy,
               path = fs::path(version_path,
                               "estimations/prod_refy_estimation.fst"))
fst::write.fst(as.data.frame(lineup_years),
               path = fs::path(version_path,
                               "estimations/lineup_years.fst"))

fst::write.fst(full_dt_dist_stats,
               path = fs::path(version_path,
                               "estimations/lineup_dist_stats.fst"))






# Create huge data table
#-------------------------------
source(fs::path("lineup_distribution_functions.R"))
t1c <- Sys.time()
df_all <- get_full_lineup_distribution(df_refy     = df_refy,
                                       full_list   = full_list, 
                                       gls         = gls,
                                       dl_aux      = dl_aux)
  
t2c <- Sys.time()
print(t2c - t1c)


setorder(df_all, country_code, reporting_year, reporting_level, welfare)
t3c <- Sys.time()
g  <- GRP(df_all,
          ~ country_code + reporting_year + reporting_level,
          sort = TRUE)

t4c <- Sys.time()
df_all <- 
  df_all |> fselect(welfare, 
                    weight)
t5c <- Sys.time()
write_lineup_files(x        = df_all, 
                   path     = fs::path(version_path2, 
                                       "lineup_data"), 
                   obj_name = "full_country_lineups", 
                   ext      = "fst", 
                   nthreads = 4)
t6c <- Sys.time()

write_lineup_files(x        = df_all, 
                   path     = fs::path(version_path2, 
                                       "lineup_data"), 
                   obj_name = "full_country_lineups", 
                   ext      = "qs", 
                   nthreads = 4)
t7c <- Sys.time()
write_lineup_files(x        = g, 
                   path     = fs::path(version_path2, 
                                       "lineup_data"), 
                   obj_name = "full_country_lineups_GRP", 
                   ext      = "qs", 
                   nthreads = 4)
t8c <- Sys.time()
print(t6c - t5c)
print(t7c - t6c)
print(t8c - t7c)

# Some manual checks
#---------------------------------
get_refy_distributions(df_refy    = df_refy |> 
                         get_refy_mult_factor(),
                       cntry_code = "SSD",
                       ref_year   = 1981,
                       gls        = gls) |>
  add_aux_data_attr(dl_aux          = dl_aux,
                    df_refy         = df_refy,
                    py              = 2021)

aux_data(cde = c("SSD"), 
         yr = 1981, 
         reporting_level = "national", 
         dl_aux          = dl_aux,
         df_refy         = df_refy,
         py              = 2021)









result <- dl_aux$pce[country_code == cde,
                     ..yr]
if (result |> unlist() |> is.na() |> all()) {
  output[["pce"]] <- NA
  print("pce NA")
  #aux_data_checks
} else {
  output[["pce"]] <-
    result |>
    as.numeric()
}



# PCE
cde <- "ZAF"
yr <- "2000"
py <- 2021
reporting_level <- "national"
dl_aux$pce[country_code == cde,
             ..yr] |> 
  unlist() |> 
  unname() |> 
  funique()


# POP
tempvs  <- c("data_level",
             yr)
temp    <- dl_aux$pop[country_code == cde,
                      ..tempvs]
result <- setNames(as.list(temp[[yr]]),
                   temp$data_level)
result

# GDP
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
result

# CPI
tempvs  <- c("data_level",
             yr)
temp    <- dl_aux$cpi[country_code == cde,
                      ..tempvs]
result <- setNames(as.list(temp[[yr]]),
                   temp$data_level)
result














df_refy |> fselect(country_code, reporting_year, survey_year, survey_acronym, reporting_level) |> View()
