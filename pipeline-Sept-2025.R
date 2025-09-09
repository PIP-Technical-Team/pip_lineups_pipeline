# load packages
library(fastverse)
library(qs)
library(joyn)

# Set key arguments
version       <- "20250930_2021_01_02_PROD"
ppp <- strsplit(version, "_")[[1]][2] |>
  as.numeric()
version_path  <- fs::path("E:/PIP/pipapi_data/", 
                          version)

# load key objects
source(fs::path("init.R")) # git creds to run create globals function=
source(fs::path("pipdata_pip_write_lineups.R")) # git creds to run create globals function
source(fs::path("pipdata_pip_estimate_lineups.R")) # git creds to run create globals function
source(fs::path("pipdata_pip_load_lineups.R")) # git creds to run create globals function
df_refy <- fst::read.fst(path = fs::path(version_path, 
                                         "estimations",
                                         "prod_ref_estimation.fst")) |> 
  get_refy_mult_factor()

gls <- pipfun::pip_create_globals(vintage = version)

# Get all .qs files
dl_aux <- read_aux_list(path = version_path)
max_lineup_year <- 
  dl_aux$metaregion |> 
  fsubset(region_code == "WLD") |> 
  fselect(lineup_year) |> 
  reg_elem() # use this lineup year


lineup_years <- 1981:2025

full_list <-
  get_full_list(lineup_years = lineup_years, 
                df_refy      = df_refy)
# is_tjk <-
#   which(unlist(full_list |>
#                lapply(\(x){return(x$country_code)})) == "TJK")
# full_list[[is_tjk]]$year <- 1981:2023
# is_ind <-
#   which(unlist(full_list |>
#                  lapply(\(x){return(x$country_code)})) == "IND")
# execute load functions
#-------------------------------------------
# tjk_list <- full_list[[is_tjk]]
# tjk_list$year <- 2025
t1 <- Sys.time()
write_multiple_refy_dist(df_refy     = df_refy,
                         cntry_refy  = full_list, #[is_tjk+1:length(full_list)],#list(tjk_list), #full_list[is_tjk], #, 
                         path        = fs::path(version_path,
                                                "lineup_data"),
                         gls         = gls,
                         dl_aux      = dl_aux)
t2 <- Sys.time()
print(t2 - t1)
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
df_refy <- fst::read.fst(path = fs::path(version_path, 
                                         "estimations",
                                         "prod_ref_estimation.fst")) |> 
  get_refy_mult_factor()
t1c <- Sys.time()
df_all <- get_full_lineup_distribution(df_refy     = df_refy,
                                       full_list   = full_list[-is_tjk], 
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
                   path     = fs::path(version_path, 
                                       "lineup_data"), 
                   obj_name = "full_country_lineups", 
                   ext      = "fst", 
                   nthreads = 4)
t6c <- Sys.time()

write_lineup_files(x        = df_all, 
                   path     = fs::path(version_path, 
                                       "lineup_data"), 
                   obj_name = "full_country_lineups", 
                   ext      = "qs", 
                   nthreads = 4)
t7c <- Sys.time()
write_lineup_files(x        = g, 
                   path     = fs::path(version_path, 
                                       "lineup_data"), 
                   obj_name = "full_country_lineups_GRP", 
                   ext      = "qs", 
                   nthreads = 4)
t8c <- Sys.time()
print(t6c - t5c)
print(t7c - t6c)
print(t8c - t7c)
