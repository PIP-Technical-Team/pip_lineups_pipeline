# load packages
library(fastverse)
library(pipdate)

# Set key arguments
version      <- "20250401_2021_01_02_PROD"
ppp          <- "2021"
version_path <- fs::path("E:/PIP/pipapi_data/", 
                         version)

# load key objects
df_refy <- fst::read.fst(path = fs::path(version_path, 
                                         "prod_ref_estimation.fst"))
dir     <- version_path
gls     <- list(version = version)
dl_aux 

# specify list of country-years to run
full_list


# execute load functions
#pipload:::write_multiple_refy_dist(df_refy     = df_refy,
pipdata:::write_multiple_refy_dist(df_refy     = df_refy,
                                   cntry_refy  = full_list,
                                   path        = fs::path(dir),
                                   gls         = gls2,
                                   dl_aux      = dl_aux2)





