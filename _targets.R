library(targets)
library(tarchetypes)      # for tar_group_summary() etc. (not strictly required)
library(fastverse)
library(pipdata)
library(fst)
library(glue)

tar_option_set(
  packages = c("fst", "fastverse", "pipdata", "glue"),
  memory   = "transient",        # drop large objects after each target
  format   = "qs"                # compact, fast serialization
)

# Source helper functions (every function in R/ is sourced automatically,
# but I do it explicitly to be super clear)
purrr::walk(fs::dir_ls("R", glob = "*.R"), source)

list(
  ## ------------------------------------------------------------------ ##
  ## CONSTANTS
  ## ------------------------------------------------------------------ ##
  tar_target(version,      
             "20250401_2021_01_02_PROD",   
             cue = tar_cue(mode = "thorough")),
  tar_target(ppp,          
             "2021",                       
             cue = tar_cue(mode = "thorough")),
  tar_target(version_path, 
             fs::path("E:/PIP/pipapi_data/", 
                      version)),
  tar_target(out_dir,      
             version_path),                # same as version_path in your original
  tar_target(gls,          
             list(version = version)),     # identical to your gls
  tar_target(dl_aux,       
             dl_aux2),                     # whatever object `dl_aux2` is
  
  ## ------------------------------------------------------------------ ##
  ## RAW INPUT FILE(S)
  ## ------------------------------------------------------------------ ##
  tar_target(df_refy_file,
             fs::path(version_path, "prod_ref_estimation.fst"),
             format = "file"),   # <- lets {targets} hash the *file itself*
  
  tar_target(df_refy,
             fst::read_fst(df_refy_file)),              # big data.frame
  
  ## ------------------------------------------------------------------ ##
  ## SPLIT INTO ONE SLICE PER COUNTRY-YEAR
  ## ------------------------------------------------------------------ ##
  tar_target(refy_slices,
             {
               split(df_refy,
                     interaction(df_refy$country_code,
                                 df_refy$reporting_year,
                                 drop = TRUE))
             }),
  
  ## ------------------------------------------------------------------ ##
  ## DYNAMIC BRANCH: ONE BRANCH PER ELEMENT OF refy_slices
  ## ------------------------------------------------------------------ ##
  tar_target(dist_file,
             write_single_dist(
               df_refy_slice = refy_slices,
               out_dir       = out_dir,
               gls           = gls,
               dl_aux        = dl_aux
             ),
             pattern = map(refy_slices),    # <-- creates one branch *per slice*
             format  = "file")              # <-- track the output file itself
)
