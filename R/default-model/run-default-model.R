source("R/default-model/helpers-tables.R")
source("R/helpers-webppl.R")
source("R/helper-functions.R")
source("R/helpers-values-of-interest.R")
library(rwebppl)
library(tidyverse)

# Parameter flags
generate_utterances <- FALSE
# generate_utterances <- TRUE
generate_tables <- FALSE
# generate_tables <- TRUE
generate_cns <- FALSE
# generate_cns <- TRUE

# Model parameters
params <- list()
# params$bias="lawn"
params$bias <- "none"
# params$level_max <- "prior_conditioned"
# params$level_max="ll_all_utts"
# params$level_max="speaker_all_bns" 
params$level_max="PL"
params$speaker_intents=c("")

params$alpha <- 3
params$cost_conditional <- 0
params$theta <- 0.9
params$utt <- "A > C"
# params$utt="likely -C"
# params$degree=0.95

# Setup -------------------------------------------------------------------
TARGET_DIR <- file.path(".", "data", "default-model", fsep = .Platform$file.sep)
dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
params$target <- file.path(TARGET_DIR, paste("results-", params$bias, sep=""), fsep=.Platform$file.sep)

# table/cns parameters
params$n_tables <- 500
params$nor_beta <- NA
params$nor_theta <- NA
params$verbose=TRUE
params$vars=c("A", "C")
params$indep_sigma=0.001
params$seed=1234
params$utts_path <- file.path(TARGET_DIR, paste("utterances-", params$bias, ".rds", sep=""),
                              fsep = .Platform$file.sep)
params$cns_path <- file.path(TARGET_DIR, "cns-default.rds", fsep=.Platform$file.sep)            

params$packages <- c("./node_modules/conditionalsHelpers",
                     "./node_modules/conditionalsDefault")
## Generate/Retrieve tables
tables_path <- file.path(TARGET_DIR, paste("tables-", params$bias, ".rds", sep=""), fsep=.Platform$file.sep)
if(generate_tables){
  tables <- create_tables(params, tables_path)
} else {
  tables <- readRDS(tables_path) %>% filter_tables(params)
  if(nrow(tables)==0){
    tables <- create_tables(params, tables_path)
  } else{
    print(paste("tables read from:", tables_path))
  }
}
tables_to_wppl <- tables %>% dplyr::select(ps, vs)
params$tables=tables_to_wppl

## Generate/Retrieve utterances and causal nets
if(generate_cns){
  cns <- run_webppl("./model/default-model/cns.wppl", params)
  cns <- cns %>% map(function(x){x %>% pull(value)}) %>% unlist()
  cns %>% save_data(params$cns_path)
} else {
  cns <- readRDS(params$cns_path)
}
params$cns <- cns

if(generate_utterances){
  utterances <- run_webppl("./model/default-model/utterances.wppl", params)
  utterances <- utterances %>% map(function(x){x %>% pull(value)}) %>% unlist()
  utterances %>% save_data(params$utts_path)
} else {
  utterances <- readRDS(params$utts_path)
}
params$utterances <- utterances

# Run Model ---------------------------------------------------------------
params$model_path="./model/default-model/default-model.wppl"
params$save=TRUE
params$save_voi=TRUE


posterior <- run_webppl(params$model_path, params)
data <- posterior %>% structure_model_data(params)

trust <- data %>% listener_beliefs("PL")
data_voi <- voi_default(data, params)
