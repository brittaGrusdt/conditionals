source("R/default-model/helpers-tables.R")
source("R/helpers-webppl.R")
source("R/helper-functions.R")
source("R/helpers-values-of-interest.R")
library(rwebppl)
library(tidyverse)
library(config)

debug <- TRUE
# params <- configure(c("bias_none", "prior"))
# params <- configure(c("bias_none", "ll"))
# params <- configure(c("bias_none", "pl"))
# params <- configure(c("bias_none", "speaker"))
# params <- configure(c("bias_none", "speaker_uncertain"))
# params <- configure(c("bias_none", "speaker_certain"))

params <- configure(c("bias_lawn", "pl"))

if(debug){
  params$verbose <- TRUE
  params$target_dir <- "./data/test-default"
}

# Setup -------------------------------------------------------------------
# time_id <- str_replace_all(Sys.time(), c(" "="_", ":"="_"))

dir.create(params$target_dir, recursive = TRUE)
params$target <- file.path(params$target_dir, params$target_fn, fsep=.Platform$file.sep)
params$utts_path <- file.path(params$target_dir, params$utts_fn, fsep = .Platform$file.sep)
params$cns_path <- file.path(params$target_dir, params$cns_fn, fsep = .Platform$file.sep)     

## Generate/Retrieve causal nets
if(params$generate_cns){
  cns <- run_webppl("./model/default-model/cns.wppl", params)
  cns <- cns %>% map(function(x){x %>% pull(value)}) %>% unlist()
  cns %>% save_data(params$cns_path)
  params$cns <- cns
}
## Generate/Retrieve tables
tables_path <- file.path(params$target_dir, params$tables_fn, fsep=.Platform$file.sep)
if(params$generate_tables){
  tables <- create_tables(params, tables_path, params$cns)
} else {
  tables <- readRDS(tables_path) %>% filter_tables(params)
  if(nrow(tables)==0){
    tables <- create_tables(params, tables_path, params$cns)
  }
  print(paste("tables read from:", tables_path))
}
tables_to_wppl <- tables %>% dplyr::select(ps, vs)
params$tables=tables_to_wppl

## Generate/Retrieve utterances
if(params$generate_utterances){
  utterances <- run_webppl("./model/default-model/utterances.wppl", params)
  utterances <- utterances %>% map(function(x){x %>% pull(value)}) %>% unlist()
  utterances %>% save_data(params$utts_path)
  params$utterances <- utterances
} else if(!"utterances" %in% names(params)) {
  utterances <- readRDS(params$utts_path)
  print(paste("utterances read from:", params$utts_path))
  params$utterances <- utterances
}

# Run Model ---------------------------------------------------------------
posterior <- run_webppl(params$model_path, params)

# structure + save data
if(params$level_max == "speaker"){
    speaker <- posterior %>% structure_speaker_data(params)
    speaker_avg <- speaker %>% average_speaker(params) %>% arrange(mean_per_intention)
    speaker_avg
} else{
  data <- posterior %>% structure_listener_data(params)
  # trust <- data %>% listener_beliefs("PL", params)
  data_voi <- voi_default(data, params)
}
