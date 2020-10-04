source("R/default-model/helpers-tables.R")
source("R/helpers-webppl.R")
source("R/helper-functions.R")
source("R/helpers-values-of-interest.R")
library(rwebppl)
library(tidyverse)

debug <- TRUE
# params <- configure(c("bias_none", "prior"))
# params <- configure(c("bias_none", "priorN"))
# params <- configure(c("bias_none", "ll"))
params <- configure(c("bias_none", "pl"))
# params <- configure(c("bias_none", "speaker"))
# params <- configure(c("bias_none", "speaker_literal"))
# params <- configure(c("bias_none", "speaker_p_rooij"))
# params <- configure(c("bias_none", "speaker_uncertain"))
# params <- configure(c("bias_none", "speaker_certain"))
# params <- configure(c("bias_lawn", "pl"))

if(debug){
  params$verbose <- TRUE
  params$target_dir <- "./data/test-default"
}

# Setup -------------------------------------------------------------------
dir.create(params$target_dir, recursive = TRUE)
params$target <- file.path(params$target_dir, params$target_fn, fsep=.Platform$file.sep)
params$target_params <- file.path(params$target_dir, params$target_params, fsep=.Platform$file.sep)
params$utts_path <- file.path(params$target_dir, params$utts_fn, fsep = .Platform$file.sep)

## Generate/Retrieve causal nets
if(params$generate_cns){
  cns <- run_webppl("./model/default-model/cns.wppl", params)
  cns <- cns %>% map(function(x){x %>% pull(value)}) %>% unlist()
  params$cns <- cns
}

## Generate/Retrieve tables
params$tables_path <- file.path(params$target_dir, params$tables_fn, fsep=.Platform$file.sep)
if(params$generate_tables || !file.exists(params$tables_path)){
  tables <- create_tables(params)
} else {
  tables <- readRDS(params$tables_path)
  if(nrow(tables)==0){
    tables <- create_tables(params)
  }
  print(paste("tables read from:", params$tables_path))
}
params$tables = tables %>% ungroup %>%
  dplyr::select(ps, vs, starts_with("logL"))

## Generate/Retrieve utterances
generate_utts <- function(params){
  utterances <- run_webppl("./model/default-model/utterances.wppl", params)
  utterances <- utterances %>% map(function(x){x %>% pull(value)}) %>% unlist()
  utterances %>% save_data(params$utts_path)
  return(utterances)
}
if(params$generate_utterances || !file.exists(params$utts_path)){
  utterances <- generate_utts(params)
} else {
  utterances <- readRDS(params$utts_path)
  print(paste("utterances read from:", params$utts_path))
}
params$utterances <- utterances

# Run Model ---------------------------------------------------------------
posterior <- run_webppl(params$model_path, params)

# structure + save data
# update pathes depending on configuration
if(! params$level_max %in% c("speaker", "literal-speaker")) {
  params$target <- file.path(
    params$target_dir,
    paste(str_sub(params$target_fn, 1, -5), "-", params$level_max, ".rds", sep=""),
          fsep=.Platform$file.sep
    )
  params$target_params <- str_replace(params$target, "results", "params")
}

# restructure data and save
if(params$level_max == "speaker") {
  speaker <- posterior %>% structure_speaker_data(params)
  speaker_avg <- speaker %>% average_speaker(params) %>% arrange(avg)
  speaker_avg
} else if(params$level_max %in% c("priorN")){
    data <- structure_bns(posterior, params)
} else {
  data <- posterior %>% structure_listener_data(params)
  # trust <- data %>% listener_beliefs("PL", params)
  data_voi <- voi_default(data, params)
}

