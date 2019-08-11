source("R/default-model/helpers-tables.R")
source("R/helpers.R")
library(rwebppl)
library(tidyverse)

# Parameters --------------------------------------------------------------
params <- tibble(n_tables=500,
                 nor_beta=NA,
                 nor_theta=NA,
                 param_nor_beta=10,
                 param_nor_theta=10,
                 indep_sigma=0.001,
                 bias="none",
                 # bias="pizza",
                 # bias="lawn",
                 verbose=TRUE,
                 alpha=3,
                 level_max="PL",
                 cost_conditional=0,
                 utt="A > C",
                 save=TRUE,
                 model_path="./model/model-general.wppl",
                 seed=1234)

# Setup -------------------------------------------------------------------
CNS <- c("A implies C", "A implies -C", "-A implies C", "-A implies -C", 
         "C implies A", "C implies -A", "-C implies A", "-C implies -A", "A || C")
TARGET_DIR <- file.path(".", "data", "default-model", fsep = .Platform$file.sep)
dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)

utts_fn <- paste("utterances-", params$bias, ".rds", sep="")
utts_path <- file.path(TARGET_DIR, utts_fn, fsep = .Platform$file.sep)
utts_wppl_model <- "./R/default-model/generate-utterances.wppl"

tables_path <- file.path(TARGET_DIR, "tables-all.rds", fsep=.Platform$file.sep)

if(!file.exists(tables_path)){
  tables <- create_tables(params, tables_path)
} else {
  tables <- readRDS(tables_path) %>% filter_tables(params)
  if(nrow(tables)==0){
    tables <- create_tables(params, tables_path)
  }
}
tables_to_wppl <- tables %>% dplyr::select(ps, vs)

if(!file.exists(utts_path)){
  # Run WebPPL program to generate utterances
  args <- params %>% as.list()
  args$tables = tables
  args$cns = CNS
  utterances <- webppl(program_file = utts_wppl_model,
                       data = args,
                       data_var = "data")
  utterances %>% save(utts_path)
} else {
  utterances <- readRDS(utts_path)
}


# Run Model ---------------------------------------------------------------

model_params <- params %>% as.list()
model_params$tables=tables_to_wppl
model_params$utterances=utterances
model_params$cns=CNS
model_params$target_dir=TARGET_DIR
model_params$target_fn=paste("results-", params$bias, sep="")


posterior <- run_model(model_params)
posterior