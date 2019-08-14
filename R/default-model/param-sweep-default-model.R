source("R/default-model/helpers-tables.R")
source("R/helpers.R")
library(rwebppl)
library(tidyverse)

# Set Parameters that are iterated over -----------------------------------
ALPHAS <- c(0.5, 1, 2, 3, 5, 10)
COSTS_CONDITIONAL <- c(0, 0.01, 0.05, 0.1, 0.25, 0.5)
PARAM_NOR_BETAS <- c(10)
PARAM_NOR_THETAS <- c(10)

# utterances and tables must be generated already
# (e.g. by running run-default-model.R)

n_iter <- length(ALPHAS) * length(COSTS_CONDITIONAL) * length(PARAM_NOR_BETAS) *
      length(PARAM_NOR_THETAS)
print(paste('# iterations:', n_iter))

# Setup -------------------------------------------------------------------
TARGET_DIR <- file.path(".", "data", "default-model", fsep = .Platform$file.sep)
params <- tibble(n_tables=500,
                 nor_beta=NA,
                 nor_theta=NA,
                 param_nor_beta=10, 
                 param_nor_theta=10,
                 indep_sigma=0.001,
                 bias="none",
                 verbose=FALSE,
                 alpha=3,
                 theta=0.9,
                 level_max="PL",
                 cost_conditional=0,
                 utt="A > C",
                 save=FALSE, # save entire distributions
                 save_voi=FALSE,
                 model_path="./model/model-general.wppl",
                 model="default",
                 target_dir=TARGET_DIR,
                 target_fn=paste("results-", bias, sep=""),
                 seed=1234)

params <- params %>% as.list()


utt_path <- file.path(TARGET_DIR, paste("utterances-", params$bias, ".rds", sep=""),
                      fsep=.Platform$file.sep)
cns_path <- file.path("data", "default-model", "cns-default.rds", fsep=.Platform$file.sep)

params$utterances <- readRDS(utt_path)
params$cns <- readRDS(cns_path)

TARGET_FN <- paste("results-", params$bias, "-voi-sweep.rds", sep="")
TABLES_PATH <- file.path(TARGET_DIR, "tables-all.rds", fsep=.Platform$file.sep)

# Loop --------------------------------------------------------------------
all_results <- list()
idx <- 1
for(param_beta in PARAM_NOR_BETAS){
  for(param_theta in PARAM_NOR_THETAS){
    
    params$param_nor_beta <- param_beta
    params$param_nor_theta <- param_theta
    
    if(!file.exists(TABLES_PATH)){
      tables <- create_tables(params, TABLES_PATH)
    } else {
      tables <- readRDS(TABLES_PATH) %>% filter_tables(params)
      if(nrow(tables)==0){
        tables <- create_tables(params, TABLES_PATH)
      }
    }
    tables_to_wppl <- tables %>% dplyr::select(ps, vs)
    
    for(c in COSTS_CONDITIONAL){
      for(a in ALPHAS){
        params$cost_conditional <- c
        params$alpha <- a
        params$tables <- tables_to_wppl
        results <- run_model_voi(params)
        
        all_results[[idx]] <- results
        idx <- idx + 1
      }
    }
    print(paste(idx-1, "from", n_iter, "done"))
  }
}
# save all results
values_all <- all_results %>% bind_rows() 
values_all %>% save(paste(TARGET_DIR, TARGET_FN, sep=.Platform$file.sep))

