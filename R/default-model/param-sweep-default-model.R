source("R/default-model/helpers-tables.R")
library(rwebppl)
library(tidyverse)

ALPHAS <- c(0.5)#, 1, 2, 3, 5, 10)
COSTS_CONDITIONAL <- c(0)#, 0.01, 0.05, 0.1, 0.25, 0.5)
PARAM_NOR_BETAS <- c(10)
PARAM_NOR_THETAS <- c(10)

CNS <- c("A implies C", "A implies -C", "-A implies C", "-A implies -C", 
         "C implies A", "C implies -A", "-C implies A", "-C implies -A", "A || C")
UTTERANCES <- c("-C", "C", "-A", "A",
                "-A and -C", "-A and C", "A and -C", "A and C",
                "C > -A", "C > A", "-C > -A", "-C > A",
                "-A > -C", "A > -C", "-A > C", "A > C", 
                "maybe -C", "maybe C", "maybe -A", "maybe A")

TARGET_DIR <- file.path(".", "data", "default-model", fsep = .Platform$file.sep)
TARGET_FN <- "results-voi-default-model.rds"
TABLES_PATH <- file.path(TARGET_DIR, "tables-all.rds", fsep=.Platform$file.sep)
params <- tibble(n_tables=500,
                 nor_beta=NA,
                 nor_theta=NA,
                 param_nor_beta=10,
                 param_nor_theta=10,
                 indep_sigma=0.001,
                 bias="none",
                 verbose=TRUE,
                 alpha=3,
                 level_max="PL",
                 cost_conditional=0,
                 utt="A > C",
                 save=FALSE, # save entire distributions
                 model_path="./model/model-general.wppl",
                 target_dir=TARGET_DIR,
                 target_fn=paste("results-", bias, sep=""),
                 seed=1234)


voi_epistemic_uncertainty <- function(posterior, params){
  val_no_bias <- get_speaker_uncertainty(posterior, args$threshold) %>% 
    mutate(key="epistemic_uncertainty",
           cost=params$cost_conditional, alpha=params$alpha,
           param_nor_beta=param_beta,
           param_nor_theta=param_theta,
           value=as.character(value))
  return(val_no_bias)
}

voi_pc <- function(posterior, params){
  val_biscuits <- marginalize(posterior, c("C")) %>%
    expected_val("C") %>% select(-p) %>%
    rename(value=ev) %>% 
    mutate(key="biscuits_pc",
           cost=params$cost_conditional, alpha=params$alpha, 
           param_nor_beta=param_beta,
           param_nor_theta=param_theta,
           value=as.character(value))
  return(val_biscuits)
}

voi_pa <- function(posterior, params){
  val_pa <- marginalize(posterior, c("A")) %>% 
    expected_val("A") %>% select(-p) %>% 
    rename(value=ev) %>% 
    mutate(key="pa",
           cost=c, alpha=a, model_id=m,
           param_nor_beta=param_beta,
           param_nor_theta=param_theta,
           value=as.character(value))
  return(val_pa)
}

voi_conditional_perfection <- function(posterior, params){
  val_cp <- get_cp_values(posterior) %>% 
    mutate(cost=params$cost_conditional, alpha=params$alpha,
           param_nor_beta=param_beta,
           param_nor_theta=param_theta)
  return(val_cp)
}

get_voi <- function(posterior, params){
  uncertainty <- voi_epistemic_ucnertainty(posterior, params)
  pa <- voi_pa(posterior, params)
  pc <- voi_pc(posterior, params)
  cp <- voi_conditional_perfection(posterior, params)
  
  results <- bind_rows(uncertainty, pa, pc, cp) %>%
    add_column(seed=model_params$seed,
               n_tables=model_params$n_tables_per_cn)
  return(results)
  
}

run_default_model <- function(tables_to_wppl, params){
  model_params <- params %>% as.list()
  model_params$utterances=UTTERANCES
  model_params$cns=CNS
  model_params$tables=tables_to_wppl
  posterior <- run_model(model_params)
  voi <- get_voi(posterior, params)
  return(voi)
}


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
        results <- run_default_model(tables_to_wppl, params)
        
        all_results[[idx]] <- results
        idx <- idx + 1
      }
    }
    print(paste(idx, "from", n_iter, "done"))
  }
}
# save all results
values_all <- all_results %>% bind_rows() 
values_all %>% save(paste(TARGET_DIR, TARGET_FN))

