library(rwebppl)
library(tidyverse)
library(statip)
source(file.path("R", "helpers.R", fsep = .Platform$file.sep))

DATA <- tribble(~id, ~bias, ~save_as, ~utterance, ~model_fn,
                1, "none", "indicative-conditionals", "A > C", "model-general",
                2, "pizza", "biscuit-conditionals", "A > C", "model-general",
                3, "lawn", "cp-conditionals", "A > C", "model-general",
                4, "", "skiing", "E > S",  "skiing",
                5, "", "sundowners", "R > -S", "sundowners")

# Parameters  --------------------------------------------------------------
args <- list(n_tables_per_cn=500,
             noise_v=250,
             noisy_or_beta=NA,
             noisy_or_theta=NA,
             param_nor_beta=NA,  # filtered in loop
             param_nor_theta=NA, # filtered in loop
             verbose=FALSE,
             level_max="PL",
             save=FALSE,
             cost_conditional=0, # altered in loop
             alpha=5, # altered in loop
             threshold=0.89,
             threshold_maybe=0.49,
             seed=123
             )

# parameter sweep over cost + alpha + table parameters
alphas <- c(0.5, 1, 2, 3, 5, 10)
costs_conditional <- c(0, 0.01, 0.05, 0.1, 0.25, 0.5)

# Load tables ----------------------------------------------------------------
target_dir <- file.path(".", "data", "precomputations", "model-general",
                        fsep = .Platform$file.sep)
tables_all <- readRDS(file.path(target_dir, "tables-all.rds", fsep=.Platform$file.sep))

param_nor_betas <- tables_all$param_nor_beta %>% unique()
param_nor_thetas <- tables_all$param_nor_theta %>% unique()

param_nor_betas <- c(10)
param_nor_thetas <- c(10)

model_ids <- c(1,2,3)
n_iter <- length(model_ids) * length(param_nor_betas) * length(param_nor_thetas) *
  length(costs_conditional) * length(alphas)

print(paste('run model with', n_iter, 'configurations'))

all_results <- list()
idx <- 1
for(m in model_ids){
  args$model_id <- m
  model_params <- load_data(DATA, args)
  
  idx_model_start <- idx
  for(param_beta in param_nor_betas){
    for(param_theta in param_nor_thetas){
      tables <- tables_all %>%
                  filter(param_nor_beta==param_beta & param_nor_theta==param_theta)
      model_params$tables <- tables
      
      for(c in costs_conditional){
        for(a in alphas){
          model_params$cost_conditional <- c
          model_params$alpha <- a
          model_results <- run_model(model_params)
          # extract single value to be saved
          val_no_bias <- get_speaker_uncertainty(model_results, args$threshold) %>% 
                          mutate(key="epistemic_uncertainty",
                                 cost=c, alpha=a, model_id=m,
                                 param_nor_beta=param_beta,
                                 param_nor_theta=param_theta,
                                 value=as.character(value))
          
          val_biscuits <- marginalize(model_results, c("C")) %>%
                            expected_val("C") %>% select(-p) %>%
                            rename(value=ev) %>% 
                            mutate(key="biscuits_pc",
                                   cost=c, alpha=a, model_id=m,
                                   param_nor_beta=param_beta,
                                   param_nor_theta=param_theta,
                                   value=as.character(value))
          
          val_cp <- get_cp_values(model_results) %>% 
                      mutate(cost=c, alpha=a, model_id=m,
                             param_nor_beta=param_beta,
                             param_nor_theta=param_theta)
          
          results <- bind_rows(val_no_bias, val_biscuits, val_cp) %>%
                      add_column(seed=model_params$seed)
          
          all_results[[idx]] <- results
          idx <- idx + 1
        }
        print(paste(idx, "from", n_iter, "done"))
      }
      print(paste(idx, "from", n_iter, "done"))
    }
  }
  result_data_model <- all_results[idx_model_start:idx]
  df_save <- result_data_model %>% bind_rows()
  
  fp <- file.path(model_params$target_dir, 
                  paste(model_params$target_fn, "-model_id-", m, ".rds", sep=""))
  
  if(file.exists(fp)){
    df_old <- readRDS(fp)
    df_save <- bind_rows(df_old, df_save)
  }
  df_save %>%
    save(file.path(model_params$target_dir, paste(model_params$target_fn,
                                                  "-model_id-", m, ".rds", sep="")))
}

# save all results
values_all <- all_results %>% bind_rows() 

if(file.exists("./data/results/model-general-all.rds")){
  values_all_old <- readRDS("./data/results/model-general-all.rds")
  values_all <- bind_rows(values_all_old, values_all)
}
values_all %>% save("./data/results/model-general-all.rds")
  
  

