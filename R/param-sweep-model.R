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
             threshold_maybe=0.49
             )

# parameter sweep over cost + alpha + table parameters
alphas <- seq(1, 10)
costs_conditional <- seq(0, 0.5, by=0.05)

# Load tables ----------------------------------------------------------------
target_dir <- file.path(".", "data", "precomputations", "model-general",
                        fsep = .Platform$file.sep)
tables_all <- readRDS(file.path(target_dir, "tables-all.rds", fsep=.Platform$file.sep))

param_nor_betas <- tables_all$param_nor_beta %>% unique()
param_nor_thetas <- tables_all$param_nor_theta %>% unique()

model_ids <- c(1,2)
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
          results <- tibble(param_nor_beta=param_beta,
                            param_nor_theta=param_theta,
                            cost=c, alpha=a, model_id=m)
          # extract single value to be saved
          # no bias
          no_bias_pl <- results %>% mutate(key="epistemic_uncertainty",
                                           value=get_speaker_uncertainty(model_results$PL, args$threshold),
                                           level="PL")
          no_bias_ll <- results %>%  mutate(key="epistemic_uncertainty",
                                            value=get_speaker_uncertainty(model_results$LL, args$threshold),
                                            level="LL")
          
          # biscuit conditionals
          biscuits_pl <- results %>% mutate(key="biscuits_pc",
                                            value=expected_val(model_results$PL, c("C")),
                                            level="PL")
          biscuits_ll <- results %>% mutate(key="biscuits_pc",
                                            value=expected_val(model_results$LL, c("C")),
                                            level="LL")
          
          # conditional perfection
          cp_df_pl <- get_cp_values(model_results$PL) %>%
            gather(cp_cns_hellinger, cp_probs_ev, key="key", value="value") %>% 
            add_column(level="PL")
          cp_df_ll <- get_cp_values(model_results$LL) %>%
            gather(cp_cns_hellinger, cp_probs_ev, key="key", value="value") %>%
            add_column(level="PL")
          
          cp_pl1 <- bind_cols(results, cp_df_pl[1,]) 
          cp_pl2 <- bind_cols(results, cp_df_pl[2,]) 
          cp_ll1 <- bind_cols(results, cp_df_ll[1,])
          cp_ll2 <- bind_cols(results, cp_df_ll[2,])
          
          results <- bind_rows(no_bias_ll, no_bias_pl,
                               biscuits_ll, biscuits_pl,
                               cp_pl1, cp_pl2, cp_ll1, cp_ll2)
          
          
          all_results[[idx]] <- results
          idx <- idx + 1
        }
      }
      print(paste(idx, "from", n_iter, "done"))
    }
  }
  result_data_model <- all_results[idx_model_start:idx]
  df_save <- result_data_model %>% bind_rows()
  df_save %>% save(file.path(model_params$target_dir, 
                            paste(model_params$target_fn, "-model_id-", m, ".rds", sep="")))
}

# save all results
values_all <- all_results %>% bind_rows() 
values_all %>% save("./data/results/model-general-all.rds")
  
  

