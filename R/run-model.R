library(rwebppl)
library(tidyverse)
source(file.path("R", "helpers.R", fsep = .Platform$file.sep))

DATA <- tribble(~id, ~bias, ~save_as, ~utterance, ~model_fn,
                1, "none", "indicative-conditionals", "A > C", "model-general",
                2, "pizza", "biscuit-conditionals", "A > C", "model-general",
                3, "lawn", "cp-conditionals", "A > C", "model-general",
                4, "", "skiing", "E > S",  "skiing",
                5, "", "sundowners", "R > -S", "sundowners")

# Set parameters --------------------------------------------------------------
args <- list(n_tables_per_cn=500,
             noise_param=250,
             noisy_or_beta=NA, 
             noisy_or_theta=NA, 
             verbose=TRUE,
             model_id=1, 
             level_max="prior",
             cost_conditional=0,
             save=FALSE,
             target_path="")

utt <- DATA %>% filter(id==args$model_id) %>% pull(utterance)
args$utt <- utt



#----------------------------------------------------------------------------#
# check P(cn=A||C) for various parameters ------------------------------------
params <- tribble(~theta, ~beta,
                           NA, NA,
                           0.85, 0.15,
                           0.9, 0.15,
                           0.9, 0.1,
                           0.95, 0.05,
                           0.8, 0.2,
                           0.85, 0.1
                  )

priors <- list()
marginals <- list()
for(i in seq(1, nrow(params))){
  args$noisy_or_beta <- params[i,]$beta
  args$noisy_or_theta <- params[i,]$theta
  model_params <- load_data(DATA, args)
  
  data <- run_model(model_params, args)
  
  prior <- data$prior %>% spread(key = cell, value = val)  
  priors[[i]] <- prior

  
  marginal <- prior %>% group_by(cn) %>% summarize(p=sum(bn_probs)) 
  marginal <- marginal %>%
              spread(key=cn, value=p) %>%
              add_column(theta=params[i,]$theta, beta=params[i,]$beta)
  marginals[[i]] <- marginal
}
marginals <- marginals %>% bind_rows() 

saveRDS(marginals, "./data/precomputations/model-general/prior-cns.rds")
marginals


# check utterance cost ----------------------------------------------------
utterances <- readRDS("./data/precomputations/model-general/utterances-none.rds")
args$noisy_or_beta <- NA
args$noisy_or_theta <- NA
args$level_max <- "speaker_all_bns"

# costs_conditional <- seq(0, 1, 2)
costs_conditional <- seq(1.1, 1.5, by = 0.1)
data <- list()
i <- 1
for(cc in costs_conditional){
  print(paste('cost conditionals', cc))
  for(utt in utterances){
    args$utt <- utt
    args$cost_conditional <- cc
    model_params <- load_data(DATA, args)
    
    p_mean_utt <- run_model(model_params, args)
    
    res <- tibble(utt=utt, p_mean=p_mean_utt, cost_if=cc)
    data[[i]] <- res
    i <- i + 1
  }
}
  
data_utts <- bind_rows(data)
data_utts <- data_utts %>% spread(key=utt, value=p_mean)
saveRDS(data_utts, "./data/precomputations/model-general/utt-cost.rds")



# run model for all biases  -------------------------------------------------
model_ids <- c(1,2,3)

args <- list(n_tables_per_cn=500,
             noise_param=250,
             noisy_or_beta=NA, 
             noisy_or_theta=NA, 
             verbose=TRUE,
             model_id=1, 
             level_max="PL",
             cost_conditional=0,
             save=TRUE,
             target_path="")
utt <- DATA %>% filter(id==args$model_id) %>% pull(utterance)
args$utt <- utt


for(i in model_ids){
  args$model_id <- i
  model_params <- load_data(DATA, args)
  res <- run_model(model_params, args)
}


# run model for log-likelihood --------------------------------------------
args <- list(n_tables_per_cn=500,
             noise_param=250,
             noisy_or_beta=NA, 
             noisy_or_theta=NA, 
             verbose=TRUE,
             model_id=1, 
             level_max="logLik",
             cost_conditional=NA,
             utt=NA,
             save=FALSE,
             target_path="")

model_params <- load_data(DATA, args)
res <- run_model(model_params, args)

saveRDS(res, "./data/precomputations/logLik.rds")
