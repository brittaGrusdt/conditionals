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
             noise_v=250,
             nor_beta=NA, 
             nor_theta=NA, 
             param_nor_beta=10,
             param_nor_theta=10,
             alpha=5,
             verbose=TRUE,
             model_id=1, 
             level_max="PL",
             cost_conditional=0,
             save=FALSE
             )

# check utterance cost ----------------------------------------------------
utterances <- readRDS("./data/precomputations/model-general/utterances-none.rds")
args$nor_beta <- NA
args$nor_theta <- NA
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
    
    p_mean_utt <- run_model(model_params)
    
    res <- tibble(utt=utt, p_mean=p_mean_utt, cost_if=cc)
    data[[i]] <- res
    i <- i + 1
  }
}
  
data_utts <- bind_rows(data)
data_utts <- data_utts %>% spread(key=utt, value=p_mean)
saveRDS(data_utts, "./data/precomputations/model-general/utt-cost.rds")
