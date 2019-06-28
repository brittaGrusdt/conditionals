library(rwebppl)
library(tidyverse)
source(file.path("R", "helpers.R", fsep = .Platform$file.sep))

DATA <- tribble(~id, ~bias, ~save_as, ~utterance, ~model_fn,
                1, "none", "indicative-conditionals", "A > C", "model-general",
                2, "pizza", "biscuit-conditionals", "A > C", "model-general",
                3, "lawn", "cp-conditionals", "A > C", "model-general",
                4, "", "skiing", "E > S",  "skiing",
                5, "", "sundowners", "R > -S", "sundowners")


thetas <- seq(0.6, 1, by=0.05)


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


# model_ids <- c(1,2,3)
# for(m in model_ids){
#   args$model_id <- m
#   utt <- DATA %>% filter(id==args$model_id) %>% pull(utterance)
#   args$utt <- utt
#   
#   for(){
#     model_params <- load_data(DATA, args)
#     res <- run_model(model_params, args)
#   }
# }

