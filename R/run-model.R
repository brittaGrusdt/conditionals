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
             save=FALSE)


# run model ---------------------------------------------------------------

# check P(cn=A||C) for various parameters
params <- tribble(~beta, ~theta,
                  NA, NA,
                  0.1, 0.9,
                  0.05, 0.95)

for(i in seq(1, nrow(params))){
  args$noisy_or_beta <- params[i,]$beta
  args$noisy_or_theta <- params[i,]$theta
  
  data <- run_model(DATA, args)
  prior <- data$prior %>% spread(key = cell, value = val)  
}


# check utterance cost
args$noisy_or_beta <- 
args$noisy_or_theta <-
args$level_max <- "speaker_all_bns"
speaker <- run_model(DATA, args)



