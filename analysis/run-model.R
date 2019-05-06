library(rwebppl)
library(tidyverse)

# Parameters --------------------------------------------------------------
#model_filename <- "sundowners"
#model_filename <- "skiing"
model_filename <- "model-with-sampling"

#utterance <- "R > -S"
#utterance <-  "E > S"
utterance <- "A > C"

bias <- "lawn"
# Setup -------------------------------------------------------------------
target_path <- file.path(".",
                         "data",
                         paste(model_filename, "rds", sep="."),
                         fsep = .Platform$file.sep)

model_path <- file.path(".", 
                        "model", 
                        paste(model_filename, "wppl", sep="."),
                        fsep = .Platform$file.sep)

params <- list(utt=utterance, bias=bias) 

# Run and save model ------------------------------------------------------

posterior <- webppl(program_file = model_path,
                    data = params,
                    data_var = "data")
posterior_tibble <- posterior %>% map(function(x){as_tibble(x)})

# samples <- posterior %>%  map(function(x){get_samples(x, 1000000)})
write_rds(posterior_tibble, target_path)

