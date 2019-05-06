library(rwebppl)
library(tidyverse)

# Parameters --------------------------------------------------------------
model_filename <- "sundowners"
#model_filename <- "skiing"
utterance <- "R > -S"
#utterance <-  "E > S"

# Setup -------------------------------------------------------------------
target_path <- file.path(".",
                         "data",
                         paste(model_filename, "rds", sep="."),
                         fsep = .Platform$file.sep)

model_path <- file.path(".", 
                        "model", 
                        paste(model_filename, "wppl", sep="."),
                        fsep = .Platform$file.sep)

params <- list(utt=utterance) 

# Run and save model ------------------------------------------------------

posterior <- webppl(program_file = model_path,
                    data = params,
                    data_var = "data") %>%
              map(function(x){as_tibble(x)})


write_rds(posterior, target_path)

