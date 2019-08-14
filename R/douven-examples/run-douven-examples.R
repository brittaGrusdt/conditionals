source("R/default-model/helpers-tables.R")
source("R/helpers.R")
library(rwebppl)
library(tidyverse)

# Parameters --------------------------------------------------------------
params <- tibble(verbose=TRUE,
                 # utt="R > -S",
                 utt="E > S",
                 # model="sundowners",
                 model="skiing",
                 alpha=3,
                 level_max="PL",
                 cost_conditional=0,
                 save=TRUE,
                 seed=1234)

# Setup -------------------------------------------------------------------
TARGET_DIR <- file.path(".", "data", "douven-examples", fsep = .Platform$file.sep)
dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)

params <- params %>% mutate(target_dir=TARGET_DIR,
                            target_fn = paste("results", model, sep="-"),
                            model_path=file.path("model", paste(model, ".wppl", sep=""))
                            ) %>% as.list()

# Run Model ---------------------------------------------------------------
posterior <- run_model(params) %>% spread(key=cell, val=val, fill = 0)

posterior %>% filter(level=="PL")