source("R/default-model/helpers-tables.R")
source("R/helpers.R")
library(rwebppl)
library(tidyverse)

# Parameters --------------------------------------------------------------
params_skiing <- tibble(verbose=TRUE,
                        utt="E > S",
                        prior_pe=0.7,
                        model="skiing",
                        alpha=3,
                        level_max="PL",
                        cost_conditional=0,
                        evidence="C",
                        save=TRUE,
                        seed=1234)

params_sundowners <- tibble(verbose=TRUE,
                            utt="R > -S",
                            model="sundowners",
                            alpha=3,
                            level_max="PL",
                            cost_conditional=0,
                            save=TRUE,
                            seed=1234)
# prior_pr <- c(0.1, 0.2, 0.3)
prior_pr <- c(0.6, 0.7, 0.8)

# params <- params_skiing
params <- params_sundowners

# Setup -------------------------------------------------------------------
print(paste('run model for ', params$model, 'example'))
TARGET_DIR <- file.path(".", "data", "douven-examples", fsep = .Platform$file.sep)
dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)

params <- params %>% mutate(target_dir=TARGET_DIR,
                            target_fn = paste("results", model, sep="-"),
                            model_path=file.path("model", paste(model, ".wppl", sep=""))
                            ) %>% as.list()
params$prior_pr <- prior_pr

# Run Model ---------------------------------------------------------------
posterior <- run_model(params) %>% spread(key=cell, val=val, fill = 0)
posterior %>% filter(level=="PL")

