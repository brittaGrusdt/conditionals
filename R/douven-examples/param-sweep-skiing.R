source("R/default-model/helpers-tables.R")
source("R/helpers.R")
library(rwebppl)
library(tidyverse)

# Set Parameters that are iterated over -----------------------------------
prior_exam <- seq(0.05, 0.5, 0.05)


# Setup -------------------------------------------------------------------
n_iter <- length(ALPHAS) * length(COSTS_CONDITIONAL) * length(prior_exam)
print(paste('# iterations:', n_iter))
TARGET_DIR <- file.path(".", "data", "douven-examples", fsep = .Platform$file.sep)
params <- tibble(verbose=FALSE,
                 alpha=3,
                 level_max="PL",
                 cost_conditional=0,
                 utt="E > S",
                 save=FALSE, # save entire distributions
                 save_voi=FALSE, # save in one big df
                 model_path="./model/skiing.wppl",
                 model="skiing",
                 evidence="C",
                 target_dir=TARGET_DIR,
                 target_fn=paste("results-", model, "-voi-sweep.rds", sep=""),
                 seed=1234)

params <- params %>% as.list()

# Loop --------------------------------------------------------------------
all_results <- list()
idx <- 1
for(pe in prior_exam){
    params$prior_pe <- pe
    results <- run_model_voi(params)
    all_results[[idx]] <- results
    idx <- idx + 1
    print(paste(idx-1, "from", length(prior_exam), "done"))
}
# save all results
values_all <- all_results %>% bind_rows() 
values_all %>% save(paste(TARGET_DIR, params$target_fn, sep=.Platform$file.sep))

