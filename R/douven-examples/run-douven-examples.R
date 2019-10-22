source("R/helper-functions.R")
source("R/helpers-webppl.R")
source("R/helpers-values-of-interest.R")
library(rwebppl)
library(tidyverse)

model <- "skiing"
# model <- "sundowners"

# Parameters --------------------------------------------------------------
params <- list()
params$alpha=3
params$cost_conditional=0
params$level_max="prior"
params$save=TRUE
params$save_voi=TRUE
params$seed=1234
params$verbose=TRUE

if(model == "skiing"){
  params$model_path <- "./model/douven-examples/skiing.wppl"
  params$evidence <- "C"
  params$prior_pe <- 0.2
  params$utt <- "E > S"
  params$condition_on=c("C")
} else {
  params$model_path <- "./model/douven-examples/sundowners.wppl"
  params$utt <- "R > -S"
  params$condition_on=NA
  
  # prior_pr <- c(0.1, 0.2, 0.3)
  # prior_pr <- c(0.6, 0.7, 0.8)
  prior_pr <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
  # prior_pr <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
  params$prior_pr <- prior_pr
}

# Setup -------------------------------------------------------------------
TARGET_DIR <- file.path(".", "data", "douven-examples", fsep = .Platform$file.sep)
dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
fn <- file.path(TARGET_DIR, paste("results-", model, sep=""), fsep=.Platform$file.sep)

params$target <- fn
params$packages <- c("./node_modules/conditionalsHelpers")

# Run Model ---------------------------------------------------------------
posterior <- run_webppl(params$model_path, params)
data <- posterior %>% structure_model_data(params)
data_voi <- voi_douven(data, params, model)

