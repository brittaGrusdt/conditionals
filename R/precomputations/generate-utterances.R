library(rwebppl)
library(tidyverse)
library(ggplot2)

source(file.path("R", "helpers.R", fsep = .Platform$file.sep))

# Parameters --------------------------------------------------------------
bias <- "none"
# bias <- "pizza"
# bias <- "lawn"

seed <- 123
n_tables_per_cn <- 1000
noise_param <- 20
noisy_or_beta <- NA
noisy_or_theta <- NA

model <- "model-general"

# Setup -------------------------------------------------------------------
target_dir <- file.path(".", "data", "precomputations", model,
                        fsep = .Platform$file.sep)

name_utts <- paste("utterances-", bias, ".rds", sep="")
target_utterances <- file.path(target_dir, name_utts, fsep = .Platform$file.sep)

# Load tables and causal nets
path_tables <- file.path(target_dir, "tables-all.rds", fsep = .Platform$file.sep)
path_cns <- file.path(target_dir, "cns.rds", fsep = .Platform$file.sep)

tables <- read_rds(path_tables)
tables <- read_rds(path_tables) %>% filter(n_tables==n_tables_per_cn &
                                           noise_v==noise_param &
                                           seed==seed)
if(is.na(noisy_or_beta)){
  tables <- tables %>% filter(is.na(beta) & is.na(theta))
} else {
  tables <- tables %>% filter(beta==noisy_or_beta & theta==noisy_or_theta)
}

causal_nets <- read_rds(path_cns)


# Run WebPPL --------------------------------------------------------------
utterances <- webppl(program_file = "./R/precomputations/generate-utterances.wppl",
                     data = list(bias=bias,
                                 tables=tables,
                                 cns=causal_nets,
                                 verbose=TRUE),
                     data_var = "data",
                     random_seed=seed)

# Save data ---------------------------------------------------------------
utterances %>% save(target_utterances)
