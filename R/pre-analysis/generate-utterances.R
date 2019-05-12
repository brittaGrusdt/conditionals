library(rwebppl)
library(tidyverse)
library(ggplot2)

source(file.path("R", "helpers.R", fsep = .Platform$file.sep))

# Parameters --------------------------------------------------------------
bias <- "none"
# bias <- "pizza"
# bias <- "lawn"

n_tables <- 1000
noise <- 0.03
seed <- 123
model <- "model-general"

# Setup -------------------------------------------------------------------
noise_str <- as.character(noise) %>% str_replace("\\.", "_")
main_folder <- paste("seed-", seed, "-noise-", noise_str, "-tables-", n_tables,
                     sep="")

target_dir <- file.path(".", "data", "precomputations", model, main_folder,
                        fsep = .Platform$file.sep)

name_utts <- paste("utterances-", bias, ".rds", sep="")
target_utterances <- file.path(target_dir, name_utts, fsep = .Platform$file.sep)

# Load tables and causal nets
path_tables <- file.path(target_dir, "tables.rds", fsep = .Platform$file.sep)
path_cns <- file.path(target_dir, "cns.rds", fsep = .Platform$file.sep)

tables <- read_rds(path_tables)
causal_nets <- read_rds(path_cns)


# Run WebPPL --------------------------------------------------------------
utterances <- webppl(program_file = "./R/pre-analysis/utterances.wppl",
                     data = list(bias=bias,
                                 tables=tables,
                                 cns=causal_nets,
                                 verbose=TRUE),
                     data_var = "data",
                     random_seed=seed)

# Save data ---------------------------------------------------------------
save <- function(data, target_path){
  data %>% write_rds(target_path)
  print(paste("saved to:", target_path))
}

utterances %>% save(target_utterances)
