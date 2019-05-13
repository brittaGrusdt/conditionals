library(rwebppl)
library(tidyverse)

data <- tribble(~id, ~bias, ~save_as, ~utterance, ~model_fn,
                1, "none", "indicative-conditionals", "A > C", "model-general",
                2, "pizza", "biscuit-conditionals", "A > C", "model-general",
                3, "lawn", "cp-conditionals", "A > C", "model-general",
                4, "", "skiing", "E > S",  "skiing",
                5, "", "sundowners", "R > -S", "sundowners")

# Set parameters --------------------------------------------------------------

model_id <- 2

seed <- "123"
n_tables <- 1000
noise <- 0.03
verbose <- TRUE


# Setup -------------------------------------------------------------------
df <- data %>% filter(id==model_id)
model_path <- file.path(".", "model", paste(df$model_fn, "wppl", sep="."),
                        fsep = .Platform$file.sep)

main_folder <- get_target_folder(seed, noise, n_tables)

# Load saved tables, utterances and causal networks
if(df$model_fn == "skiing" || df$model_fn== "sundowners"){
  utterances <-NULL
  tables <- NULL
  causal_nets <- NULL
  target_dir <- file.path(".", "data", "results", fsep = .Platform$file.sep)
} else {
  fn_utts <- paste("utterances-", df$bias, ".rds", sep="")
  data_dir <- file.path(".", "data", "precomputations", df$model_fn, main_folder,
                        fsep = .Platform$file.sep)
  
  path_tables <- file.path(data_dir, "tables.rds", fsep = .Platform$file.sep)
  path_cns <- file.path(data_dir, "cns.rds", fsep = .Platform$file.sep)
  
  path_utterances <- file.path(data_dir, fn_utts, fsep = .Platform$file.sep)
  
  tables <- read_rds(path_tables)
  causal_nets <- read_rds(path_cns)
  utterances <- read_rds(path_utterances)
  target_dir <- file.path(".", "data", "results", df$model_fn, main_folder,
                          fsep = .Platform$file.sep)
}

# Target files
dir.create(target_dir, recursive = TRUE)
target_path <- file.path(target_dir, paste(df$save_as, ".rds", sep=""),
                         fsep = .Platform$file.sep)
# Model params
params <- list(utt=df$utterance,
               bias=df$bias,
               tables=tables,
               utterances=utterances,
               cns=causal_nets,
               verbose=verbose) 

# Run and save model ------------------------------------------------------

posterior <- webppl(program_file = model_path,
                    data = params,
                    data_var = "data")  %>% 
             map(function(x){as_tibble(x)})

posterior_tibbles_list = list()
for(i in seq(1, length(posterior))){
  distr <- posterior_tibble[[i]]
  cells <- seq(1,4) %>%  map(function(idx_cell){
    distr$support$table.probs %>% map(function(x){nth(x, idx_cell)})
  })
  
  cells <- seq(1,4) %>%  map(function(idx_cell){
    distr$table_probs %>% map(function(x){nth(x, idx_cell)}) %>% as.numeric()
  })
  n <- nrow(distr)
                            
  distr <- distr %>% mutate("AC"=cells[[1]], "A-C"=cells[[2]],
                            "-AC"=cells[[3]], "-A-C"=cells[[4]]) %>% 
           gather(`AC`, `A-C`, `-AC`, `-A-C`, key="cell", value="val") %>% 
           select(bn_id, bn_probs, cell, val)
  
  posterior_tibbles_list[[i]] <- distr
}
names(posterior_tibbles_list) <- names(posterior)


# samples <- posterior %>%  map(function(x){get_samples(x, 1000000)})
write_rds(posterior_tibble, target_path)
print(paste('saved results to:', target_path))
