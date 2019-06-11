library(rwebppl)
library(tidyverse)
source(file.path("R", "helpers.R", fsep = .Platform$file.sep))

data <- tribble(~id, ~bias, ~save_as, ~utterance, ~model_fn,
                1, "none", "indicative-conditionals", "A > C", "model-general",
                2, "pizza", "biscuit-conditionals", "A > C", "model-general",
                3, "lawn", "cp-conditionals", "A > C", "model-general",
                4, "", "skiing", "E > S",  "skiing",
                5, "", "sundowners", "R > -S", "sundowners")

# Set parameters --------------------------------------------------------------
args <- list(n_tables_per_cn=500,
             noise_param=100,
             noisy_or_beta=NA, #0.1,
             noisy_or_theta=NA, #0.9, 
             verbose=TRUE,
             model_id=3, 
             level_max="PL")
# RUN -------------------------------------------------------------------
df <- data %>% filter(id==args$model_id)
model_path <- file.path(".", "model", paste(df$model_fn, "wppl", sep="."),
                        fsep = .Platform$file.sep)

# Load saved tables, utterances and causal networks
if(df$model_fn == "skiing" || df$model_fn== "sundowners"){
  utterances <-NULL
  tables <- NULL
  causal_nets <- NULL
  target_dir <- file.path(".", "data", "results", fsep = .Platform$file.sep)
} else {
  fn_utts <- paste("utterances-", df$bias, ".rds", sep="")
  data_dir <- file.path(".", "data", "precomputations", df$model_fn,
                        fsep = .Platform$file.sep)
  
  path_tables <- file.path(data_dir, "tables-all.rds", fsep = .Platform$file.sep)
  path_cns <- file.path(data_dir, "cns.rds", fsep = .Platform$file.sep)
  path_utterances <- file.path(data_dir, fn_utts, fsep = .Platform$file.sep)
  
  tables <- read_rds(path_tables) %>% filter(n_tables==args$n_tables_per_cn &
                                               noise_v==args$noise_param)
  if(is.na(args$noisy_or_beta)){
    tables <- tables %>% filter(is.na(beta) & is.na(theta))
  } else {
    tables <- tables %>% filter(beta==args$noisy_or_beta & 
                                  theta==args$noisy_or_theta)
  }
  causal_nets <- read_rds(path_cns)
  utterances <- read_rds(path_utterances)
  target_dir <- file.path(".", "data", "results", df$model_fn,
                          fsep = .Platform$file.sep)
}

# Target files
dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
target_path <- file.path(target_dir, paste(df$save_as, ".rds", sep=""),
                         fsep = .Platform$file.sep)
# Model params
tables_to_wppl <- tables %>% select(ps, vs)
params <- list(utt=df$utterance,
               bias=df$bias,
               tables=tables_to_wppl,
               noise_v=args$noise_param,
               utterances=utterances,
               cns=causal_nets,
               verbose=args$verbose,
               level_max=args$level_max) 

# Run and save model ------------------------------------------------------

posterior <- webppl(program_file = model_path,
                    data = params,
                    data_var = "data")  %>% 
  map(function(x){
    as_tibble(x) %>% add_column(beta=args$noisy_or_beta,
                                theta=args$noisy_or_theta,
                                n_tables=args$n_tables_per_cn,
                                noise_v=args$noise_param)
  })

posterior_tibbles <- posterior %>% webppl_distrs_to_tibbles()

# samples <- posterior %>%  map(function(x){get_samples(x, 1000000)})
write_rds(posterior_tibbles, target_path)
print(paste('saved results to:', target_path))

