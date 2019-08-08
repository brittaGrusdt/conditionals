library(rwebppl)
library(tidyverse)
library(ggplot2)
source(file.path("R", "helpers.R", fsep = .Platform$file.sep))

# Parameters --------------------------------------------------------------
n_tables <- 500
seed <- 1234
verbose <- TRUE
model <- "model-general"


noisy_or_params <-  tribble(~theta, ~beta,
                            NA, NA
                            # 0.85, 0.15,
                            # 0.9, 0.15,
                            # 0.9, 0.1,
                            # 0.95, 0.05,
                            # 0.8, 0.2,
                            # 0.85, 0.1
                            )
# fn <- "tables-independent"
fn <- "tables"

# noise_params <- c(10, 50, 100, 250)
noise_params <- c(500)

# Setup -------------------------------------------------------------------
for(idx_noisy_or in seq(1, nrow(noisy_or_params))){
  
  noisy_or_theta <- noisy_or_params[idx_noisy_or,]$theta
  noisy_or_beta <- noisy_or_params[idx_noisy_or,]$beta
  
  if(is.na(noisy_or_theta)){
    noisy_or_theta <- as.logical(noisy_or_theta)
    noisy_or_beta <- as.logical(noisy_or_beta)
  }
  print(paste('noisy_or_theta:', noisy_or_theta, 'noisy_or_beta:',
              noisy_or_beta))
  
  tables_all <- list()
  for(idx in seq(1, length(noise_params))) {
    noise <- noise_params[[idx]]
    print(paste('run with noise param', noise))
    tables_fn <- paste(fn, "-config-", idx, ".rds", sep="")
    target_dir <- file.path(".", "data", "precomputations", model,
                            fsep = .Platform$file.sep)
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
    
    target_cns <- file.path(target_dir, "cns.rds", fsep = .Platform$file.sep)
    target_tables <- file.path(target_dir, tables_fn, fsep = .Platform$file.sep)
    
    # Run WebPPL --------------------------------------------------------------
    webppl_args <- list(noise=noise,
                        n_tables=n_tables,
                        nor_beta=noisy_or_beta,
                        nor_theta=noisy_or_theta,
                        param_nor_beta=10,
                        param_nor_theta=10,
                        verbose=verbose)
    data <- webppl(program_file = "./R/precomputations/generate-tables-cns.wppl",
                   data = webppl_args,
                   data_var = "data",
                   random_seed=seed)
    
    data %>%  map(function(x){tibble(x)})
    
    causal_nets <- data$cns
    tables <- data$tables
    
    tables <- tables %>%  map(function(x){
        matrix(x, nrow(x), 5) %>% convert_data() %>% unite(cn_id, cn, rowid)
      })
    data_tables <- bind_rows(tables) %>% spread(key=cell, value = val) %>% 
                    group_by(cn_id) %>% rowid_to_column("bn_id") %>% 
                    separate(cn_id, into=c("cn", "table_id"), sep="_") %>% 
                    dplyr::select(-table_id) %>%  gather(AC,`A-C`, `-AC`, `-A-C`,
                                                  key=cell, value=val)
    plot_tables(data_tables)
    
    # Restructure tables -------------------------------------------------
    tables <- data_tables %>%
              spread(key=cell, value = val) %>%
              rowid_to_column("id") %>% 
              group_by(id) %>%
              transmute(ps=list(c(`AC`, `A-C`, `-AC`, `-A-C`)),
                        vs=list(c("AC", "A-C", "-AC", "-A-C")),
                        cn=cn) %>%
      add_column(nor_theta=noisy_or_theta, nor_beta=noisy_or_beta,
                 param_nor_beta=webppl_args$param_nor_beta,
                 param_nor_theta=webppl_args$param_nor_theta,
                 noise_v=noise, n_tables=n_tables, seed=seed) 
    
    tables_all[[idx]] <- tables
    # Save data ---------------------------------------------------------------
    tables %>% save(target_tables)
  }
  tables_new <- tables_all %>% bind_rows() 
  
  path_tables <- file.path(target_dir, "tables-all.rds",
                           fsep = .Platform$file.sep)
  if(file.exists(path_tables)){
    tables_old <- read_rds(path_tables)
    tables_new <- bind_rows(tables_old, tables_new)
  }
  tables_new %>% save(path_tables)
}
causal_nets %>% save(target_cns)
