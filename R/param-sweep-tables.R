library(rwebppl)
library(tidyverse)
library(ggplot2)
source(file.path("R", "helpers.R", fsep = .Platform$file.sep))

# Parameters  --------------------------------------------------------------
n_tables <- 500
seed <- 123
verbose <- TRUE
model <- "model-general"
noise_v <- 250


# Setup -------------------------------------------------------------------
print(paste('noise_v set to', noise_v))
target_dir <- file.path(".", "data", "precomputations", model,
                        fsep = .Platform$file.sep)
dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
target_cns <- file.path(target_dir, "cns.rds", fsep = .Platform$file.sep)


# theta and beta are sampled for each Bayes net seperately
noisy_or_theta <- as.logical(NA)
noisy_or_beta <- as.logical(NA)

# parameters for beta distributions from which noisy or theta and beta are
# sampled
param_nor_thetas <- seq(4, 13)
param_nor_betas <- c(10, 15, 20)

i <- 0
tables_all <- list()
for(param_theta in param_nor_thetas){
  for(param_beta in param_nor_betas){
    i <- i + 1
    print(paste("run with param_nor_theta:", param_theta,
                "param_nor_beta:", param_beta))
    # Run WebPPL --------------------------------------------------------------
    webppl_args <- list(noise=noise_v,
                        n_tables=n_tables,
                        noisy_or_beta=noisy_or_beta,
                        noisy_or_theta=noisy_or_theta,
                        param_nor_beta=param_beta,
                        param_nor_theta=param_theta,
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
      select(-table_id) %>%  gather(AC,`A-C`, `-AC`, `-A-C`,
                                    key=cell, value=val)
    
    # Restructure tables -------------------------------------------------
    tables <- data_tables %>%
      spread(key=cell, value = val) %>%
      rowid_to_column("id") %>% 
      group_by(id) %>%
      transmute(ps=list(c(`AC`, `A-C`, `-AC`, `-A-C`)),
                vs=list(c("AC", "A-C", "-AC", "-A-C")),
                cn=cn) %>%
      add_column(noisy_or_theta=noisy_or_theta, noisy_or_beta=noisy_or_beta,
                 param_nor_theta=param_theta, param_nor_beta=param_beta,
                 noise_v=noise_v, n_tables=n_tables, seed=seed) 
    
    tables_all[[i]] <- tables
    
    # save tables of current config
    tables_fn <- paste("tables-config-", i, ".rds", sep="")
    target_tables <- file.path(target_dir, tables_fn, fsep = .Platform$file.sep)
    tables %>% save(target_tables)
    
  }
}

# Save all data -----------------------------------------------------------
tables_new <- tables_all %>% bind_rows() 

path_tables <- file.path(target_dir, "tables-all.rds", fsep = .Platform$file.sep)
tables_new %>% save(path_tables)
causal_nets %>% save(target_cns)
