  library(rwebppl)
  library(tidyverse)
  library(ggplot2)
  source(file.path("R", "helpers.R", fsep = .Platform$file.sep))
  
  # Parameters --------------------------------------------------------------
  n_tables <- 500
  seed <- 123
  verbose <- TRUE
  model <- "model-general"
  
  
  noisy_or_params <-  tribble(~theta, ~beta,
                              NA, NA,
                              0.85, 0.15,
                              0.9, 0.15,
                              0.9, 0.1,
                              0.95, 0.05,
                              0.8, 0.2,
                              0.85, 0.1
                              )
  # fn <- "tables-independent"
  fn <- "tables"
  
  # noise_params <- c(10, 50, 100, 250)
  noise_params <- c(250)
  
  # Helpers
  convert_data <- function(data_tables){
    data_tables <- data_tables %>% 
      as_tibble() %>%
      mutate("V1" = as.numeric(V1),
             "V2" = as.numeric(V2),
             "V3" = as.numeric(V3),
             "V4" = as.numeric(V4),
      ) %>%
      rowid_to_column() %>%
      gather("V1", "V2", "V3", "V4", key="cell", value="val")
    
    data_tables <- data_tables %>%
      mutate(V5 = as.factor(V5), cell = as.factor(cell),
             cell = fct_recode(cell, `AC`="V1", `A-C`="V2", `-AC`="V3", `-A-C`="V4")
      ) %>% 
      rename(cn=V5)
    return(data_tables)
  }
  
  
  # Setup -------------------------------------------------------------------
  for(idx_noisy_or in seq(1, nrow(noisy_or_params))){
    
    theta <- noisy_or_params[idx_noisy_or,]$theta
    beta <- noisy_or_params[idx_noisy_or,]$beta
    
    if(is.na(theta)){
      theta <- as.logical(theta)
      beta <- as.logical(beta)
    }
    print(paste('theta:', theta, 'beta:', beta))
    
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
                          beta=beta,
                          theta=theta,
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
      # plot tables 
      plot_tables(data_tables)
      
      # Restructure tables -------------------------------------------------
      tables <- data_tables %>%
                spread(key=cell, value = val) %>%
                rowid_to_column("id") %>% 
                group_by(id) %>%
                transmute(ps=list(c(`AC`, `A-C`, `-AC`, `-A-C`)),
                          vs=list(c("AC", "A-C", "-AC", "-A-C")),
                          cn=cn) %>%
        add_column(theta=theta,beta=beta,noise_v=noise,n_tables=n_tables,seed=seed) 
      
      tables_all[[idx]] <- tables
      # Save data ---------------------------------------------------------------
      # tables %>% save(target_tables)
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
