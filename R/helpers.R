library(tidyverse)
library(ggplot2)
library(rwebppl)


# Probabilities -----------------------------------------------------------
webppl_distrs_to_tibbles <- function(posterior){
  posterior_tibbles <- posterior %>%
    map(function(x){
      x <- x %>% rowid_to_column("bn_id") 
      bn_probs <- x %>% select("probs", "bn_id")
      data_tibble <- x$support %>% rowid_to_column("bn_id") %>% 
                      unnest() %>%
                      as_tibble() %>% 
                      left_join(bn_probs, by = "bn_id") %>% 
                      mutate("bn_id" = as.character(bn_id)) %>% 
                      rename(bn_probs=probs, val=table.probs, cell=table.support)
      return(data_tibble)             
    })
}
  
marginalize <- function(data, vars){
  for(var in vars){
    if(str_detect(var, "^-")){
      data <- data %>% group_by(bn_id) %>% filter(str_detect(cell, var))
    } else {
      token <- paste("-", var, sep="")
      data <- data %>% group_by(bn_id) %>% filter(!str_detect(cell, token))
    }
  }
  # data <- data %>% group_by(bn_id) %>% summarise(marginal = sum(val))
  data <- data %>% group_by(bn_id) %>% mutate(marginal = sum(val))
  return(data)
}

expected_val <- function(data, vars){
  df <- marginalize(data, vars)
  return(sum(df$val*df$bn_probs))
}

# Plotting ----------------------------------------------------------------------------------------
plot_bns <- function(data, distribution_str){
  data %>% spread(key = cell, value = val) %>% 
  ggplot() + 
    geom_bar(mapping = aes(x=bn_id, y=bn_probs), stat="identity") +
    labs(title = distribution_str)
}

plot_cns <- function(data, distribution_str){
  data %>% spread(key = cell, value = val) %>% 
  ggplot() + 
    geom_bar(mapping = aes(x=cn, y=bn_probs), stat="identity") + 
    labs(title = distribution_str, y="probability") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

plot_bn_table <- function(data, id, distribution_str){
  df <- data %>%  filter(bn_id==id)
  ggplot(data = df) + 
    geom_bar(mapping = aes(x=cell, y=val), stat="identity") +
    labs(title = paste(distribution_str, ": P(bn", id, "): ", df$bn_probs[1],
                       " cn: ", df$cn[1], sep=""),
         y="probability")
}

plot_marginal <- function(data, vars, distribution_str, density_graph = FALSE){
  df_marginal <- data %>% marginalize(vars)
   
  vars_str <- paste(vars, collapse = "")
  if(density_graph){
    p <-  ggplot(data=df_marginal, aes(x=marginal, color=marginal)) +
            geom_freqpoly(bins=10) +
            labs(x = paste('P(', vars_str, ')'),  y = "density",
                 title = distribution_str)
  }else{
    marginals <- df_marginal %>% spread(key = cell, value = val) %>%
                 mutate(marginal=as.character(marginal))
    p <- ggplot(data = marginals) +
          geom_bar(mapping = aes(x=marginal, y=bn_probs), stat="identity") +
          labs(x = paste('P(', vars_str, ')'),  y = "probability",
               title = distribution_str)
  }
  p
}

# Plot all table distributions for each causal network respectively
plot_tables <- function(data){
    cns <- data$cn %>% as.factor() %>% levels()
    for(causal_net in cns){
      p <- data %>% filter(cn==causal_net) %>%
        ggplot(aes(x=val,  color = cell)) +
            geom_density() +
            facet_wrap(~cell, scales = "free_y") +
            labs(title = causal_net)
      print(p)
    }
}


# data structures ---------------------------------------------------------
get_target_folder <- function(seed, noise, n_tables){
  noise_str <- as.character(noise) %>% str_replace("\\.", "_")
  fn <- paste("seed-", seed, "-noise-", noise_str, "-tables-", n_tables, sep="")
  return(fn)
}

save <- function(data, target_path){
  data %>% write_rds(target_path)
  print(paste("saved to:", target_path))
}

# run webppl model --------------------------------------------------------
load_data <- function(data, args){
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
  params <- list(utt=args$utt,
                 bias=df$bias,
                 tables=tables_to_wppl,
                 noise_v=args$noise_param,
                 utterances=utterances,
                 cns=causal_nets,
                 verbose=args$verbose,
                 level_max=args$level_max,
                 cost_conditional=args$cost_conditional,
                 target_path=target_path,
                 model_path=model_path)
  return(params)
}

run_webppl <- function(model_args){
  # Run and save model ------------------------------------------------------
  posterior <- webppl(program_file = model_args$model_path,
                      data = model_args,
                      data_var = "data")
  return(posterior)
}

structure_model_data <- function(posterior, model_args, params){
  posterior <- posterior %>% 
    map(function(x){
      as_tibble(x) %>% add_column(beta=params$noisy_or_beta,
                                  theta=params$noisy_or_theta,
                                  n_tables=params$n_tables_per_cn,
                                  noise_v=params$noise_param)
    })
  
  posterior_tibbles <- posterior %>% webppl_distrs_to_tibbles()
  
  # samples <- posterior %>%  map(function(x){get_samples(x, 1000000)})
  if(args$save){
    write_rds(posterior_tibbles, model_args$target_path)
    print(paste('saved results to:', model_args$target_path))
  }
  return(posterior_tibbles)
}

run_model <- function(model_args, params){
  posterior <- run_webppl(model_args)
  
  if(model_args$level_max=="speaker_all_bns"){
    df <- posterior %>% map(function(x){as_tibble(x)})
    df <- df$speaker %>% unnest() 
    result <- df %>% group_by(support) %>% summarize(p_mean=mean(probs)) %>%
      filter(support==model_args$utt) %>% pull(p_mean)
    
  }else{
    result <- structure_model_data(posterior, model_args, params)
  }
  return(result)
}



