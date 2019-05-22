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

# Plotting ----------------------------------------------------------------

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
