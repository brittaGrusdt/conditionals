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
    labs(title = distribution_str, y="probability")
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
    # mit samples von rwebppl::get_samples machen!
    # p <-  ggplot(data=df_marginal, aes(marginal)) +
    #         geom_density() +
    #         labs(x = paste('P(', vars_str, ')'),  y = "density",
    #              title = distribution_str)
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


# data structures ---------------------------------------------------------
get_target_folder <- function(seed, noise, n_tables){
  noise_str <- as.character(noise) %>% str_replace("\\.", "_")
  fn <- paste("seed-", seed, "-noise-", noise_str, "-tables-", n_tables, sep="")
  return(fn)
}

