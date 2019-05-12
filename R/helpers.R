library(tidyverse)
library(ggplot2)
library(rwebppl)

# HELPERS
webppl_distrs_to_tibbles <- function(data){
  data_tibbles <- data %>%
    map(function(x){
      x %>% transmute(cn=support$cn,
                      table_x=support$table.support,
                      table_probs=support$table.probs,
                      bn_probs=probs) %>% 
        rowid_to_column("bn_id") %>% 
        mutate("bn_id" = as.character(bn_id))
    })
}

marginalize <- function(data, vars){
  for(var in vars){
    if(var %>% startsWith("-")) {
      data <- data %>% filter(stringr::str_detect(table_x, var)) 
    } else {
      var_neg <- paste("-", var, sep="")
      data <- data %>% filter(!stringr::str_detect(table_x, var_neg))
    }  
  } 
  return(data)
}

marginals <- function(df, vars){
  seq(nrow(df)) %>% 
    map(function(i){
      tibble(table_x = df$table_x[[i]], table_probs=df$table_probs[[i]]) %>% 
        marginalize(vars) %>% 
        summarise(marginal=sum(table_probs)) %>% 
        mutate(probs=df$bn_probs[i])
    }) %>% bind_rows()
}


plot_bns <- function(data, distribution_str){
  ggplot(data = data) + 
    geom_bar(mapping = aes(x=bn_id, y=bn_probs), stat="identity") +
    labs(title = distribution_str)
}

plot_cns <- function(data, distribution_str){
  # Causal Nets
  ggplot(data = data) + 
    geom_bar(mapping = aes(x=cn, y=bn_probs), stat="identity") + 
    labs(title = distribution_str, y="probability")
}

plot_bn_table <- function(data, id, distribution_str){
  df <- data %>%  filter(bn_id==id)
  x <- df$table_x[[1]]
  y <- df$table_probs[[1]]
  p <- df$bn_probs
  ggplot(data=tibble(x=x, probs=y)) + 
    geom_bar(mapping = aes(x=x, y=probs), stat="identity") +
    labs(title = paste(distribution_str, ": P(bn", id, "): ", p, " cn: ", df$cn, sep=""),
         y="probability")
}

plot_marginal <- function(data, vars, distribution_str, density_graph = FALSE){
  df_marginal <- data %>%  select(table_x, table_probs, bn_probs) %>%
                  marginals(vars) 
  
  vars_str <- paste(vars, collapse = "")
  if(density_graph){
    # mit samples von rwebppl::get_samples machen!
    p <-  ggplot(data=df_marginal, aes(marginal)) +
            geom_density() +
            labs(x = paste('P(', vars_str, ')'),  y = "density",
                 title = distribution_str)
  }else{
    df_marginal <- df_marginal %>% mutate(marginal=as.character(marginal))
    p <- ggplot(data = df_marginal) +
          geom_bar(mapping = aes(x=marginal, y=probs), stat="identity") +
          labs(x = paste('P(', vars_str, ')'),  y = "probability",
               title = distribution_str)
  }
  p
}

expected_val <- function(data, vars){
  df_marginal <- data %>%  select(table_x, table_probs, bn_probs) %>%
    marginals(vars) 
  return(sum(df_marginal$marginal * df_marginal$probs))
}


