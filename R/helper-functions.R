save_data <- function(data, target_path){
  data %>% write_rds(target_path)
  print(paste("saved to:", target_path))
}

filter_vars <- function(df_long, vars){
  df <- df_long %>% mutate(keep=TRUE)
  for(var in vars){
    if(str_detect(var, "^-")){
      df <- df %>% mutate(keep=case_when(!keep ~ keep, TRUE ~ str_detect(cell, var)))
    }
    else {
      token <- paste("-", var, sep="")
      df <- df %>% mutate(keep=case_when(!keep ~ keep, TRUE ~ !str_detect(cell, token)))
    }
  }
  return(df)
}

# Probabilities -----------------------------------------------------------
marginalize <- function(data, vars){
  # data must be in long format, such that cell is one column and marginals can
  # be computed for any cell entries, returned object is in wide format
  df <- data %>% filter_vars(vars)
  df <- df %>%  mutate(p=case_when(keep ~ val, TRUE ~ 0)) %>%
          group_by(bn_id, level, intention) %>% mutate(p=sum(p))  %>%
          select(-keep) %>% spread(key=cell, val=val, fill = 0)
        
  return(df)
}

marginal_cns <- function(data_wide){
  data_wide %>% group_by(level, cn, intention) %>% summarize(marginal=sum(prob))
}

# takes the expected value of column *p* with probability in column *prob*
# args: df_wide; tibble with one bn per row, at least columns *p*, *prob*, *level*, *intention*
#       value_str: str describing value, e.g. *P(A)* for expected val of P(A)
expected_val <- function(df_wide, value_str){
  evs <- df_wide %>% mutate(ev_prod=p * prob) %>% group_by(intention, level) %>%
    summarize(ev=sum(ev_prod)) %>% add_column(p=value_str) %>% ungroup()
  
  # fill non-existent levels for plotting
  levels <- evs$level 
  if(is.na(match("prior", levels))){
    evs <- evs %>% add_row(intention="", level="prior", ev=0, p=value_str)}
  if(is.na(match("LL", levels))){
    evs <- evs %>% add_row(intention="", level="LL", ev=0, p=value_str)}
  if(is.na(match("PL", levels))){
    evs <- evs %>% add_row(intention="", level="PL", ev=0, p=value_str)}
  return(evs)
}

compute_cond_prob <- function(distr_wide, prob){
  if(prob=="P(C|A)"){
    distr <- distr_wide %>% mutate(p=`AC`/(`AC`+`A-C`))
  }else if(prob=="P(-C|-A)"){
    distr <- distr_wide %>% mutate(p=`-A-C`/(`-AC`+`-A-C`))
  }else if(prob=="P(A|C)"){
    distr <- distr_wide %>% mutate(p=`AC`/(`-AC`+`AC`))
  }else if(prob=="P(-A|-C)"){
    distr <- distr_wide %>% mutate(p=`-A-C`/(`A-C`+`-A-C`))
  } else if(prob=="P(C|-A)"){
    distr <- distr_wide %>% mutate(p=`-AC`/(`-AC`+`-A-C`))
  }
  else{
    stop("not implemented.")
  }
  return(distr)
}


# model ------------------------------------------------------------------
add_model_params <- function(df, params){
  df <- df %>% mutate(cost=params$cost_conditional,
                      alpha=params$alpha,
                      bias=params$bias,
                      value=as.character(value))
  return(df)
}

filter_by_model_params <- function(df, params){
  df <- df %>% filter(cost==params$cost_conditional &
                      alpha==params$alpha)
  return(df)
}

# other functions ---------------------------------------------------------
hellinger <- function(p, q){
  (1/sqrt(2)) * sqrt(sum((sqrt(p)-sqrt(q))^2))
}

adapt_bn_ids <- function(data_wide){
  # only considers levels PL, LL and prior
  df <- data_wide %>% dplyr::select(-prob, -level, -bn_id, -cn)
  cell_names <- names(df)
  data_wide <- data_wide %>% unite(cells, names(df), sep="__")
  
  # makes sure that bn_ids are identical across levels PL/LL/prior
  prior <- data_wide %>% filter(level=="prior") %>% arrange(cn, cells)
  ll <- data_wide %>% filter(level=="LL") %>% arrange(cn, cells)
  pl <- data_wide %>% filter(level=="PL") %>% arrange(cn, cells)
  df <- bind_rows(ll, prior)
  
  # not all Bayes nets that are in the prior also occur in the literal/pragmatic listener
  # (but there is no diff btw. those in LL/PL)
  idx_dups <- df %>% dplyr::select(-level, -prob, -bn_id) %>% duplicated()
  duplicates_prior <- df[idx_dups, ] %>%  arrange(cn, cells)
  # duplicates_prior$level %>% unique()
  
  pl <- pl %>% mutate(bn_id=duplicates_prior$bn_id)
  ll <- ll %>% mutate(bn_id=duplicates_prior$bn_id)
  
  df <- bind_rows(prior, ll, pl)
  df <- df %>% separate(cells, cell_names, sep="__", convert=TRUE) 
  return(df)
}


# plotting functions ------------------------------------------------------

plot_evs <- function(data){
  p <- data %>% ggplot() +
    geom_bar(mapping = aes(x=level, y=ev, fill=level), stat="identity", position="dodge") +
    labs(x="", y="", title="") +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=12),
          text = element_text(size= 20),
          legend.position = "none") 
  return(p)
}
