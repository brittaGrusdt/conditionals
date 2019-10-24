webppl_distrs_to_tibbles <- function(posterior){
  posterior_tibbles <- map2(posterior, names(posterior), function(x, y){
    x <- x %>% rowid_to_column("bn_id") 
    bn_probs <- x %>% dplyr::select("probs", "bn_id")
    data_tibble <- x$support %>% rowid_to_column("bn_id") %>% 
      unnest() %>%
      as_tibble() %>% 
      left_join(bn_probs, by = "bn_id") %>% 
      mutate("bn_id" = as.character(bn_id)) %>%
      add_column(level=y) %>% 
      rename(prob=probs, val=bn.table.probs, cell=bn.table.support,
             cn=bn.cn)
    return(data_tibble)             
  })
  
  return(bind_rows(posterior_tibbles))
}

run_webppl <- function(path_wppl_file, params){
  data <-   webppl(program_file = path_wppl_file,
                   data = params,
                   data_var = "data",
                   random_seed = params$seed,
                   packages=params$packages
                  )
  # data is a list of lists
  data <- data %>% map(function(x){as_tibble(x)})
  return(data)
}

structure_model_data <- function(posterior, params){
  posterior_tibbles <- posterior %>% webppl_distrs_to_tibbles() %>%
    mutate(val=case_when(val<0.000001 ~ 0.000001, TRUE ~ val))
  fn <- paste(params$target, ".rds", sep="")
  if(params$save){save_data(posterior_tibbles, fn)}
  return(posterior_tibbles)
}


# Summarize webppl distributions ------------------------------------------
listener_beliefs <- function(posterior, level, params, vars_condition_on=NA){
  df <- posterior %>% filter(level==(!! level)) %>% mutate(val=prob*val)
  listener <- df %>% group_by(cn, intention, cell) %>% summarize(val=sum(val), marginal_cn_int=sum(prob))
  # df <- df %>% group_by(cn, intention, cell) %>% spread(key=cell, val=val)
  # listener <- df %>% summarize(ac=sum(`AC`), anc=sum(`A-C`), nac=sum(`-AC`), nanc=sum(`-A-C`),
  #               marginal_cn=sum(prob))
  
  if(!is.na(vars_condition_on)){
    listener <- listener %>% filter_vars(vars_condition_on) %>%  filter(keep) %>% mutate(val=val/sum(val))
  }
  if(params$save){listener %>%
      save_data(paste(params$target, "-listener-beliefs-world.rds", sep=""))}
  
  return(listener)
}


webppl_speaker_distrs_to_tibbles <- function(posterior){
  posterior_tibbles <- map2(posterior, names(posterior), function(x, y){
    data_tibble <- x %>% rowid_to_column("bn_id") %>% unnest() %>% 
      rename(utterance=support) %>% 
      add_column(level=y) %>% separate(level, sep="_", into=c("level", "intention"))
    return(data_tibble)             
  })
  
  return(bind_rows(posterior_tibbles))
}

average_speaker <- function(distrs, params){
  df <- distrs %>% group_by(utterance, intention) %>%
    summarize(mean_per_intention=mean(probs))
  if(params$save){df %>% save_data(paste(params$target, "-avg-speaker.rds", sep=""))}
  return(df)
}











