webppl_distrs_to_tibbles <- function(posterior){
  posterior_tibbles <- map2(posterior, names(posterior), function(x, y){
    x <- x %>% rowid_to_column("bn_id") 
    bn_probs <- x %>% dplyr::select("probs", "bn_id")
    data_tibble <- x$support %>% rowid_to_column("bn_id") %>% 
                    unnest(cols = c(bn.table.probs, bn.table.support)) %>%
                    as_tibble() %>% 
                    left_join(bn_probs, by = "bn_id") %>% 
                    mutate("bn_id" = as.character(bn_id)) %>%
                    add_column(level=y) %>% 
                    rename(prob=probs, val=bn.table.probs, cell=bn.table.support, cn=bn.cn)
    return(data_tibble)             
  })
  df <- bind_rows(posterior_tibbles)
  return(df)
}

structure_bns <- function(posterior, params){
  data.long <- posterior$bns %>% rowid_to_column(var = "bn_id") %>%
    unnest(c(table.probs, table.support)) %>%
    rename(val=table.probs, cell=table.support) %>%
    add_column(bias=params$bias, level=params$level_max)

  if(params$add_accept_conditions){
    df_wide <- data.long %>% spread(key=cell, val=val)
    df <- acceptability_conditions(df_wide)
    data.long <- df %>% group_by(bn_id, cn, level) %>%
      pivot_longer(cols = c(`AC`, `A-C`, `-AC`, `-A-C`), #c(-bn_id, -cn, -level, -bias, -p_delta, -p_rooij, -p_diff),
                   names_to = "cell", values_to = "val")
  }
  if(params$save){
    data.long %>% save_data(params$target)
    params %>% save_data(params$target_params)
  }
  return(data.long)
}

run_webppl <- function(path_wppl_file, params){
  if(params$verbose){
    print(paste('model file read from:', path_wppl_file))
    print(paste('packages loaded from:' ,params$packages))
  }
  data <-   webppl(program_file = path_wppl_file,
                   data = params,
                   data_var = "data",
                   random_seed = params$seed_webppl,
                   packages=params$packages
                  )
  # data is a list of lists
  data <- data %>% map(function(x){as_tibble(x)})
  return(data)
}

structure_listener_data <- function(posterior, params){
  df_long <- posterior %>% webppl_distrs_to_tibbles() %>%
              mutate(val=case_when(val<0.000001 ~ 0.000001, TRUE ~ val)) %>% 
              add_column(bias=params$bias)
  if(params$add_accept_conditions){
    df_wide <- df_long %>% spread(key=cell, val=val) %>%
      mutate(`-A-C` = case_when(is.na(`-A-C`) ~ rowSums(select(., starts_with("-A-C_"))),
                                TRUE ~ `-A-C`),
             `-AC` = case_when(is.na(`-AC`) ~ rowSums(select(., starts_with("-AC_"))),
                               TRUE ~ `-AC`),
             `A-C` = case_when(is.na(`A-C`) ~ rowSums(select(., starts_with("A-C_"))),
                               TRUE ~ `A-C`),
             `AC` = case_when(is.na(`AC`) ~ rowSums(select(., starts_with("AC_"))),
                              TRUE ~ `AC`)
    )
    df <- acceptability_conditions(df_wide)
    df_long <- df %>% group_by(bn_id, cn, level) %>%
      pivot_longer(cols=c(AC, `A-C`, `-AC`, `-A-C`),
                   names_to="cell", values_to="val")
  }
  
  if(params$save){
    df_long %>% save_data(params$target)
    params %>% save_data(params$target_params)
  }
  return(df_long)
}


# summarise webppl distributions ------------------------------------------
# @arg posterior: in long format, must have columns *cell* and *val*
listener_beliefs <- function(posterior, level, params, vars_condition_on=NA){
  df <- posterior %>% filter(level==(!! level)) %>% mutate(ev=prob*val)
  if(!is.na(vars_condition_on)){
    listener <- df %>% filter_vars(vars_condition_on) %>%  filter(keep) %>%
      select(-keep)
  } 
  listener <- df %>% group_by(cell) %>% mutate(ev=sum(ev))
    
  listener <- listener %>% mutate(ev=sum(ev)) %>%
    summarise(ev=sum(val), marginal=sum(prob), .groups="keep")
  if(params$save){listener %>% 
      save_data(paste(str_sub(params$target, 1, -5), "-listener-beliefs-world.rds", sep=""))
  }
  return(listener)
}


webppl_speaker_distrs_to_tibbles <- function(posterior){
  speaker <- posterior[names(posterior) != "bns"] 
  posterior_tibbles <- map2(speaker, names(speaker), function(x, y){
    data_tibble <- x %>% rowid_to_column("bn_id") %>% unnest(cols = c(probs, support)) %>% 
      rename(utterance=support) %>% 
      add_column(level=y) %>% separate(level, sep="_", into=c("level", "intention"))
    return(data_tibble)             
  })
  speaker <- bind_rows(posterior_tibbles) 
  bns_unique <- posterior$bns %>% rowid_to_column("bn_id") %>%
    unnest(cols = c(table.probs, table.support)) %>% 
    rename(cell=table.support, val=table.probs) %>%
    spread(key=cell, val=val) %>%
    # mutate(AC=as.double(AC), `A-C`=as.double(`A-C`), `-AC`=as.double(`-AC`), `-A-C`=as.double(`-A-C`)) %>% 
    mutate(`-A-C` = case_when(is.na(`-A-C`) ~ rowSums(select(., starts_with("-A-C_"))),
                              TRUE ~ `-A-C`),
           `-AC` = case_when(is.na(`-AC`) ~ rowSums(select(., starts_with("-AC_"))),
                             TRUE ~ `-AC`),
           `A-C` = case_when(is.na(`A-C`) ~ rowSums(select(., starts_with("A-C_"))),
                             TRUE ~ `A-C`),
           `AC` = case_when(is.na(`AC`) ~ rowSums(select(., starts_with("AC_"))),
                            TRUE ~ `AC`)
           ) %>%
    nest(data = c(cn, `-A-C`, `-AC`, `A-C`, `AC`))

  # here we only keep AC, A-C, -AC, -A-C cell entries!
  bns <- bns_unique[speaker$bn_id,]$data
  speaker_wide <- speaker %>% add_column(bn=bns) %>% unnest(cols = c(bn)) %>%
    spread(key=utterance, val=probs, fill=0)
  
  return(speaker_wide)
}


structure_speaker_data <- function(posterior, params){
  speaker_wide <- webppl_speaker_distrs_to_tibbles(posterior)
  bns = posterior$bns %>% rowid_to_column("bn_id") %>% group_by(bn_id) %>%
    unnest(c(table.probs, table.support)) %>%
    pivot_wider(names_from=table.support, values_from=table.probs) %>%
    rename(stimulus_id=id) %>% select(-cn);
  df.wide = left_join(speaker_wide %>% select(-`AC`, -`A-C`, -`-AC`, -`-A-C`), bns, by = "bn_id")
  df <- acceptability_conditions(df.wide)
  df <- df %>% group_by(bn_id, stimulus_id) %>% 
    pivot_longer(cols=c(-bn_id, -intention, -stimulus_id, -starts_with("cell."), 
                        -p_delta, -p_rooij, -p_diff,
                        -level, -cn, -`AC`, -`A-C`, -`-AC`, -`-A-C`),
                  names_to = "utterance", values_to = "probs") %>%
        add_column(bias=params$bias)
  
  if(params$speaker_intents == "") df <- df %>% select(-intention)
  if(params$save){
    df %>% save_data(params$target)
    params %>% save_data(params$target_params)
  }
  return(df)
}

average_speaker <- function(distrs, params){
  # @distrs: long format with columns: utterance, [intention]
  if(params$speaker_intents != ""){
    data <- distrs %>% group_by(utterance, intention)
    data.cns <- distrs %>% group_by(utterance, intention, cn)
  } else {
    data <- distrs %>% group_by(utterance)
    data.cns <- distrs %>% group_by(utterance, cn)
  }
  df <- data %>% summarise(avg=mean(probs)) %>% add_column(bias=params$bias)
  df_cns <- data.cns %>% summarise(avg=mean(probs), .groups="keep") %>%
    add_column(bias=params$bias)

  if(params$save){
    fn <- str_split(params$target, ".rds")
    df %>% save_data(paste(fn[[1]][1], "-avg.rds", sep=""))
    df_cns %>% save_data(paste(fn[[1]][1], "-avg-cns.rds", sep=""))
  }
  return(df)
}
