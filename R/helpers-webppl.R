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
                   packages=c("./node_modules/conditionalsHelpers")
                  )
  # data is a list of lists
  data <- data %>% map(function(x){as_tibble(x)})
  return(data)
}

structure_model_data <- function(posterior, params){
  posterior_tibbles <- posterior %>% webppl_distrs_to_tibbles()
  if(params$save){save(posterior_tibbles, paste(params$target, ".rds", sep=""))}
  return(posterior_tibbles)
}


listener_beliefs <- function(posterior, level){
  df <- posterior %>% filter(level==(!! level)) %>% mutate(val=prob*val)
  df <- df %>% group_by(cn, intention) %>% spread(key=cell, val=val)
  listener <- df %>% summarize(ac=sum(`AC`), anc=sum(`A-C`), nac=sum(`-AC`), nanc=sum(`-A-C`),
                marginal_cn=sum(prob))
  return(listener)
}











