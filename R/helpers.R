library(tidyverse)
library(ggplot2)
library(rwebppl)


# Probabilities -----------------------------------------------------------
webppl_distrs_to_tibbles <- function(posterior){
  posterior_tibbles <- map2(posterior, names(posterior), function(x, y){
                        x <- x %>% rowid_to_column("bn_id") 
                        bn_probs <- x %>% select("probs", "bn_id")
                        data_tibble <- x$support %>% rowid_to_column("bn_id") %>% 
                          unnest() %>%
                          as_tibble() %>% 
                          left_join(bn_probs, by = "bn_id") %>% 
                          mutate("bn_id" = as.character(bn_id)) %>%
                          add_column(level=y) %>% 
                          rename(prob=probs, val=table.probs, cell=table.support)
                        return(data_tibble)             
                      })
  
  return(bind_rows(posterior_tibbles))
}
  
marginalize <- function(data, vars){
  # data must be in long format, such that cell is one column and marginals can
  # be computed for any cell entries
  for(var in vars){
    if(str_detect(var, "^-")){
      data <- data %>% filter(str_detect(cell, var))
    } else {
      token <- paste("-", var, sep="")
      data <- data %>% filter(!str_detect(cell, token))
    }
  }
  df <- data %>% group_by(bn_id, level) %>% mutate(p = sum(val))
  # now data must be wide again, each bn_id should appear only once per level
  df <- df %>% spread(key=cell, val=val)
  return(df)
}

expected_val <- function(df_wide, value_str){
  evs <- df_wide %>% mutate(ev_prod=p * prob) %>% group_by(level) %>%
          summarize(ev=sum(ev_prod)) %>% add_column(p=value_str)
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
  }else{
    stop("not implemented.")
  }
  return(distr)
}


# ev_conditional_prob <- function(distr_wide, prob){
#   df <- compute_cond_prob(distr_wide, prob)
#   sum(df$bn_probs * df$cond)
# }

hellinger <- function(p, q){
  (1/sqrt(2)) * sqrt(sum((sqrt(p)-sqrt(q))^2))
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
    p <-  ggplot(data=df_marginal, aes(x=p, color=p)) +
            geom_freqpoly(bins=10) +
            labs(x = paste('P(', vars_str, ')'),  y = "density",
                 title = distribution_str)
  }else{
    marginals <- df_marginal %>% spread(key = cell, value = val) %>%
                 mutate(p=as.character(p))
    p <- ggplot(data = marginals) +
          geom_bar(mapping = aes(x=p, y=prob), stat="identity") +
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
    tables_to_wppl <- NULL
  } else {
    fn_utts <- paste("utterances-", df$bias, ".rds", sep="")
    data_dir <- file.path(".", "data", "precomputations", df$model_fn,
                          fsep = .Platform$file.sep)
    
    path_tables <- file.path(data_dir, "tables-all.rds", fsep = .Platform$file.sep)
    path_cns <- file.path(data_dir, "cns.rds", fsep = .Platform$file.sep)
    path_utterances <- file.path(data_dir, fn_utts, fsep = .Platform$file.sep)
    
    tables <- read_rds(path_tables) %>% filter(n_tables==args$n_tables_per_cn &
                                                 noise_v==args$noise_v)
    if(is.na(args$noisy_or_beta)){
      tables <- tables %>% filter(is.na(noisy_or_beta) & is.na(noisy_or_theta))
    } else {
      tables <- tables %>% filter(noisy_or_beta==args$noisy_or_beta & 
                                  noisy_or_theta==args$noisy_or_theta)
    }
    if(!is.na(args$param_nor_beta)){
      tables <- tables %>% filter(param_nor_beta==args$param_nor_beta &
                                  param_nor_theta==args$param_nor_theta)
    }
    
    
    causal_nets <- read_rds(path_cns)
    utterances <- read_rds(path_utterances)
    target_dir <- file.path(".", "data", "results", df$model_fn,
                            fsep = .Platform$file.sep)
    tables_to_wppl <- tables %>% select(ps, vs)
  }
  
  # Target files
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

  # Model params
  model_params <- list(utt=df$utterance,
                       bias=df$bias,
                       tables=tables_to_wppl,
                       utterances=utterances,
                       cns=causal_nets,
                       target_dir=target_dir,
                       target_fn=df$save_as,
                       model_path=model_path)
  params <- c(args, model_params)
  return(params)
}

run_webppl <- function(params){
  # Run and save model ------------------------------------------------------
  posterior <- webppl(program_file = params$model_path,
                      data = params,
                      data_var = "data",
                      random_seed = params$seed)
  return(posterior)
}

structure_model_data <- function(posterior, params){
  posterior <- posterior %>% 
    map(function(x){
      as_tibble(x) %>% add_column(noisy_or_beta=params$noisy_or_beta,
                                  noisy_or_theta=params$noisy_or_theta,
                                  n_tables=params$n_tables_per_cn,
                                  noise_v=params$noise_v)
    })
  
  posterior_tibbles <- posterior %>% webppl_distrs_to_tibbles()
  
  if(params$save){
    target_path <- file.path(params$target_dir,
                             paste(params$target_fn, ".rds", sep=""),
                             fsep = .Platform$file.sep)
    
    save(posterior_tibbles, target_path)
  }
  return(posterior_tibbles)
}

run_model <- function(params){
  posterior <- run_webppl(params)
  
  if(params$level_max=="speaker_all_bns"){
    df <- posterior %>% map(function(x){as_tibble(x)})
    df <- df$speaker %>% unnest() 
    result <- df %>% group_by(support) %>% summarize(p_mean=mean(probs)) %>%
      filter(support==params$utt) %>% pull(p_mean)
    
  } else if(params$level_max=="logLik"){
    df <- posterior %>% map(function(x){as_tibble(x)})
    result <- df$logLik %>% unnest() 
    
  } else{
    result <- structure_model_data(posterior, params)
  }
  return(result)
}


# Data transformations -----------------------------------------------------
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

# not needed anymore!?
join_model_levels <- function(data){
  data_prior <- data$prior %>% add_column(level="prior")
  data_ll <- data$LL %>% add_column(level="LL")
  data_pl <- data$PL %>% add_column(level="PL")
  predictions <- bind_rows(data_prior, data_ll, data_pl)
  predictions <- predictions %>% spread(key=cell, val=val) %>%
    group_by(bn_id, level)
  return(predictions)
}


# values-of-interest ------------------------------------------------------

# theta <= P(C) <= 1-theta where theta is threshold at which utterances count as true
get_speaker_uncertainty <- function(distr, threshold){
  pc <- marginalize(distr, c("C"))
  pc_intervals <- pc %>% filter(p>=threshold |
                                p<=1-threshold)
  
  evs <- pc_intervals %>% group_by(level) %>% summarize(value=sum(prob)) %>% 
          add_column(key="sp-uncertainty")
  return(evs)
}




get_cp_values <- function(distr){
  # Expected value of the minimum of the hellinger distances
  # a: 0 1  and b: 1 0
  #    1 0         0 1
  # and each Bayes net: f(bn) = min(hellinger(a, bn), helling(b, bn)) weighted
  # by P(bn)
  distr_wide <- distr %>% spread(key = cell, value = val)
  # causal nets
  p_cns <- distr_wide %>% select(prob, bn_id, cn, level)
  marginal <- p_cns %>% group_by(cn, level) %>% summarize(marginal=sum(prob))
  
  marginal <- marginal %>%
                mutate(marginal_cp1=case_when(cn=="A implies C" ~ 0.25,
                                              cn=="-A implies -C" ~ 0.25,
                                              cn=="C implies A" ~ 0.25,
                                              cn=="-C implies -A" ~ 0.25,
                                              TRUE ~ 0
                                              ), 
                       marginal_cp2=case_when(cn=="A implies -C" ~ 0.25,
                                              cn=="-A implies C" ~ 0.25,
                                              cn=="C implies -A" ~ 0.25,
                                              cn=="-C implies A" ~ 0.25,
                                              TRUE ~ 0
                       ))
  
  values <- marginal %>% group_by(level) %>%
              summarize(hellinger_ac=hellinger(marginal, marginal_cp1),
                        hellinger_anc=hellinger(marginal, marginal_cp2),
                        hel_min_val=min(hellinger_ac, hellinger_anc),
                        hel_min_dir=case_when(hellinger_ac == hellinger_anc ~ "eq",
                                              hellinger_ac < hellinger_anc ~ "AC",
                                              TRUE ~ "A-C")) %>% 
              unite("dir_val", hel_min_dir, hel_min_val)
  voi_cns <- values %>% select(level, dir_val) %>%
              rename(value=dir_val) %>% add_column(key="cp-cns")
  
  
  # expected values of corresponding conditional probabilities
  values <- distr_wide %>% group_by(bn_id, level) %>% 
    mutate(hellinger_anc=hellinger(c(`AC`, `A-C`, `-AC`, `-A-C`), 
                                   c(0, 0.5, 0.5, 0)),
           hellinger_ac=hellinger(c(`AC`, `A-C`, `-AC`, `-A-C`), 
                                  c(0.5, 0, 0, 0.5))
    ) 
  values <- values %>% group_by(level) %>%
              summarize(ev_hel_ac=sum(prob*hellinger_ac),
                        ev_hel_anc=sum(prob*hellinger_anc))
  
  voi_bns <- values %>%
              mutate(dir=case_when(ev_hel_ac == ev_hel_anc ~ "eq",
                                   ev_hel_ac < ev_hel_anc ~ "AC",
                                   TRUE ~ "A-C"),
                     val=case_when(ev_hel_ac < ev_hel_anc ~ ev_hel_ac,
                                   TRUE ~ ev_hel_anc)) %>%
              unite("dir_val", dir, val) %>%
              rename(value=dir_val) %>%
              select(level, value) %>% add_column(key="cp-bns") 
  
  return(bind_rows(voi_bns, voi_cns))
}

