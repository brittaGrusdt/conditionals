library(tidyverse)
library(ggplot2)
library(rwebppl)


# Probabilities -----------------------------------------------------------
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
  df <- df %>% spread(key=cell, val=val, fill=0)
  return(df)
}

expected_val <- function(df_wide, value_str){
  evs <- df_wide %>% mutate(ev_prod=p * prob) %>% group_by(level) %>%
          summarize(ev=sum(ev_prod)) %>% add_column(p=value_str)
  
  levels <- evs$level 
  if(is.na(match("prior", levels))){
    evs <- evs %>% add_row(level="prior", ev=0, p=value_str)}
  if(is.na(match("LL", levels))){
    evs <- evs %>% add_row(level="LL", ev=0, p=value_str)}
  if(is.na(match("PL", levels))){
    evs <- evs %>% add_row(level="PL", ev=0, p=value_str)}
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
            labs(title = causal_net) +
            theme(legend.position = "none")
      print(p)
    }
}


# data structures ---------------------------------------------------------
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
    print("load tables from")
    print(path_tables)
    if(is.na(args$nor_beta)){
      # sample noisy or parameters, filter for specific parameters of resp. distributions
      tables <- tables %>% filter(is.na(nor_beta) & is.na(nor_theta) &
                                  param_nor_beta==args$param_nor_beta &
                                  param_nor_theta==args$param_nor_theta
                                  )
      # if(nrow(tables) == 0){
      #   tables <- ...
      # }
    } else {
      tables <- tables %>% filter(nor_beta==args$nor_beta &
                                  nor_theta==args$nor_theta)
    }
    # if(!is.na(args$param_nor_beta)){
    #   tables <- tables %>% filter(param_nor_beta==args$param_nor_beta &
    #                               param_nor_theta==args$param_nor_theta)
    # }
    
    causal_nets <- read_rds(path_cns)
    utterances <- read_rds(path_utterances)
    target_dir <- file.path(".", "data", "results", df$model_fn,
                            fsep = .Platform$file.sep)
    tables_to_wppl <- tables %>% dplyr::select(ps, vs)
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
  if(params$model == "default"){
    posterior <- posterior %>% 
      map(function(x){
        as_tibble(x) %>% add_column(nor_beta=params$nor_beta,
                                    nor_theta=params$nor_theta,
                                    param_nor_beta=params$param_nor_beta,
                                    param_nor_theta=params$param_nor_theta,
                                    n_tables=params$n_tables,
                                    indep_sigma=params$indep_sigma)
      })
  } else{
    posterior <- posterior %>% map(function(x){
      as_tibble(x)
    })
  }
  
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
    
  } else if(params$level_max=="LL-all-utts"){
    df <- posterior %>%  map(function(x){as_tibble(x)})
    result <- df$LL %>% unnest() %>% rowid_to_column()
    result <- result %>% unnest() %>% rename(cell=table.support, val=table.probs, prob=LL.probs)
  } 
  else{
    result <- structure_model_data(posterior, params)
  }
  return(result)
}

run_model_voi <- function(params){
  # params: list
  posterior <- run_model(params)
  voi <- get_voi(posterior, params)
  return(voi)
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

add_table_params <- function(df, params){
  df <- df %>% mutate(cost=params$cost_conditional, alpha=params$alpha,
                      nor_beta=params$nor_beta,
                      nor_theta=params$nor_theta,
                      param_nor_beta=params$param_nor_beta,
                      param_nor_theta=params$param_nor_theta,
                      indep_sigma=params$indep_sigma,
                      bias=params$bias,
                      value=as.character(value))
  return(df)
}


filter_by_model_params <- function(df, params){
  df <- df %>% filter(cost==params$cost_conditional &
                      alpha==params$alpha &
                      seed==params$seed &
                      bias==params$bias)
  return(df)
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

sample_webppl_distr <- function(data_wide){
  # data_wide needs field prob (Bayes net prob) and p
  samples <- list()
  for(lev in unique(data_wide$level)){
    d <- data_wide %>% filter(level==lev)
    s <- get_samples(tibble(support=d$p, prob=d$prob), 5000000)
    samples[[`lev`]] <- s %>% add_column(level=lev)
  }
  data_marginal <- bind_rows(samples)
  return(data_marginal)
}

# values-of-interest ------------------------------------------------------

# 1. returns the minimum of the hellinger distances between P(cn) and
# a) P(A->C)=0.25, P(-A->-C)=0.25, P(C->A)=0.25, P(-C->-A)=0.25
# b) P(A->-C)=0.25, P(-A->C)=0.25, P(-C->A)=0.25, P(C->-A)=0.25
# and the direction of minimum
get_cp_values_cns <- function(distr_wide){
  p_cns <- distr_wide %>% dplyr::select(prob, bn_id, cn, level)
  # get marginal probabilities for each cn
  marginal <- p_cns %>% group_by(cn, level) %>% summarize(marginal=sum(prob))
  
  # distributions to compare with
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
  
  voi_cns <- marginal %>% group_by(level) %>%
    summarize(cp_cns_ac=hellinger(marginal, marginal_cp1),
              cp_cns_anc=hellinger(marginal, marginal_cp2)) %>% 
      gather(cp_cns_ac, cp_cns_anc, key="key", value="value")
  
  return(voi_cns)
}

# 2. for each Bayes net compute the hellinger distance (h) between the joint
#    distribution of A and C with distribution (a) and distribution (b) 
# with (a): P(A,C)  = 0.5, P(-A,-C) = 0.5 
# and  (b): P(A,-C) = 0.5, P(-A,C)  = 0.5
# compute weighted average of prior probability of Bayes nets weighted by 
# respective hellinger distance:
# a) sum_bn P(bn) * h(bn, a)
# b) sum_bn P(bn) * h(bn, b)
# returns minimum of both evs and direction 
get_cp_values_bns <- function(distr_wide){
  # expected values of corresponding conditional probabilities
  values <- distr_wide %>% group_by(bn_id, level) %>% 
    mutate(hellinger_anc=hellinger(c(`AC`, `A-C`, `-AC`, `-A-C`), 
                                   c(0, 0.5, 0.5, 0)),
           hellinger_ac=hellinger(c(`AC`, `A-C`, `-AC`, `-A-C`), 
                                  c(0.5, 0, 0, 0.5))
    ) 
  values <- values %>% group_by(level) %>%
    summarize(cp_bns_ac=sum(prob*hellinger_ac),
              cp_bns_anc=sum(prob*hellinger_anc))
  voi_bns <- values %>% gather(cp_bns_ac, cp_bns_anc, key="key", value="value")
  
  return(voi_bns)
}


get_cp_values_pnc_given_na <- function(posterior_wide){
  ev_pnc_given_na <- compute_cond_prob(posterior_wide, "P(-C|-A)") %>% 
    expected_val("p_nc_given_na") %>% rename(key=p, value=ev)
  return(ev_pnc_given_na)
}

get_cp_values <- function(distr){
  distr_wide <- distr %>% spread(key = cell, value = val)
  voi_cns <- get_cp_values_cns(distr_wide)
  voi_bns <- get_cp_values_bns(distr_wide)
  voi_pnc_given_na <- get_cp_values_pnc_given_na(distr_wide)
  return(bind_rows(voi_bns, voi_cns, voi_pnc_given_na))
}

# theta <= P(C) <= 1-theta where theta is threshold at which utterances count
# as true
get_speaker_uncertainty <- function(distr, theta){
  pc <- marginalize(distr, c("C"))
  pc_intervals <- pc %>% filter(p>=theta | p<=1-theta)
  
  evs <- pc_intervals %>% group_by(level) %>% summarize(value=sum(prob)) %>% 
          add_column(key="sp-uncertainty")
  return(evs)
}


voi_epistemic_uncertainty <- function(posterior, params){
  val_no_bias <- get_speaker_uncertainty(posterior, params$theta) %>% 
                  mutate(key="epistemic_uncertainty")
  val_no_bias <- add_table_params(val_no_bias, params)
  return(val_no_bias)
}

voi_pc <- function(posterior, params){
  val_biscuits <- marginalize(posterior, c("C")) %>%
    expected_val("C") %>% select(-p) %>%
    rename(value=ev) %>% mutate(key="pc")
  val_biscuits <- add_table_params(val_biscuits, params)
  return(val_biscuits)
}

voi_pa <- function(posterior, params){
  val_pa <- marginalize(posterior, c("A")) %>% 
    expected_val("A") %>% select(-p) %>% 
    rename(value=ev) %>% mutate(key="pa")
  val_pa <- val_pa %>% add_table_params(params)
  return(val_pa)
}

voi_conditional_perfection <- function(posterior, params){
  val_cp <- get_cp_values(posterior) %>% add_table_params(params)
  return(val_cp)
}

voi_skiing <- function(posterior, params){
  pe <- marginalize(posterior, c("E")) 
  ev_pe <- pe %>% expected_val("E") %>% rename(value=ev, key=p) %>% 
    mutate(alpha=params$alpha, cost=params$cost_conditional, pe=params$prior_pe)
  return(ev_pe)
}

voi_sundowners <- function(posterior, params){
  
}

get_voi <- function(posterior, params){
  if(params$model == "default"){
    uncertainty <- voi_epistemic_uncertainty(posterior, params)
    pa <- voi_pa(posterior, params)
    pc <- voi_pc(posterior, params)
    cp <- voi_conditional_perfection(posterior, params)
    
    results <- bind_rows(uncertainty, pa, pc, cp) %>%
                add_column(seed=params$seed,
                           n_tables=params$n_tables)
  } else if(params$model == "skiing"){
    results <- voi_skiing(posterior, params)
  } else if(params$model == "sundowners"){
    # TODO
    # results <- voi_sundowners(posterior, params)
  }else{
    stop(paste("unknown model:", params$model))
  }
  
  name <- file.path(params$target_dir, params$target_fn, fsep=.Platform$file.sep)
  if(params$save_voi){
    results %>% save(paste(name, "-voi.rds", sep=""))
  }
  return(results)
}

