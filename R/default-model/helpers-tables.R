library(truncnorm)
library(dplyr)

SEED <- 1234
set.seed(SEED)
BASE_DIR <- getwd()
cns_path <- file.path(paste(BASE_DIR, "/data/default-model/cns-default.rds", sep=""), fsep = .Platform$file.sep)
cns <- readRDS(cns_path)
CNS_DEP <- cns[cns != "A || C"]

# Table Generation --------------------------------------------------------
create_me_tables <-function(params) {
  cn <- "A,B->D->C"
  get_pd_given_ab <- function(a, b, d, n=1){
    val <- ifelse(xor(a, b), ifelse(a==1, "A", "B"), NA_character_)
    if(is.na(val)){
      p <- ifelse(is.na(d), 1, 0)
    } else {
      p <- ifelse(val == d, 1, 0);
    }
    return(rep(p, n))
  }
  get_pc_given_d = function(d, c=1, n=1){
    if(is.na(d)) {
      val <- ifelse(c==1, 0, 1)
      p <- rep(val, n)
    } else {
      p <- rbeta(n, 10, 1)
    }
    return(p)
  }

  pa = rbeta(params$n_tables, 1, 1)
  pb = rbeta(params$n_tables, 1, 1)
  pc_given_dA = get_pc_given_d("A", 1, params$n_tables)
  pc_given_dB = get_pc_given_d("B", 1, params$n_tables)
  pc_given_dNA = get_pc_given_d(NA, 1, params$n_tables)
  pna = 1-pa
  pnb = 1-pb
  pnc_given_dA = 1 - pc_given_dA
  pnc_given_dB = 1 - pc_given_dB
  pnc_given_dNA = 1- pc_given_dNA

  tables <- tibble(`AC_Da`=pc_given_dA * get_pd_given_ab(1, 0, "A", params$n_tables) * pa * (1-pb),
                   `A-C_Da`=pnc_given_dA * get_pd_given_ab(1, 0, "A", params$n_tables) * pa * (1-pb),

                   `AC_Dna`=rep(0, params$n_tables),
                   `A-C_Dna`=pnc_given_dNA * get_pd_given_ab(1, 1, NA, params$n_tables) * pa * pb,
                   `-AC_Dna`=rep(0, params$n_tables),
                   `-A-C_Dna`=pnc_given_dNA * get_pd_given_ab(0, 0, NA, params$n_tables) * (1-pa) * (1-pb),

                   `-AC_Db`=pc_given_dB * get_pd_given_ab(0, 1, "B", params$n_tables) * (1-pa) * pb,
                   `-A-C_Db`=pnc_given_dB * get_pd_given_ab(0, 1, "B", params$n_tables) * (1-pa) * pb,

                   `AC_Db`=rep(0, params$n_tables),
                   `A-C_Db`=rep(0, params$n_tables),
                   `-AC_Da`=rep(0, params$n_tables),
                   `-A-C_Da`=rep(0, params$n_tables)

                   ) %>% rowid_to_column("id")

  tables_long <- tables %>%
    pivot_longer(c(-id), names_to = "cell", values_to = "val") %>% group_by(id) %>%
    mutate(val=round(val, 4))
    # summarize(s=sum(val)) # must sum to 1
  tables_wide <- tables_long %>%
    summarise(ps = list(val), vs=list(cell), .groups = 'drop') %>%
    add_column(cn=(!! cn)) %>% dplyr::select(-id)

  return(tables_wide)
}

create_ab_tables <-function(params, cn) {
  if(cn == "A,B->D->C") {
  # here we need B in support instead of D (as in me_tables - function)
    tables = create_me_tables(params) %>% rowid_to_column("bn_id") %>%
      unnest(cols = c(ps, vs)) %>%
      mutate(vs = case_when(vs ==  "AC_Dna" ~ "AC_B",
                            vs ==  "-A-C_Dna" ~ "-A-C_-B",
                            vs ==  "A-C_Dna" ~ "A-C_B",
                            vs ==  "-AC_Dna" ~ "-AC_-B",
   
                            vs ==  "AC_Da" ~ "AC_-B",
                            vs ==  "-A-C_Da" ~ "impossible1", # 0 probability
                            vs ==  "A-C_Da" ~ "A-C_-B",
                            vs ==  "-AC_Da" ~ "impossible2", # 0

                            vs ==  "AC_Db" ~ "impossible3", #0
                            vs ==  "-A-C_Db" ~ "-A-C_B", 
                            vs ==  "A-C_Db" ~ "impossible4", # 0
                            vs ==  "-AC_Db" ~ "-AC_B",
                            TRUE ~ NA_character_)) %>% group_by(bn_id) %>% 
      pivot_wider(names_from = vs, values_from = ps) %>%
      select(-impossible1, -impossible2, -impossible3, -impossible4) %>% 
      ungroup() %>% select(-bn_id)
  } else if(cn == "A,B->C"){
      pa = rbeta(params$n_tables, 1, 1)
      pb = rbeta(params$n_tables, 1, 1)
      pc_given_ab = rbeta(params$n_tables, 10, 1)
      pc_given_anb = rbeta(params$n_tables, 10, 1)
      pc_given_nab = rbeta(params$n_tables, 10, 1)
      pc_given_nanb = rep(0, params$n_tables)
      pna = 1-pa
      pnb = 1-pb
      pnc_given_ab = 1 - pc_given_ab
      pnc_given_anb = 1 - pc_given_anb
      pnc_given_nab = 1- pc_given_nab
      pnc_given_nanb = 1- pc_given_nanb

      tables <- tibble(`AC_B` = pc_given_ab * pa * pb,
                       `AC_-B` = pc_given_anb * pa * (1-pb),
                       `A-C_B` = pnc_given_ab * pa * pb,
                       `A-C_-B` = pnc_given_anb * pa * (1-pb),
                       `-AC_B` = pc_given_nab * (1-pa) * pb,
                       `-AC_-B` = rep(0, params$n_tables),
                       `-A-C_B` = pnc_given_nab * (1-pa) * pb,
                       `-A-C_-B` = pnc_given_nanb * (1-pa) * (1-pb)
                       ) %>% add_column(cn=(!! cn))

  }
  tables_long <- tables %>% rowid_to_column("bn_id") %>%
    pivot_longer(c(-bn_id, -cn), names_to = "cell", values_to = "val") %>% group_by(bn_id) %>%
    mutate(val=round(val, 4))
  # summarize(s=sum(val)) # must sum to 1
  tables_wide <- tables_long %>%
    summarise(ps = list(val), vs=list(cell), .groups = 'drop') %>%
    add_column(cn=(!! cn)) %>% dplyr::select(-bn_id)
  return(tables_wide)
}



create_dependent_tables <- function(params, cns){
  all_tables <- list()
  idx <- 1
  for(cn in cns){
    if(cn ==  "A,B->D->C") {
      if(params$bias == "lawn"){
        tables_wide <- create_ab_tables(params, cn)
      } else {
        tables_wide <- create_me_tables(params)
      }
    } else if(cn == "A,B->C") {
      tables_wide <- create_ab_tables(params, cn)
    } else {
      theta <- rbeta(params$n_tables, 10, 1)
      if(cn == "only A implies C") {
        beta <- rep(0, params$n_tables) 
      } else {
        beta <- rbeta(params$n_tables, 1, 10)
      }
      p_child_parent <- theta + beta * (1 - theta)
      p_child_neg_parent <- beta
      p_parent <- runif(params$n_tables)

      if(cn == "A implies C" || cn == "C implies A" || cn == "only A implies C"){
        probs <- tibble(cond1=p_child_parent, cond2=p_child_neg_parent, marginal=p_parent)
      } else if(cn=="A implies -C" || cn=="C implies -A"){
        probs <- tibble(cond1=1-p_child_parent, cond2=1-p_child_neg_parent, marginal=p_parent)
      } else if(cn=="-A implies C" || cn=="-C implies A"){
        probs <- tibble(cond1=p_child_neg_parent, cond2=p_child_parent, marginal=1-p_parent)
      } else if(cn=="-A implies -C" || cn=="-C implies -A"){
        probs <- tibble(cond1=1-p_child_neg_parent, cond2=1-p_child_parent, marginal=1-p_parent)
      }

      # A -> C and -A -> C use the same probabilities (P(C|A), P(C|-A), P(A)/P(-A))
      if(startsWith(cn, "A") || startsWith(cn, "-A") || startsWith(cn, "only A")){
        probs <- probs %>% mutate(`AC`=cond1 * marginal,
                                  `A-C`=(1-cond1) * marginal,
                                  `-AC`=cond2 * (1-marginal),
                                  `-A-C`=(1-cond2) * (1-marginal))
      } else if(startsWith(cn, "C") || startsWith(cn, "-C")){
        # diagonals are switched
        probs <- probs %>% mutate(`AC`=cond1 * marginal,
                                  `A-C`=cond2 * (1-marginal),
                                  `-AC`=(1-cond1) * marginal,
                                  `-A-C`=(1-cond2) * (1-marginal))
      } else {
        stop(paste(cn, "not implemented."))
      }
      tables <- probs %>% dplyr::select(-cond1, -cond2, -marginal) %>%
        rowid_to_column("id")
      tables_long <- tables %>%
        gather(`AC`, `A-C`, `-AC`, `-A-C`, key="cell", val="val") %>%
        mutate(val=round(val, 4))
      tables_wide <- tables_long %>% group_by(id) %>%
        summarise(ps = list(val), .groups = 'drop') %>% add_column(cn=(!! cn)) %>%
        mutate(vs=list(c("AC", "A-C", "-AC", "-A-C"))) %>% dplyr::select(-id)
    }
    
    all_tables[[idx]] <- tables_wide
    idx <- idx + 1
  }
  tables <- all_tables %>% bind_rows()
  return(tables)
}

create_independent_tables <- function(params){
  tables <- tibble(pc=runif(params$n_tables), pa=runif(params$n_tables)) %>% rowid_to_column("id") %>%
    group_by(id) %>% mutate(min=min(pa, pc),
                            `AC`=rtruncnorm(1, a=0, b=min, mean=pc*pa, sd=params$indep_sigma),
                            `-AC`=pc-`AC`,
                            `A-C`=pa-`AC`,
                            s=sum(`AC`, `-AC`, `A-C`))
  cell_nanc <- case_when(tables$s >= 1 ~ 0,
                         TRUE ~ 1-(tables$`AC` + tables$`-AC` + tables$`A-C`))
  
  tables <- tables %>% add_column(`-A-C`=cell_nanc) %>% mutate(s=sum(`AC`, `-AC`, `A-C`, `-A-C`))
  
  tables_long <- tables %>% gather(`AC`, `A-C`, `-AC`, `-A-C`, key="cell", val="val") %>% 
                  mutate(val=round(val, 4))
  tables_wide <- tables_long %>% group_by(id) %>%
    summarise(ps = list(val), .groups = 'drop') %>% add_column(cn="A || C") %>% 
    mutate(vs=list(c("AC", "A-C", "-AC", "-A-C"))) %>% dplyr::select(-id)
  
  return(tables_wide)
}

create_tables <- function(params, target_path, cns=CNS_DEP){
  cns_dep=cns[cns != "A || C"]
  tables_all <- list()
  if(params$bias == "dutchman" || params$bias == "pizza"){
    tables_ind <- create_independent_tables(params)
    tables_dep <- tibble()
  } else {
      tables_ind <- create_independent_tables(params)
      tables_dep <- create_dependent_tables(params, cns_dep)
  }
  tables <- bind_rows(tables_ind, tables_dep) %>% rowid_to_column("id") %>% 
              mutate(nor_theta=params$nor_theta, nor_beta=params$nor_beta,
                     indep_sigma=params$indep_sigma,
                     n_tables=params$n_tables,
                     seed=params$seed
              )
  tables %>% save_data(target_path)
  return(tables)
}

filter_tables <- function(tables, params){
  if(is.na(params$nor_beta)){
    df <- tables %>% filter(is.na(nor_beta) & is.na(nor_theta))
  } else{
    df <- tables %>% filter(nor_beta == params$nor_beta & nor_theta == params$nor_theta)
  }
  df <- df %>% filter(n_tables == params$n_tables & indep_sigma == params$indep_sigma)
  return(df)
}

unnest_tables <- function(tables){
  tables <- tables %>% rowid_to_column()
  tables_long <- tables %>% unnest(cols=c(vs, ps)) %>% rename(cell=vs, val=ps)
  return(tables_long)
}

# TODO: compare with same function in helper-functions.R!
adapt_bn_ids <- function(data_wide){
  # makes sure that bn_ids are identical across levels PL/LL/prior
  prior <- data_wide %>% filter(level=="prior") %>% arrange(cn, `AC`, `-AC`, `A-C`, `-A-C`)
  ll <- data_wide %>% filter(level=="LL") %>% arrange(cn, `AC`, `-AC`, `A-C`, `-A-C`)
  pl <- data_wide %>% filter(level=="PL") %>% arrange(cn, `AC`, `-AC`, `A-C`, `-A-C`)
  df <- bind_rows(ll, prior)
  
  # not all Bayes nets that are in the prior also occur in the literal/pragmatic listener
  # (but there is no diff btw. those in LL/PL)
  idx_dups <- df %>% dplyr::select(-level, -prob, -bn_id) %>% duplicated()
  duplicates_prior <- df[idx_dups, ] %>%  arrange(cn, `AC`, `AC`, `A-C`, `-AC`, `-A-C`)
  duplicates_prior$level %>% unique()
  
  # pl <- pl %>% mutate(bn_id=duplicates_prior$bn_id)
  ll <- ll %>% mutate(bn_id=duplicates_prior$bn_id)
  
  df <- bind_rows(prior, ll, pl)
  return(df)
}


plot_tables <- function(data){
  # data must be in long format with columns *cell* and *val*
  cns <- data$cn %>% as.factor() %>% levels()
  plots <- list(); idx = 1
  for(causal_net in cns){
    if(causal_net == "A || C"){
      cn_title <- "A independent C"
    } else {
      cn_title <- causal_net
    }
    p <- data %>% 
      filter(cn==causal_net) %>%
      ggplot(aes(x=val,  color = cell)) +
      geom_density() +
      facet_wrap(~cell, ncol = 2, scales = "free",
                 labeller = labeller(cell = c(`AC` = "A ∧ C", `A-C` = "A ∧ ¬C",
                                              `-AC`= "¬A ∧ C", `-A-C` = "¬A ∧ ¬C"))
                 ) +
      labs(title = cn_title, x="probability") +
      theme_classic(base_size = 20) +
      theme(legend.position = "none", axis.text.x = element_text(size=10))
    plots[[idx]] <- p
    idx <- idx + 1
    print(p)
  }
  return(plots)
}

# analysis generated tables
# tables_data <- marginalize(tables_long %>% rename(level=cn), c("A")) %>%
#   mutate(p=case_when(p==0 ~ 0.00001, TRUE~p), `P(C|A)`=AC/p)
# tables_data %>% group_by(level) %>% summarise(m=mean(`P(C|A)`))

# df <- tables %>% spread(key=cell, val=val) %>% mutate(pca=AC/(AC+`A-C`), pac=AC/(AC+`-AC`))
# df %>% group_by(cn) %>% summarise(mean_pca=mean(pca, na.rm = TRUE), mean_pac=mean(pac, na.rm = TRUE))