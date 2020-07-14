library(truncnorm)
library(dplyr)

SEED <- 1234
set.seed(SEED)
# BASE_DIR <- "/net/store/cogmod/users/brgrusdt/MA-project/conditionals/
BASE_DIR <- "/home/britta/UNI/Osnabrueck/MA-project/conditionals/"
cns_path <- file.path(paste(BASE_DIR, "data/default-model/cns-default.rds", sep=""), fsep = .Platform$file.sep)
cns <- readRDS(cns_path)
CNS_DEP <- cns[cns != "A || C"]

# Table Generation --------------------------------------------------------
create_dependent_tables <- function(params){
  all_tables <- list()
  idx <- 1
  for(cn in CNS_DEP){
    theta <- rbeta(params$n_tables, 10, 1)
    beta <- rbeta(params$n_tables, 1, 10)
    
    p_child_parent <- theta + beta * (1 - theta)
    p_child_neg_parent <- beta
    p_parent <- runif(params$n_tables)
    
    if(cn == "A implies C" || cn == "C implies A"){
      probs <- tibble(cond1=p_child_parent, cond2=p_child_neg_parent, marginal=p_parent)
    } else if(cn=="A implies -C" || cn=="C implies -A"){
      probs <- tibble(cond1=1-p_child_parent, cond2=1-p_child_neg_parent, marginal=p_parent)
    } else if(cn=="-A implies C" || cn=="-C implies A"){
      probs <- tibble(cond1=p_child_neg_parent, cond2=p_child_parent, marginal=1-p_parent)
    } else if(cn=="-A implies -C" || cn=="-C implies -A"){
      probs <- tibble(cond1=1-p_child_neg_parent, cond2=1-p_child_parent, marginal=1-p_parent)
    }
    
    # A -> C and -A -> C use the same probabilities (P(C|A), P(C|-A), P(A)/P(-A))
    if(startsWith(cn, "A") || startsWith(cn, "-A")){
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
    
    tables <- probs %>% dplyr::select(-cond1, -cond2, -marginal) %>% rowid_to_column("id") 
    tables_long <- tables %>% gather(`AC`, `A-C`, `-AC`, `-A-C`, key="cell", val="val") %>% 
                    mutate(val=round(val, 4))
    tables_wide <- tables_long %>% group_by(id) %>% summarise(ps = list(val)) %>% add_column(cn=(!! cn)) %>% 
      mutate(vs=list(c("AC", "A-C", "-AC", "-A-C"))) %>% dplyr::select(-id)
    
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
  tables_wide <- tables_long %>% group_by(id) %>% summarise(ps = list(val)) %>% add_column(cn="A || C") %>% 
    mutate(vs=list(c("AC", "A-C", "-AC", "-A-C"))) %>% dplyr::select(-id)
  
  return(tables_wide)
}

create_tables <- function(params, target_path){
  tables_all <- list()
  if(params$bias == "dutchman" || params$bias == "pizza"){
    tables_ind <- create_independent_tables(params)
    tables_dep <- tibble()
  } else {
      tables_ind <- create_independent_tables(params)
      tables_dep <- create_dependent_tables(params)
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
      facet_wrap(~cell, ncol = 2, scales = "free_y",
                 labeller = labeller(cell = c(`AC` = "A ∧ C", `A-C` = "A ∧ ¬C",
                                              `-AC`= "¬A ∧ C", `-A-C` = "¬A ∧ ¬C")
                                     )
                 ) +
      labs(title = cn_title, x="probability") +
      # theme_minimal(base_size = 20) +
      theme_classic(base_size = 20) +
      # theme_void(base_size = 20) +
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