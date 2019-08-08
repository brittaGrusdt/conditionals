source("R/helpers.R")
library(truncnorm)
library(dplyr)


SEED <- 1234
set.seed(SEED)

CNS_DEP <- c("A implies C", "A implies -C", "-A implies C", "-A implies -C", 
             "C implies A", "C implies -A", "-C implies A", "-C implies -A")

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
    tables_long <- tables %>% gather(`AC`, `A-C`, `-AC`, `-A-C`, key="cell", val="val")
    tables_wide <- tables_long %>% group_by(id) %>% summarize(ps = list(val)) %>% add_column(cn=(!! cn)) %>% 
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
  
  tables_long <- tables %>% gather(`AC`, `A-C`, `-AC`, `-A-C`, key="cell", val="val")
  tables_wide <- tables_long %>% group_by(id) %>% summarize(ps = list(val)) %>% add_column(cn="A || C") %>% 
    mutate(vs=list(c("AC", "A-C", "-AC", "-A-C"))) %>% dplyr::select(-id)
  
  return(tables_wide)
}

create_tables <- function(params, target_path){
  tables_all <- list()
  tables_ind <- create_independent_tables(params)
  tables_dep <- create_dependent_tables(params)
  tables <- bind_rows(tables_ind, tables_dep) %>% rowid_to_column("id") %>% 
              mutate(nor_theta=params$nor_theta, nor_beta=params$nor_beta,
                     param_nor_theta=params$param_nor_theta,
                     param_nor_beta=params$param_nor_beta,
                     indep_sigma=params$indep_sigma,
                     n_tables=params$n_tables,
                     seed=SEED
              )
  tables_new <- tables
  if(file.exists(target_path)){
    tables_old <- readRDS(target_path)
    tables_new <- bind_rows(tables_old, tables)
  }
  tables_new %>% save(target_path)
  return(tables)
}


filter_tables <- function(tables, params){
  if(is.na(params$nor_beta)){
    df <- tables %>% filter(is.na(nor_beta) & is.na(nor_theta) &
                                  param_nor_beta==params$param_nor_beta &
                                  param_nor_theta==params$param_nor_theta)
  } else{
    df <- tables %>% filter(nor_beta == params$nor_beta &
                            nor_theta == params$nor_theta)
  }
  return(df)
}

unnest_tables <- function(tables){
  tables_long <- tables %>% unnest() %>% rename(cell=vs, val=ps)
  return(tables_long)
}