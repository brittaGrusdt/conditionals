# conditional perfection --------------------------------------------------

# 1. returns the minimum of the hellinger distances between P(cn) and
# a) P(A->C)=0.25, P(-A->-C)=0.25, P(C->A)=0.25, P(-C->-A)=0.25
# b) P(A->-C)=0.25, P(-A->C)=0.25, P(-C->A)=0.25, P(C->-A)=0.25
# and the direction of minimum
get_cp_values_cns <- function(distr_wide){
  p_cns <- distr_wide %>% dplyr::select(prob, bn_id, cn, level, intention)
  # get marginal probabilities for each cn
  marginal <- p_cns %>% group_by(cn, level, intention) %>%
    summarise(marginal=sum(prob), .groups="keep")
  
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
  
  voi_cns <- marginal %>% group_by(level, intention) %>%
    summarise(cp_cns_ac=hellinger(marginal, marginal_cp1),
              cp_cns_anc=hellinger(marginal, marginal_cp2),
              .groups="keep") %>% 
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
  values <- values %>% group_by(level, intention) %>%
    summarise(cp_bns_ac=sum(prob*hellinger_ac),
              cp_bns_anc=sum(prob*hellinger_anc), .groups="keep")
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

voi_conditional_perfection <- function(posterior, params){
  val_cp <- get_cp_values(posterior) %>% add_model_params(params)
  return(val_cp)
}

# speaker-uncertainty/listener's ignorance inferences  --------------------
addUncertainty2Bns <- function(distr, theta){
  marginalA <- marginalize(distr, c("A")) %>% add_column(marginal="pA")
  marginalC <- marginalize(distr, c("C")) %>% add_column(marginal="pC")
  marginal <- bind_rows(marginalA, marginalC) %>%
    pivot_wider(names_from = marginal, values_from = p) %>% 
    mutate(uncertainty = case_when((pA<theta & pA>1-theta) & (pC<theta & pC>1-theta) ~ "both",
                                   (pA<theta & pA>1-theta) ~ "only A",
                                   (pC<theta & pC>1-theta) ~ "only C",
                                   TRUE ~ "none")
           ) %>% select(-pA, -pC)
  return(marginal)
}

# filter for those states where C is uncertain OR where A is uncertain,
# take EV from union
voi_epistemic_uncertainty <- function(posterior, params){
  bns <- addUncertainty2Bns(posterior, params$theta)
  evs <- bns %>% group_by(uncertainty, level, intention) %>%
    summarise(value=sum(prob), .groups="keep") %>%
    add_column(key="epistemic_uncertainty")
  
  val_no_bias <- add_model_params(evs, params)
  return(val_no_bias)
}

voi_pc <- function(posterior, params){
  val_biscuits <- marginalize(posterior, c("C")) %>% expected_val("C") %>% select(-p) %>%
    rename(value=ev) %>% mutate(key="pc")
  val_biscuits <- add_model_params(val_biscuits, params)
  return(val_biscuits)
}

voi_pa <- function(posterior, params){
  val_pa <- marginalize(posterior, c("A")) %>% expected_val("A") %>% select(-p) %>% 
    rename(value=ev) %>% mutate(key="pa")
  val_pa <- val_pa %>% add_model_params(params)
  return(val_pa)
}

voi_default <- function(posterior, params){
  # @posterior: in long format, must have columns *cell* and *val* 
  certainty <- voi_epistemic_uncertainty(posterior, params)
  pa <- voi_pa(posterior, params)
  pc <- voi_pc(posterior, params)
  cp <- voi_conditional_perfection(posterior, params)
  
  results <- bind_rows(certainty, pa, pc, cp)
  if(params$save){results %>% save_data(paste(str_sub(params$target, 1, -5), "-voi.rds", sep=""))}
  return(results)
}
# Acceptability/Assertability conditions ----------------------------------
# p_rooij: (P(e|i) - P(e|¬i)) / (1-P(e|¬i))
# p_delta: P(e|i) - P(e|¬i)
acceptability_conditions <- function(data_wide){
  df <- data_wide %>% compute_cond_prob("P(C|A)") %>% rename(p_c_given_a=p) %>% 
          compute_cond_prob("P(C|-A)") %>% rename(p_c_given_na=p) %>%
          mutate(p_delta=round(p_c_given_a - p_c_given_na, 5),
                 p_nc_given_na=round(1-p_c_given_na, 5),
                 p_rooij=case_when(p_nc_given_na == 0 ~ round(p_delta/0.00001, 5),
                                   TRUE ~ round(p_delta/p_nc_given_na, 5)),
                 pc=`AC` + `-AC`,
                 p_diff=round(p_c_given_a - pc, 5)) %>%
          select(-p_nc_given_na, -p_c_given_a, -p_c_given_na, -pc)
  return(df)
}

# Douven examples ---------------------------------------------------------
voi_douven <- function(posterior, params, model){
  if(model=="skiing"){voi <- voi_skiing(posterior, params)}
  else if(model=="sundowners"){voi <- voi_sundowners(posterior, params)}
  return(voi)
}

voi_skiing <- function(posterior, params){
  pe <- marginalize(posterior, c("E")) 
  ev_pe <- pe %>% expected_val("E") %>% rename(value=ev, key=p) %>% 
    mutate(alpha=params$alpha, cost=params$cost_conditional, pe=params$prior_pe)
  if(params$save){ev_pe %>% save_data(paste(params$target, "-voi.rds", sep=""))}
  return(ev_pe)
}

voi_sundowners <- function(posterior, params){
  pr <- marginalize(posterior, c("R"))
  ev_pr <- pr %>% expected_val("R") %>% rename(value=ev, key=p)
  
  prs <- marginalize(posterior, c("R", "S"))
  ev_prs <- prs %>% expected_val("R and S") %>% rename(value=ev, key=p)
  
  vois <- bind_rows(ev_prs, ev_pr)  %>% 
    mutate(alpha=params$alpha, cost=params$cost_conditional, 
           pr1=params$prior_pr[1],
           pr2=params$prior_pr[2],
           pr3=params$prior_pr[3]) %>% nest(pr1,pr2,pr3, .key = "prior_pr")
  if(params$save){vois %>% save_data(paste(params$target, "-voi.rds", sep=""))}
  return(vois)
}