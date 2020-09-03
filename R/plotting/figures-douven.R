library(tidyverse)
library(ggplot2)
library(rwebppl)
source("R/helper-functions.R")
source("R/helpers-webppl.R")
source("R/helper-functions.R")
# source("R/plot-functions.R")

# Run Model ---------------------------------------------------------------
run_douven_case <- function(params){
  data <- run_webppl(params$model_path, params) %>%
    structure_listener_data(params) %>% select(-bias) %>%
    group_by(cn, cell, level) %>% summarise(ev=sum(prob*val))
  return(data)
}
listener_beliefs <- function(data, params){
  data <- data %>% filter(level==params$level_max) %>%
    group_by(cn, cell) 
  if(!is.na(params$condition_on)){
    data <- data %>% filter_vars(params$condition_on) %>%
      summarise(ev=sum(ev), .groups="drop_last") %>%
      mutate(ev=ev/sum(ev)) # marginalize
  }
  else {data = data %>% summarise(ev=sum(ev), .groups="drop_last")}
      
  return(data %>% add_column(level="trust"))
}


# 1. Skiing ---------------------------------------------------------------
params.skiing <- configure(c("skiing"))
data.skiing <- run_douven_case(params.skiing)
skiing <- bind_rows(data.skiing, listener_beliefs(data.skiing, params.skiing)) 
skiing.marginal = skiing %>% filter_vars(c("E")) %>%
  group_by(cn, level) %>% summarise(ev=sum(ev)) %>% 
  add_column(marginal="e", example="skiing")

# 2. Sundowners -----------------------------------------------------------
params.sundowners <- configure(c("sundowners"))
data.sundowners <- run_douven_case(params.sundowners)
sundowners <- bind_rows(data.sundowners, listener_beliefs(data.sundowners,
                                                          params.sundowners)) 

sundowners.rain = sundowners %>% filter_vars(c("R")) %>%
  group_by(cn, level) %>% summarise(ev=sum(ev)) %>% add_column(marginal="r")

sundowners.rain_sundowner = sundowners %>% filter_vars(c("R", "S")) %>%
  group_by(cn, level) %>% summarise(ev=sum(ev)) %>%
  add_column(marginal="rs")

sundowners.data <- bind_rows(sundowners.rain, sundowners.rain_sundowner) %>%
  add_column(example="sundowners")

douven.data <- bind_rows(sundowners.data, skiing.marginal)

douven_plot <- function(data){
  p <- data %>% mutate(level=as.factor(level)) %>%
    ggplot() +
    geom_bar(mapping = aes(y=level, x=ev, fill=cn), stat="identity", position="stack") +
    facet_wrap(~example) + 
    facet_wrap(~marginal, labeller=
                 as_labeller(c(`r`="Sundowners: P(R)", `rs`="Sundowners: P(R,S)",
                               `e`="Skiing: P(E)"))) +
    scale_y_discrete(
      name = "",
      limits = c("trust", "PL", "LL", "prior"),
      labels = c(
        paste(strwrap("Listener's beliefs", width=20), collapse="\n"),
        paste(strwrap("Pragmatic interpretation", width=20), collapse="\n"),
        paste(strwrap("Literal interpretation", width=20), collapse="\n"),
        "Prior Belief"
      )) +
    scale_fill_discrete(name="causal net",
                        limits=c("R > W > S", "R||S", "E || S>C", "E>S>C"),
                        labels=c("R->W->S", "R,S indep.", "E indep. S, S->C", "E->S->C")) +
    labs(x="Expected value", title="") +
    theme_bw(base_size=25) + theme(legend.position="bottom")
  
  return(p)
}

p <- douven.data %>% douven_plot()
ggsave(paste(PLOT_DIR, "douven-cases.png", sep=SEP), p, width=15, height=6)

  

