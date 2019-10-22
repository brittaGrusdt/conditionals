library(tidyverse)
library(ggplot2)
source("R/helper-functions.R")
source("R/plot-functions.R")

RESULT_DIR <- "data"
TARGET_DIR <- file.path(RESULT_DIR, "default-model", "figs", fsep=.Platform$file.sep)
dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)

params <- list()
params$theta=0.9
params$bias="none"
params$alpha=3
params$cost_conditional=0

# Values-of-interest ------------------------------------------------------
# 1. speaker's uncertainty 
voi_none_all <- readRDS(file.path("data", "default-model", "results-none-voi.rds"))
voi_none <- voi_none_all %>% filter(startsWith(key, "epistemic_uncertainty")) %>% filter_by_model_params(params)

p <- voi_none %>% 
  mutate(value=round(as.numeric(value), 2)) %>% 
  ggplot() + 
  geom_bar(mapping = aes(x=level, y=value, fill=level),
           stat="identity")  +
  geom_text( aes( label = value, x = level,  y = value ),
             hjust = -0.1, size = 8) + 
  
  # facet_grid(intention~key,
  facet_wrap(~key,
             labeller = labeller(key = c(`epistemic_uncertainty_A` = "Antecedent",
                                         `epistemic_uncertainty_C` = "Consequent"))) +
  scale_x_discrete(limits = c("PL", "LL", "prior"), labels = c("Pragmatic interpretation",
                                                               "Literal interpretation",
                                                               "Belief before hearing 'If A, C'"),
                   position = "top") +
  scale_y_continuous(limits=c(0, 0.4)) +
  coord_flip() +
  theme(axis.text.x = element_text(size= 25),
        axis.text.y = element_text(size= 25),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25),
        strip.text = element_text(size = 25),
        legend.position = "none", legend.title = element_blank(), legend.direction = "horizontal") +
  # labs(title=paste(strwrap("Degree of belief about consequent C to be true or false", width=25), collapse="\n")) +
  # labs(y=paste("Expected degree of belief about", t, "to be true or false"), x="") +
  labs(y=TeX("$F_X(1-\\theta) + 1-F_X(\\theta)$"), x="") 
# theme_classic(base_size=20) +
p
ggsave(paste(TARGET_DIR, "none-voi-epistemic-uncertainty.png", sep=.Platform$file.sep), p, width=14.5, height=3.8)

# 2. CP-reading
voi_lawn_all <- readRDS("data/default-model/results-lawn-voi.rds")
voi_none_all <- readRDS("data/default-model/results-none-voi.rds")
params_cp <- params
params_cp$bias <- "lawn"

filter_vois_cp <- function(data, params){
  # data %>% filter(startsWith(key, "cp_bns") | startsWith(key, "cp_cns") | key=="p_nc_given_na") %>% 
  data %>% filter(key=="cp_bns_ac" | key=="cp_cns_ac" | key=="p_nc_given_na") %>% 
    filter_by_model_params(params) %>% dplyr::select(level, value, key, bias)
}

voi_lawn_none <- filter_vois_cp(voi_none_all, params)
voi_lawn_cp <- filter_vois_cp(voi_lawn_all, params_cp)
data <- bind_rows(voi_lawn_none, voi_lawn_cp) %>% ungroup() %>% 
          mutate(level=as.factor(level),
                 bias=factor(bias, levels=c("none", "lawn")),
                 value=round(as.numeric(value),2))
p <- data %>%
  # filter(key=="cp_bns_ac" | key=="p_nc_given_na") %>%
  filter(key=="p_nc_given_na") %>%
  mutate(value=as.numeric(value)) %>%
  ggplot() + 
  geom_bar(aes(x=level, y=value, fill=level), stat="identity", position="dodge") + 
  geom_text( aes( label = value, x = level,  y = value ),
             hjust = 1, size = 6) +
  # facet_wrap(~key, labeller = labeller(
  #   key = c(`cp_bns_ac` = paste(strwrap(
  #                           "expected hellinger distance btw. perfectly
  #                           biconditional distribution and joint probability
  #                           tables", width=50), collapse="\n"),
  #           `p_nc_given_na`= paste(strwrap(
  #                             "belief in consequent to be false given
  #                             antecedent is false", width=40), collapse="\n"))
  # )) +
  facet_wrap(~bias, labeller = labeller(
    bias=c(none = "Default context", lawn="Context: P(¬A,C)<0.25")
  )) + 
  labs(x="", y="Expected degree of belief in ¬C given ¬A") +
  scale_x_discrete(limits = c("PL", "LL", "prior"), 
                   labels=c("Pragmatic interpretation",
                            "Literal interpretation",
                            "Belief before hearing 'If A, C'"
                   ), 
                   position = "top") +
  coord_flip() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size= 25),
        strip.text.x = element_text(size = 20),
        legend.position = "none", legend.title = element_blank())
p
ggsave(paste(TARGET_DIR, "comparison-vois-cp.png", sep=.Platform$file.sep), p, width=15.5, height=3.8)

p <- plot_cp_vois(data %>% filter(bias=="none"))
fn <- paste(TARGET_DIR, "none-vois-cp.png", sep=.Platform$file.sep)
ggsave(fn, p, width=15, height=6)

# 3. marginal distribution over causal nets
# no bias
data <- read_rds(file.path(RESULT_DIR, "default-model", "results-none.rds"))
data_wide <- data %>% spread(key=cell, val=val)# %>% adapt_bn_ids()
p <- plot_cns_default(data_wide)
p    
ggsave(paste(TARGET_DIR, "none-cns.png", sep=.Platform$file.sep), p, width=7, height=3)

# cp-bias
data <- read_rds(file.path(RESULT_DIR, "default-model", "results-lawn.rds"))
data_wide <- data %>% spread(key=cell, val=val)# %>% adapt_bn_ids()
p <- plot_cns_default(data_wide)
p    



# Biscuit conditionals ----------------------------------------------------
data <- read_rds(file.path(RESULT_DIR, "default-model", "results-pizza.rds"))
data_voi <- read_rds(file.path(RESULT_DIR, "default-model", "results-pizza-voi.rds"))
data_wide <- data %>% spread(key=cell, val=val)# %>% adapt_bn_ids()

# marginal_intents <- data_wide %>% group_by(level, intention) %>% summarize(p=sum(prob))

pc <- marginalize(data, c("C")) 
ev_pc <-  pc %>% expected_val("C") %>% mutate(level=as.factor(level))

p <- plot_evs(ev_pc) + facet_wrap(~intention, 
                                  labeller = labeller(intention=c(ISA="Indirect speech act",
                                                                  PA= "Plain assertion"))) +
      scale_x_discrete(limits = c("prior", "LL", "PL"), 
                       labels = c("Belief before hearing 'If A, C'",
                                 "Literal interpretation",
                                 "Pragmatic interpretation"),
                       position="top") +
      labs(y="E[P(C)]") +
      theme(axis.text.x = element_text(angle = 0, hjust=-0.0125)) +
      scale_y_continuous(position="left") + coord_flip()


fn <- paste(TARGET_DIR, "pizza-pc.png", sep=.Platform$file.sep)
ggsave(fn, p, width=15, height=4)


