library(tidyverse)
library(ggplot2)
source("R/helper-functions.R")
source("R/plot-functions.R")

# Data --------------------------------------------------------------------
TARGET_DIR <- file.path("data", "default-model", "figs", fsep=.Platform$file.sep)
dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
model_params <- list()
model_params$theta=0.9
model_params$alpha=3
model_params$cost_conditional=0

biscuit_voi <- read_rds(file.path("data", "default-model", "results-pizza-voi.rds"))
none_voi <- read_rds(file.path("data", "default-model", "results-none-voi.rds"))
cp_voi <- read_rds(file.path("data", "default-model", "results-lawn-voi.rds"))
data_voi <- bind_rows(biscuit_voi, none_voi, cp_voi) %>% filter_by_model_params(model_params)

data_biscuits <- read_rds(file.path("data", "default-model", "results-pizza.rds"))
data_default <- read_rds(file.path("data", "default-model", "results-none.rds"))

# Values-of-interest ------------------------------------------------------
# 1. speaker's uncertainty 
voi_none <- data_voi %>% filter(startsWith(key, "epistemic_uncertainty") & bias=="none") 

p <- voi_none %>% mutate(value=round(as.numeric(value), 2)) %>% 
  ggplot() + 
  geom_bar(mapping = aes(x=level, y=value, fill=level), stat="identity")  +
  geom_text(aes( label = value, x = level,  y = value ), hjust = -0.1, size = 8) + 
  # facet_grid(intention~key,
  facet_wrap(~key,
             labeller = labeller(key = c(`epistemic_uncertainty_A` = "Antecedent",
                                         `epistemic_uncertainty_C` = "Consequent"))) +
  scale_x_discrete(limits = c("PL", "LL", "prior"),
                   labels = c("Pragmatic interpretation",
                              "Literal interpretation",
                              "Belief before hearing 'If A, C'"),
                   position = "top") +
  scale_y_continuous(limits=c(0, 0.4)) + coord_flip() +
  theme(axis.text.x = element_text(size= 25),
        axis.text.y = element_text(size= 25),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25),
        strip.text = element_text(size = 25),
        legend.position = "none") +
  labs(y=TeX("$F_X(1-\\theta) + 1-F_X(\\theta)$"), x="") 
p
ggsave(paste(TARGET_DIR, "none-voi-epistemic-uncertainty.png",
             sep=.Platform$file.sep), p, width=14.5, height=3.8)

# 2. CP-reading
voi_lawn_none <- data_voi %>% filter(key=="cp_bns_ac" | key=="cp_cns_ac" | key=="p_nc_given_na") %>% 
                  filter(bias=="none" | bias=="lawn") %>% dplyr::select(level, value, key, bias) %>%
                  ungroup() %>% mutate(level=as.factor(level),
                                       bias=factor(bias, levels=c("none", "lawn")),
                                       value=round(as.numeric(value),2)) %>% rename(ev=value)

p <- voi_lawn_none %>% filter(key=="p_nc_given_na") %>% plot_evs()
p <- p +
  facet_wrap(~bias, labeller = labeller(bias=c(none = "Default context", lawn="Context: P(¬A,C)<0.25"))) +
  geom_text( aes( label = ev, x = level,  y = ev ), hjust = 1, size = 6) +
  labs(x="", y="Expected degree of belief in P(¬C|¬A)") +
  scale_x_discrete(limits = c("PL", "LL", "prior"), 
                   labels=c("Pragmatic interpretation",
                            "Literal interpretation",
                            "Belief before hearing 'If A, C'"
                   ), 
                   position = "top") +
  coord_flip() + 
  theme(axis.text.x = element_text(angle = 0, hjust = -0.0125),
        strip.text.x = element_text(size = 20))
p
ggsave(paste(TARGET_DIR, "comparison-vois-cp.png", sep=.Platform$file.sep), p, width=15.5, height=3.8)

####
p <- plot_cp_vois(data %>% filter(bias=="none"))
fn <- paste(TARGET_DIR, "none-vois-cp.png", sep=.Platform$file.sep)
ggsave(fn, p, width=15, height=6)
####

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
# 1. Speaker intents
pc <- marginalize(data_biscuits, c("C")) 
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

# 2. Speaker's degree of belief in consequent and marginal speaker_intents
evs_pc <- data_voi %>% filter((key=="pc") & (bias=="pizza" | bias=="none")) %>%
          mutate(value=round(as.numeric(value), 2))

data_wide_bc <- data_biscuits %>% spread(key=cell, val=val)# %>% adapt_bn_ids()
marginal_intents_bc <- data_wide_bc %>% group_by(level, intention, bias) %>%
                      summarize(p=sum(prob))

# data_wide_default <- data_default %>% spread(key=cell, val=val)
# marginal_intents_default <- data_wide_default %>% group_by(level, intention, bias) %>%
#                               summarize(p=sum(prob))

# todo!


# ....
p <-  data_biscuits %>% ggplot(aes(x=level, y=value, fill=intention)) + 
      geom_bar(stat="identity", position=position_dodge()) +
      geom_text(aes( label = value, x = level,  y = value ),
                position = position_dodge(0.9), size=3.5, vjust=-0.1) +
      facet_wrap(~key, labeller=labeller(key=c(`pa`="Expected value of antecedent",
                                               `pc`="Expected value of consequent"))) +
      scale_x_discrete(limits = c("PL", "LL", "prior"),
                       labels = c(paste(strwrap("Pragmatic interpretation", width=15),
                                        collapse="\n"),
                                  paste(strwrap("Literal interpretation", width=15),
                                        collapse="\n"),
                                  "Prior belief"),
                       position="bottom") +
      labs(y=TeX(""), x="") +
      theme(axis.ticks.y = element_blank(), 
            axis.text.y = element_text(size= 15),
            axis.title.x = element_text(size = 15),
            axis.text.x = element_text(size= 12,  angle=0),
            strip.text = element_text(size = 15),
            legend.position = "bottom"
            #, panel.spacing = unit(1, "lines")
            )  
p
fn <- paste(TARGET_DIR, "pizza-pc-intents.png", sep=.Platform$file.sep)
ggsave(fn, p, width=12, height=5)



# 3. Speaker Average distributions
fn <- "results-pizza-given-C-without-intents-avg-speaker.rds"
speaker_bc <- read_rds(file.path("data", "default-model", fn))
fn <- "results-none-given-C-without-intents-avg-speaker.rds"
speaker_default <- read_rds(file.path("data", "default-model", fn))
speaker <- bind_rows(speaker_default, speaker_bc) 

p <- speaker %>% ggplot() +
      geom_bar(mapping = aes(x=utterance, y=mean_per_intention, fill=bias),
               stat="identity", position="dodge") +
      # facet_wrap(~bias) +
      labs(x="", y="", title="Average speaker") +
      theme(axis.text.x = element_text(angle = 30, hjust = 1, size=15),
            text = element_text(size= 15),
            legend.position = c(0.8, 0.9), legend.direction = "horizontal") +
      scale_fill_discrete(name="Context",
                          breaks=c("none", "pizza"),
                          labels=c("Default", "Biscuit"))

fn <- paste(TARGET_DIR, "speaker-default-biscuits.png", sep=.Platform$file.sep)
ggsave(fn, p, width=12, height=5)







